library(foreign)
library(tidyverse)
library(sf)
library(dplyr)
library(purrr)
library(jpmesh)
#Sys.setlocale("LC_ALL", "Japanese_Japan.932")
library(cowplot)
library(lubridate)
library(raster)
library(gdistance)
library(viridis)
library(secr)
library(R6)
library(numDeriv)

source("functions.R", encoding = "UTF-8")
sourcepath<-"../../ADCR/adcrtest2/secrad.r"
source(sourcepath, encoding = "UTF-8")

# # テストデータ作成コード ( 列=個体, 行=調査) -----------------------------------------
# 1. グリッドと座標の定義
n_side <- 5
n_cell <- n_side * n_side
coords <- expand.grid(x = 1:n_side, y = 1:n_side) 

dataset_test <- list()
dataset_test$coords <- coords 
dataset_test$area <- rep(1, n_cell)        
dataset_test$resolution <- c(x = 1, y = 1) 

# 2. 環境要因
set.seed(123)
dataset_test$grid_cov_std <- data.frame(
  agri = rnorm(n_cell, 0, 1),
  wtr = rnorm(n_cell, 0, 1)
)

# 3. 調査努力量 (Effort)
n_occ <- 3
n_survey_per_occ <- 5
total_surveys <- n_occ * n_survey_per_occ

dataset_test$effort_occ <- rep(1:n_occ, each = n_survey_per_occ)
dataset_test$effort_loc <- sample(1:n_cell, total_surveys, replace = TRUE)
dataset_test$effort <- rep(1, total_surveys)

# 4. 捕獲データ (Detection Matrix) の作成
n_multi <- 5   
n_single <- 15 
n_ind <- n_multi + n_single

# 行数=調査数(15), 列数=個体数(20) で初期化
detect_mat <- matrix(0, nrow = total_surveys, ncol = n_ind)

#複数回捕獲個体 (列 1~5)
for(col_i in 1:n_multi) {
  # ランダムに2〜3箇所の調査(行)を選ぶ
  rows <- sample(1:total_surveys, size = sample(2:3, 1))
  detect_mat[rows, col_i] <- 1
}

#1回捕獲個体 (列 6~20)
for(col_i in (n_multi + 1):n_ind) {
  row <- sample(1:total_surveys, size = 1)
  detect_mat[row, col_i] <- 1
}

dataset_test$detect <- detect_mat
dataset_test$nind <- n_ind
dataset_test$ncell <- n_cell
dataset_test$nocc <- n_occ
dataset_test$ind_cov <- sample(c(0, 1), n_ind, replace = TRUE)

#  secrdata 
secrdata_test <- secrad_data$new(
  coords = dataset_test$coords,
  area = dataset_test$area,
  grid_cov = dataset_test$grid_cov_std,
  resolution = dataset_test$resolution
)

# 観測情報
secrdata_test$add_obs(
  type = "poisson", 
  effort = dataset_test$effort,
  effort_loc = dataset_test$effort_loc,
  effort_occ = dataset_test$effort_occ,
  detect = dataset_test$detect # ここで (15行 x 20列) の行列が入る
)

# 個体属性
secrdata_test$ind_cov <- dataset_test$ind_cov

# 
# cat("Detect Matrix Dim:", dim(dataset_test$detect), "\n") 
# cat("Object nind:", secrdata_test$nind, "\n")             

# plot
try({
  secrdata_test$ggsecraddata(covname="agri")
})


# 通常ADCRモデル -------------------------------------------------------------------
secrad_obj<-secrad$new(secrdata=secrdata_test)
envmodel<-list(D~1,C~agri+wtr,A~0)
indmodel<-c(A=FALSE,g0=FALSE)
occmodel<-c(A=FALSE,g0=FALSE)
secrad_obj$set_model(envmodel=envmodel,indmodel=indmodel,occmodel=occmodel)

initpar<-generate_init(secrad_obj)
initpar["dens_0"]<--1
initpar["conn_0"]<--2
initpar["g0_1"]<--5
secrad_res<-optim(initpar,secrad_obj$loglf,method="BFGS",control=list(maxit=1000,trace=2),loglfscale=-1,verbose=T,hessian=T)

# wrapper関数 ---------------------------------------------------------------
# 指定IDのデータのみを持つsecradオブジェクトを作成する
create_subset_secrad <- function(original_secrdata, ids, model_settings) {
  orig_obs <- original_secrdata$obs[[1]]
  
  if(!is.null(orig_obs$detect)) {
    subset_detect <- as.matrix(orig_obs$detect[, ids, drop=FALSE])
  } else { 
    subset_detect <- NULL 
  }
  
  new_secrdata <- secrad_data$new(
    coords = original_secrdata$coords, area = original_secrdata$area,
    grid_cov = original_secrdata$grid_cov, resolution = original_secrdata$resolution
  )
  
  new_secrdata$add_obs(
    type = orig_obs$type, effort = orig_obs$effort,
    effort_loc = orig_obs$effort_loc, effort_occ = orig_obs$effort_occ,
    detect = subset_detect
  )
  
  if(!is.null(original_secrdata$ind_cov)) {
    new_secrdata$ind_cov <- original_secrdata$ind_cov[ids]
  } else { 
    new_secrdata$ind_cov <- rep(1, length(ids)) 
  }
  
  new_obj <- secrad$new(secrdata = new_secrdata)
  new_obj$set_model(envmodel = model_settings$envmodel, indmodel = model_settings$indmodel, occmodel = model_settings$occmodel)
  
  return(new_obj)
}


# 2. SGD用ラッパー尤度関数
## SGD用：loglf(sgd=TRUE) のパーツを使う目的関数
wrapper_sgd_loglf_fast <- function(par,
                                   obj_multi,
                                   obj_single,
                                   n_detected,
                                   sampling_rate,
                                   verbose = FALSE) {
  
  out_multi <- obj_multi$loglf(
    par,
    loglfscale = 1,
    sgd = TRUE
  )
  
  out_single <- obj_single$loglf(
    par,
    loglfscale = 1,
    sgd = TRUE
  )
  
  ll_pois <- dpois(
    x = n_detected,
    lambda = exp(out_multi$lambda_grp),
    log = TRUE
  )
  
  ll_ch <- out_multi$loglfmulti +
    out_single$loglfmulti / sampling_rate
  
  res <- sum(ll_pois) + ll_ch
  
  if (verbose) {
    cat("[SGD loglf parts]\n")
    cat("ll_pois       :", sum(ll_pois), "\n")
    cat("ll_multi      :", out_multi$loglfmulti, "\n")
    cat("ll_single/r   :", out_single$loglfmulti / sampling_rate, "\n")
    cat("total         :", res, "\n")
  }
  
  return(res)
}


# SGDパラメータ設定 --------------------------------------------------------------

# 捕獲数
## SGDパラメータ設定 --------------------------------------------------------------

capture_counts <- colSums(dataset_test$detect)
multi_ids <- which(capture_counts > 1)
single_ids <- which(capture_counts == 1)

current_model_settings <- list(
  envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
  indmodel = c(A = FALSE, g0 = FALSE),
  occmodel = c(A = FALSE, g0 = FALSE)
)

sampling_rate <- 1.0
sample_size <- max(1, floor(length(single_ids) * sampling_rate))

n_detected <- secrdata_test$nind

## 初期値
initpar_test <- generate_init(secrad_obj)
initpar_test["dens_0"]<--1
initpar_test["conn_0"]<--2
initpar_test["g0_1"]<--5

current_par <- initpar_test

# current_par["dens_0"] <- secrad_res$par["dens_0"]
# current_par["conn_0"] <- secrad_res$par["conn_0"]
# current_par["conn_agri"] <- secrad_res$par["conn_agri"]
# current_par["conn_wtr"] <- secrad_res$par["conn_wtr"]
# current_par["g0_1"] <- secrad_res$par["g0_1"]

## 学習設定
learning_rate <- 0.01
max_iter <- 5

trace_par <- matrix(NA, nrow = max_iter, ncol = length(current_par))
colnames(trace_par) <- names(current_par)

trace_ll <- rep(NA, max_iter)

obj_multi_fixed <- create_subset_secrad(
  original_secrdata = secrdata_test,
  ids = multi_ids,
  model_settings = current_model_settings
)

n_detected <- secrdata_test$nind

# cat("--- SGD Optimization using prebuilt objects Started ---\n")
# 
# system.time(
#   for (iter in 1:max_iter) {
#     
#     ## 単回捕獲個体のサンプリング
#     if (length(single_ids) > sample_size) {
#       current_single_sample <- sample(single_ids, size = sample_size)
#     } else {
#       current_single_sample <- single_ids
#     }
#     
#     ## この iteration で使う単回個体オブジェクトを1回だけ作る
#     obj_single_iter <- create_subset_secrad(
#       original_secrdata = secrdata_test,
#       ids = current_single_sample,
#       model_settings = current_model_settings
#     )
#     
#     ## この iteration 内で固定する目的関数
#     objfun_iter <- function(p) {
#       names(p) <- names(current_par)
#       
#       wrapper_sgd_loglf_fast(
#         par = p,
#         obj_multi = obj_multi_fixed,
#         obj_single = obj_single_iter,
#         n_detected = n_detected,
#         sampling_rate = sampling_rate,
#         verbose = FALSE
#       )
#     }
#     
#     ## 勾配計算
#     g <- tryCatch({
#       numDeriv::grad(
#         func = objfun_iter,
#         x = current_par
#       )
#     }, error = function(e) {
#       cat(sprintf("\n[Error] Iter %d で勾配計算エラー: %s\n", iter, e$message))
#       return(rep(NA, length(current_par)))
#     })
#     
#     if (any(is.na(g)) || any(!is.finite(g))) {
#       cat("勾配が NA または Inf になったため、SGDを停止します。\n")
#       break
#     }
#     
#     ## 対数尤度を最大化するので + 方向に更新
#     current_lr <- learning_rate / (1 + 0.01 * iter)
#     current_par <- current_par + current_lr * g
#     
#     trace_par[iter, ] <- current_par
#     
#     ## モニタリング
#     if (iter %% 50 == 0 || iter == 1) {
#       curr_ll <- objfun_iter(current_par)
#       trace_ll[iter] <- curr_ll
#       
#       cat(sprintf(
#         "Iter: %3d, LR: %.5f, SGD objective: %.6f\n",
#         iter, current_lr, curr_ll
#       ))
#     }
#   }
# )
# 
# cat("--- SGD Completed ---\n")



#  Adam-SGD  --------------------------------------------------------------
# Adam-SGD 用の準備
capture_counts <- colSums(dataset_test$detect)
multi_ids <- which(capture_counts > 1)
single_ids <- which(capture_counts == 1)

current_model_settings <- list(
  envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
  indmodel = c(A = FALSE, g0 = FALSE),
  occmodel = c(A = FALSE, g0 = FALSE)
)

sampling_rate <- 0.2
sample_size <- max(1, floor(length(single_ids) * sampling_rate))

is_full_sampling <- isTRUE(all.equal(sampling_rate, 1.0))

n_detected <- secrdata_test$nind

# 固定できる secrad オブジェクトを作る
obj_multi_fixed <- create_subset_secrad(
  original_secrdata = secrdata_test,
  ids = multi_ids,
  model_settings = current_model_settings
)

if (is_full_sampling) {
  obj_single_fixed <- create_subset_secrad(
    original_secrdata = secrdata_test,
    ids = single_ids,
    model_settings = current_model_settings
  )
} else {
  obj_single_fixed <- NULL
}

make_single_obj <- function(ids) {
  create_subset_secrad(
    original_secrdata = secrdata_test,
    ids = ids,
    model_settings = current_model_settings
  )
}

get_single_obj <- function(ids) {
  if (is_full_sampling) {
    return(obj_single_fixed)
  } else {
    return(make_single_obj(ids))
  }
}

# 初期値と記録用オブジェクト
initpar_test <- generate_init(secrad_obj)

initpar_test["dens_0"] <- -1
initpar_test["conn_0"] <- -2
initpar_test["g0_1"] <- -5

current_par <- initpar_test

max_iter <- 2000

trace_par <- matrix(NA, nrow = max_iter, ncol = length(current_par))
colnames(trace_par) <- names(current_par)

trace_ll <- rep(NA, max_iter)

trace_grad <- matrix(NA, nrow = max_iter, ncol = length(current_par))
colnames(trace_grad) <- names(current_par)

trace_step <- matrix(NA, nrow = max_iter, ncol = length(current_par))
colnames(trace_step) <- names(current_par)

# Adam 設定
alpha <- 0.01
beta1 <- 0.9
beta2 <- 0.999
eps_adam <- 1e-8
alpha_vec <- rep(0.05, length(current_par))
names(alpha_vec) <- names(current_par)

alpha_vec["dens_0"] <- 0.03
alpha_vec["conn_0"] <- 0.05
alpha_vec["conn_agri"] <- 0.05
alpha_vec["conn_wtr"] <- 0.05
alpha_vec["g0_1"] <- 0.05

m <- rep(0, length(current_par))
v <- rep(0, length(current_par))
names(m) <- names(current_par)
names(v) <- names(current_par)

max_step <- rep(0.2, length(current_par))
names(max_step) <- names(current_par)

max_step["dens_0"] <- 0.05
max_step["conn_0"] <- 0.10
max_step["conn_agri"] <- 0.05
max_step["conn_wtr"] <- 0.05
max_step["g0_1"] <- 0.05

grad_method <- "simple"
grad_eps <- 1e-4

# 初期目的関数チェック
if (is_full_sampling) {
  current_single_sample <- single_ids
} else {
  current_single_sample <- sample(single_ids, size = sample_size)
}

obj_single_check <- get_single_obj(current_single_sample)

initial_objfun <- function(p) {
  names(p) <- names(current_par)
  
  wrapper_sgd_loglf_fast(
    par = p,
    obj_multi = obj_multi_fixed,
    obj_single = obj_single_check,
    n_detected = n_detected,
    sampling_rate = sampling_rate,
    verbose = FALSE
  )
}

initial_ll <- initial_objfun(current_par)

cat("\n[Initial check]\n")
cat("initial objective:", initial_ll, "\n")

if (is.na(initial_ll) || !is.finite(initial_ll)) {
  stop("初期値で目的関数が NA または Inf です。")
}

# Adam-SGD ループ
cat("--- Adam-SGD Optimization using sgd=TRUE parts Started ---\n")

time_adam_sgd <- system.time(
  for (iter in 1:max_iter) {
    
    # 単回捕獲個体のサンプリング
    if (is_full_sampling) {
      current_single_sample <- single_ids
    } else {
      if (length(single_ids) > sample_size) {
        current_single_sample <- sample(single_ids, size = sample_size)
      } else {
        current_single_sample <- single_ids
      }
    }
    
    obj_single_iter <- get_single_obj(current_single_sample)
    
    # この iteration 内で固定する目的関数
    # ここが重要：numDeriv::grad() にはこの objfun_iter を渡す
    objfun_iter <- function(p) {
      names(p) <- names(current_par)
      
      wrapper_sgd_loglf_fast(
        par = p,
        obj_multi = obj_multi_fixed,
        obj_single = obj_single_iter,
        n_detected = n_detected,
        sampling_rate = sampling_rate,
        verbose = FALSE
      )
    }
    
    # 勾配計算
    g <- tryCatch({
      numDeriv::grad(
        func = objfun_iter,
        x = current_par,
        method = grad_method,
        method.args = list(eps = grad_eps)
      )
    }, error = function(e) {
      cat(sprintf("\n[Error] Iter %d で勾配計算エラー: %s\n", iter, e$message))
      return(rep(NA, length(current_par)))
    })
    
    names(g) <- names(current_par)
    
    if (any(is.na(g)) || any(!is.finite(g))) {
      cat("勾配が NA または Inf になったため、Adam-SGDを停止します。\n")
      break
    }
    
    # Adam更新
    m <- beta1 * m + (1 - beta1) * g
    v <- beta2 * v + (1 - beta2) * (g^2)
    
    m_hat <- m / (1 - beta1^iter)
    v_hat <- v / (1 - beta2^iter)
    
    step <- alpha_vec * m_hat / (sqrt(v_hat) + eps_adam)
    
    # ステップ幅クリッピング
    step <- pmax(pmin(step, max_step), -max_step)
    
    # 対数尤度を最大化するので + 方向
    current_par <- current_par + step
    
    trace_par[iter, ] <- current_par
    trace_grad[iter, ] <- g
    trace_step[iter, ] <- step
    
    curr_ll <- objfun_iter(current_par)
    trace_ll[iter] <- curr_ll
    
    #if (iter %% 10 == 0 || iter == 1) {
      cat(sprintf(
      "Iter: %4d, objective: %.8f, max|g|: %.4e, max|step|: %.4e\n",
      iter,
      curr_ll,
      max(abs(g)),
      max(abs(step))
    ))
    
    print(round(current_par, 5))
    #}
  }
)

cat("--- Adam-SGD Completed ---\n")
print(time_adam_sgd)
save(
  secrad_res,
  current_par,
  trace_par,
  trace_ll,
  trace_grad,
  trace_step,
  sampling_rate,
  sample_size,
  alpha_vec,
  beta1,
  beta2,
  eps_adam,
  max_step,
  grad_method,
  grad_eps,
  time_adam_sgd,
  file = "SGD_Adam_result_202608171730.Rdata"
)



# 結果の比較 -------------------------------------------------------------------
cat("\n【結果比較】\n")
cat(sprintf("%-15s | %-15s | %-15s\n", "Parameter", "True (optim)", "Est (SGD)"))
cat("--------------------------------------------------\n")

# secrad_res と比較
par_check <- secrad_res$par

ll_full <- secrad_obj$loglf(
  par_check,
  loglfscale = 1
)

ll_sgd_parts <- wrapper_sgd_loglf_fast(
  par = par_check,
  ids_multi = multi_ids,
  ids_single = single_ids,
  sampling_rate = 1.0,
  original_secrdata = secrdata_test,
  model_settings = current_model_settings,
  verbose = TRUE
)

cat("\n[Check]\n")
cat("full loglf       :", ll_full, "\n")
cat("sgd-parts loglf  :", ll_sgd_parts, "\n")
cat("difference       :", ll_full - ll_sgd_parts, "\n")

# secrad_res と比較
for(i in 1:length(current_par)) {
  par_name <- names(current_par)[i]
  true_val <- secrad_res$par[i] # 正解の値
  sgd_val <- current_par[i]   # SGDの値
  
  cat(sprintf("%-15s | %15.4f | %15.4f\n", par_name, true_val, sgd_val))
}

# plot
par(mfrow=c(2,3)) 
for(i in 1:length(current_par)){
  plot(trace_par[,i], type="l", main=names(current_par)[i], 
       xlab="Iter", ylab="Value", col="blue")
  abline(h=secrad_res$par[i], col="red", lty=2, lwd=2) # 正解のライン
}
par(mfrow=c(1,1)) 

# 
# 
# 
# 
# 
# 
# 
# # モデル設定と安全な初期値の生成
# # 1. モデル設定 (optimと合わせるため wtr を追加)
# current_model_settings <- list(
#   envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0), 
#   indmodel = c(A = FALSE, g0 = FALSE),
#   occmodel = c(A = FALSE, g0 = FALSE)
# )
# 
# # 2. グローバルオブジェクトの設定も更新
# secrad_obj_test$set_model(
#   envmodel = current_model_settings$envmodel, 
#   indmodel = current_model_settings$indmodel, 
#   occmodel = current_model_settings$occmodel
# )
# 
# # 3. 【重要】安全な初期値を自動生成する
# current_par <- generate_init(secrad_obj_test)
# 
# cat("--- 生成された安全な初期パラメータ ---\n")
# print(current_par)
# 
# # 念のため、この初期値でNAが出ないか単発テスト
# test_ll <- wrapper_weighted_loglf(
#   current_par, multi_ids, single_ids, weight_single, 
#   secrdata_test, current_model_settings
# )
# cat("\n初期尤度のテスト計算:", test_ll, "(NAでなければOK!)\n")
# 
# 
# 
# # SGDループの実行 (安全装置付き)
# 
# cat("\n--- SGD Optimization (Wrapper Mode) Started ---\n")
# 
# learning_rate <- 0.01 
# max_iter <- 500
# 
# trace_par <- matrix(NA, nrow = max_iter, ncol = length(current_par))
# trace_ll <- numeric(max_iter)
# 
# for(iter in 1:max_iter) {
#   
#   # サンプリング
#   if(length(single_ids) > sample_size) {
#     current_single_sample <- sample(single_ids, size = sample_size)
#   } else {
#     current_single_sample <- single_ids
#   }
#   
#   # 勾配計算 (エラーが起きても止まらないように tryCatch で保護)
#   g <- tryCatch({
#     numDeriv::grad(
#       func = wrapper_weighted_loglf,
#       x = current_par,
#       ids_multi = multi_ids,
#       ids_single = current_single_sample,
#       weight = weight_single,
#       original_secrdata = secrdata_test,
#       model_settings = current_model_settings
#     )
#   }, error = function(e) {
#     cat(sprintf("\n[Error] Iter %d で勾配計算エラー: %s\n", iter, e$message))
#     return(rep(NA, length(current_par)))
#   })
#   
#   # もし勾配がNAになってしまったら、ループを安全に脱出する
#   if(any(is.na(g))) {
#     cat("計算不能なパラメータ領域に入ったため、SGDを早期終了します。\n")
#     break
#   }
#   
#   # パラメータ更新 (モメンタムなしのシンプル版)
#   current_lr <- learning_rate / (1 + 0.01 * iter)
#   current_par <- current_par + g * current_lr
#   
#   # 記録
#   trace_par[iter, ] <- current_par
#   
#   # モニタリング
#   if(iter %% 50 == 0 || iter == 1) {
#     curr_ll <- wrapper_weighted_loglf(
#       current_par, multi_ids, current_single_sample, weight_single, 
#       secrdata_test, current_model_settings
#     )
#     trace_ll[iter] <- curr_ll
#     cat(sprintf("Iter: %3d, LR: %.4f, ApproxLL: %.2f\n", iter, current_lr, curr_ll))
#   }
# }
# 
# cat("--- SGD Completed ---\n")




#BFGS refine: all single-capture individuals ---------------------------
# Adam-SGD の current_par を初期値にして、固定 objective で仕上げる


# 念のためパラメータ名を確認
print(current_par)

# 複数回捕獲個体オブジェクト
# 既に obj_multi_fixed があるなら再作成不要
if (!exists("obj_multi_fixed")) {
  obj_multi_fixed <- create_subset_secrad(
    original_secrdata = secrdata_test,
    ids = multi_ids,
    model_settings = current_model_settings
  )
}

# 単回捕獲個体を全て使う固定オブジェクト
obj_single_all <- create_subset_secrad(
  original_secrdata = secrdata_test,
  ids = single_ids,
  model_settings = current_model_settings
)

# 総検出個体数
n_detected <- secrdata_test$nind

# BFGS 用の負の objective
# optim は最小化なので、SGD parts objective にマイナスを付ける
objfun_refine_neg <- function(p) {
  names(p) <- names(current_par)
  
  -wrapper_sgd_loglf_fast(
    par = p,
    obj_multi = obj_multi_fixed,
    obj_single = obj_single_all,
    n_detected = n_detected,
    sampling_rate = 1.0,
    verbose = FALSE
  )
}

# refine 前の objective
obj_before_refine <- -objfun_refine_neg(current_par)

cat("\n[Before BFGS refine]\n")
cat("objective:", obj_before_refine, "\n")
print(current_par)

# BFGS refine
time_refine <- system.time({
  res_refine_sgdobj <- optim(
    par = current_par,
    fn = objfun_refine_neg,
    method = "BFGS",
    control = list(
      maxit = 500,
      trace = 1,
      REPORT = 1
    ),
    hessian = FALSE
  )
})

cat("\n--- BFGS refine completed ---\n")
print(time_refine)

# refine 後の objective
obj_after_refine <- -res_refine_sgdobj$value

cat("\n[After BFGS refine]\n")
cat("objective:", obj_after_refine, "\n")
print(res_refine_sgdobj$par)

cat("\n[Improvement]\n")
cat("before:", obj_before_refine, "\n")
cat("after :", obj_after_refine, "\n")
cat("gain  :", obj_after_refine - obj_before_refine, "\n")

# 結果を current_par_refined として保存
current_par_refined <- res_refine_sgdobj$par



# optim 解との比較 -------------------------------------------------------------

if (exists("secrad_res")) {
  cat("\n[Comparison with full optim]\n")
  
  comp_refine <- cbind(
    optim_full = secrad_res$par[names(current_par_refined)],
    adam_sgd = current_par[names(current_par_refined)],
    bfgs_refined = current_par_refined,
    diff_refined = current_par_refined - secrad_res$par[names(current_par_refined)]
  )
  
  print(comp_refine)
  
  # objective 比較
  obj_at_optim <- wrapper_sgd_loglf_fast(
    par = secrad_res$par,
    obj_multi = obj_multi_fixed,
    obj_single = obj_single_all,
    n_detected = n_detected,
    sampling_rate = 1.0,
    verbose = FALSE
  )
  
  obj_at_adam <- wrapper_sgd_loglf_fast(
    par = current_par,
    obj_multi = obj_multi_fixed,
    obj_single = obj_single_all,
    n_detected = n_detected,
    sampling_rate = 1.0,
    verbose = FALSE
  )
  
  obj_at_refined <- wrapper_sgd_loglf_fast(
    par = current_par_refined,
    obj_multi = obj_multi_fixed,
    obj_single = obj_single_all,
    n_detected = n_detected,
    sampling_rate = 1.0,
    verbose = FALSE
  )
  
  cat("\n[Objective comparison under SGD-parts objective]\n")
  cat("at full optim par :", obj_at_optim, "\n")
  cat("at Adam-SGD par   :", obj_at_adam, "\n")
  cat("after BFGS refine :", obj_at_refined, "\n")
}
