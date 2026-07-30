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
wrapper_sgd_loglf <- function(par,
                              ids_multi,
                              ids_single,
                              sampling_rate,
                              original_secrdata,
                              model_settings,
                              verbose = FALSE) {
  
  ## 複数回捕獲個体のオブジェクト
  obj_multi <- create_subset_secrad(
    original_secrdata = original_secrdata,
    ids = ids_multi,
    model_settings = model_settings
  )
  
  out_multi <- obj_multi$loglf(
    par,
    loglfscale = 1,
    sgd = TRUE
  )
  
  ## 単回捕獲個体のオブジェクト
  obj_single <- create_subset_secrad(
    original_secrdata = original_secrdata,
    ids = ids_single,
    model_settings = model_settings
  )
  
  out_single <- obj_single$loglf(
    par,
    loglfscale = 1,
    sgd = TRUE
  )
  
  ## 実際に検出された個体数 n
  ## 今回のモデルでは indmodel が FALSE なので scalar でよい
  n_detected <- original_secrdata$nind
  
  ## 少なくとも1回検出された個体数の Poisson process 尤度
  ll_pois <- dpois(
    x = n_detected,
    lambda = exp(out_multi$lambda_grp),
    log = TRUE
  )
  
  ## 捕獲履歴部分
  ## 複数回個体は全使用、単回個体は sampling_rate で補正
  ll_ch <- out_multi$loglfmulti + out_single$loglfmulti / sampling_rate
  
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

## 初期値
current_par <- generate_init(secrad_obj)

current_par["dens_0"] <- secrad_res$par["dens_0"]
current_par["conn_0"] <- secrad_res$par["conn_0"]
current_par["conn_agri"] <- secrad_res$par["conn_agri"]
current_par["conn_wtr"] <- secrad_res$par["conn_wtr"]
current_par["g0_1"] <- secrad_res$par["g0_1"]

## 学習設定
learning_rate <- 0.01
max_iter <- 500

trace_par <- matrix(NA, nrow = max_iter, ncol = length(current_par))
colnames(trace_par) <- names(current_par)

trace_ll <- rep(NA, max_iter)

cat("--- SGD Optimization using sgd=TRUE parts Started ---\n")

system.time(
  for (iter in 1:max_iter) {
    
    ## 単回捕獲個体のサンプリング
    if (length(single_ids) > sample_size) {
      current_single_sample <- sample(single_ids, size = sample_size)
    } else {
      current_single_sample <- single_ids
    }
    
    ## 勾配計算
    g <- tryCatch({
      numDeriv::grad(
        func = wrapper_sgd_loglf,
        x = current_par,
        ids_multi = multi_ids,
        ids_single = current_single_sample,
        sampling_rate = sampling_rate,
        original_secrdata = secrdata_test,
        model_settings = current_model_settings
      )
    }, error = function(e) {
      cat(sprintf("\n[Error] Iter %d で勾配計算エラー: %s\n", iter, e$message))
      return(rep(NA, length(current_par)))
    })
    
    if (any(is.na(g)) || any(!is.finite(g))) {
      cat("勾配が NA または Inf になったため、SGDを停止します。\n")
      break
    }
    
    ## 対数尤度を最大化するので + 方向に更新
    current_lr <- learning_rate / (1 + 0.01 * iter)
    current_par <- current_par + current_lr * g
    
    trace_par[iter, ] <- current_par
    
    ## モニタリング
    if (iter %% 50 == 0 || iter == 1) {
      curr_ll <- wrapper_sgd_loglf(
        par = current_par,
        ids_multi = multi_ids,
        ids_single = current_single_sample,
        sampling_rate = sampling_rate,
        original_secrdata = secrdata_test,
        model_settings = current_model_settings,
        verbose = FALSE
      )
      
      trace_ll[iter] <- curr_ll
      
      cat(sprintf(
        "Iter: %3d, LR: %.5f, SGD objective: %.3f\n",
        iter, current_lr, curr_ll
      ))
    }
  }
)

cat("--- SGD Completed ---\n")
save.image("SGDtest_20260421.Rdata")




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

ll_sgd_parts <- wrapper_sgd_loglf(
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



