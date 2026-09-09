#Fukasawa and Higashide(2025)のクマデータをつかってSGD
library(sf)
library(tidyverse)
library(cowplot)
library(lubridate)
library(raster)
library(gdistance)
library(viridis)
library(secr)

source("functions.R", encoding = "UTF-8")
sourcepath<-"adcrsgd/secrad.r"
source(sourcepath, encoding = "UTF-8")
# advdiff キャッシュの共有と、キャッシュに沿った順序の有限差分。
# 1反復あたりの advdiff.eigen 呼び出しが 10回 -> 4回 になる（tests/cache_bench.R）。
source("adcrsgd/sgd_utils.R", encoding = "UTF-8")

effort<-read_csv("../../ADCR/doi_10_5061_dryad_ksn02v7bq__v20250117/effort_231225.csv")
effort_st<-effort%>%st_as_sf(coords=c("x","y"),crs=3100)

detect<-read_csv("../../ADCR/doi_10_5061_dryad_ksn02v7bq__v20250117/detectmat_231225.csv")


# set data -----------------------------------------------------------
# read griddata 
gridpath<-"../../ADCR/doi_10_5061_dryad_ksn02v7bq__v20250117/"
ngrid<-1
dataset<-list()
dataset$griddata<-st_read(gridpath, "meshutm_0.5km_buff_land")

#dataset
coords<-dataset$griddata[,c("x","y")]/1000	#km
coords<-coords%>%as_tibble%>% dplyr::select(-geometry)
dataset$coords<-coords%>%as.matrix

ncell<-nrow(coords)
ncell<-nrow(coords)
xdist<-abs(outer(coords$x,rep(1,ncell))-outer(rep(1,ncell),coords$x))
ydist<-abs(outer(coords$y,rep(1,ncell))-outer(rep(1,ncell),coords$y))
dx<-min(xdist[xdist!=0])
dy<-min(ydist[ydist!=0])

area<-dataset$griddata$area/1000/1000
dataset$area<-area

grid_cov<-dataset$griddata%>%as_tibble()%>%
  mutate(agri=agri_mean,wtr=wtr_mean)%>%
  dplyr::select(agri,wtr,-geometry)
mu_agri<-mean(grid_cov$agri)
sd_agri<-sd(grid_cov$agri)
mu_wtr<-mean(grid_cov$wtr)
sd_wtr<-sd(grid_cov$wtr)
grid_cov_std<-grid_cov%>%mutate(agri=(agri-mu_agri)/sd_agri,agri=(agri-mu_agri)/sd_agri)
dataset$grid_cov_std<-grid_cov_std
dataset$grid_cov_musd<-data.frame(mu=c(mu_agri,mu_wtr),sd=c(sd_agri,sd_wtr))
resolution<-c(x=dx,y=dy)

dataset$resolution<-resolution

dataset$effort<-effort$effort
effort_loc<-st_intersects(effort_st,dataset$griddata)%>%
  unlist
dataset$effort_loc<-effort_loc

dataset$detect<-as.matrix(detect[,-1])
dataset$effort_occ<-effort$effort_occ

secrdata<-secrad_data$new(coords=dataset$coords,
                          area=dataset$area,
                          grid_cov=dataset$grid_cov_std,
                          resolution=dataset$resolution)
secrdata$add_obs(type="poisson",
                 effort=dataset$effort,
                 effort_loc=dataset$effort_loc,
                 effort_occ=dataset$effort_occ,
                 detect=dataset$detect)

# plot --------------------------------------------------------------------
#secrdata$ggsecraddata(covname="agri")

#secrdata$ggsecraddata(covname="wtr")


# optim estimation --------------------------------------------------------
secrad_obj<-secrad$new(secrdata=secrdata)
envmodel<-list(D~1,C~agri+wtr,A~0)
indmodel<-c(A=FALSE,g0=FALSE)
occmodel<-c(A=FALSE,g0=FALSE)
secrad_obj$set_model(envmodel=envmodel,indmodel=indmodel,occmodel=occmodel)

initpar<-generate_init(secrad_obj)
initpar["dens_0"]<--1
initpar["conn_0"]<--2
initpar["g0_1"]<--5
secrad_res<-optim(initpar,secrad_obj$loglf,method="BFGS",control=list(maxit=1000,trace=2),loglfscale=-1,verbose=T,hessian=T)


# SGD ---------------------------------------------------------------------
# wrapper function --------------------------------------

create_subset_secrad <- function(
    original_secrdata,
    ids,
    model_settings
){
  
  orig_obs <- original_secrdata$obs[[1]]
  
  subset_detect <- NULL
  
  if(!is.null(orig_obs$detect)){
    subset_detect <- as.matrix(
      orig_obs$detect[, ids, drop = FALSE]
    )
  }
  
  new_secrdata <- secrad_data$new(
    coords = original_secrdata$coords,
    area = original_secrdata$area,
    grid_cov = original_secrdata$grid_cov,
    resolution = original_secrdata$resolution
  )
  
  new_secrdata$add_obs(
    type = orig_obs$type,
    effort = orig_obs$effort,
    effort_loc = orig_obs$effort_loc,
    effort_occ = orig_obs$effort_occ,
    detect = subset_detect
  )
  
  if(!is.null(original_secrdata$ind_cov)){
    
    new_secrdata$ind_cov <- original_secrdata$ind_cov[ids]
    
  } else {
    
    new_secrdata$ind_cov <- rep(1, length(ids))
    
  }
  
  new_obj <- secrad$new(
    secrdata = new_secrdata
  )
  
  new_obj$set_model(
    envmodel = model_settings$envmodel,
    indmodel = model_settings$indmodel,
    occmodel = model_settings$occmodel
  )
  
  return(new_obj)
  
}

# SGD objective function --------------------------------------

wrapper_sgd_loglf_fast <- function(
    par,
    obj_multi,
    obj_single,
    n_detected,
    sampling_rate,
    verbose = FALSE
){
  
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
  
  if(verbose){
    
    cat("[SGD loglf parts]\n")
    cat("ll_pois      :", sum(ll_pois), "\n")
    cat("ll_multi     :", out_multi$loglfmulti, "\n")
    cat("ll_single/r  :", out_single$loglfmulti / sampling_rate, "\n")
    cat("total        :", res, "\n")
    
  }
  
  return(res)
  
}

# detected individuals --------------------------------------

capture_counts <- colSums(dataset$detect)

multi_ids <- which(capture_counts > 1)
single_ids <- which(capture_counts == 1)

cat("multi captured :", length(multi_ids), "\n")
cat("single captured:", length(single_ids), "\n")

# model settings --------------------------------------

current_model_settings <- list(
  
  envmodel = list(
    D ~ 1,
    C ~ agri + wtr,
    A ~ 0
  ),
  
  indmodel = c(
    A = FALSE,
    g0 = FALSE
  ),
  
  occmodel = c(
    A = FALSE,
    g0 = FALSE
  )
  
)

# sampling settings --------------------------------------

sampling_rate <- 1.0

sample_size <- max(
  1,
  floor(length(single_ids) * sampling_rate)
)

is_full_sampling <- isTRUE(
  all.equal(sampling_rate, 1.0)
)

n_detected <- secrdata$nind

# fixed objects --------------------------------------

obj_multi_fixed <- create_subset_secrad(
  original_secrdata = secrdata,
  ids = multi_ids,
  model_settings = current_model_settings
)

# advdiff の結果は格子とパラメータだけで決まり、どの個体を持っているかに依存しない。
# multi 側と single 側で同じキャッシュを指させると、同じ cpar に対する
# ncell x ncell の計算が1回で済む（クマデータ ncell=8497 では約150秒/回）。
# 格子が違うオブジェクト同士なら share_advdiff_cache が止めてくれる。
adcache <- share_advdiff_cache(list(obj_multi_fixed))

if(is_full_sampling){

  obj_single_fixed <- create_subset_secrad(
    original_secrdata = secrdata,
    ids = single_ids,
    model_settings = current_model_settings
  )
  share_advdiff_cache(list(obj_single_fixed), cache = adcache)

} else {

  obj_single_fixed <- NULL

}

make_single_obj <- function(ids){

  obj <- create_subset_secrad(
    original_secrdata = secrdata,
    ids = ids,
    model_settings = current_model_settings
  )
  # ミニバッチのオブジェクトは毎反復作り直すので、そのつど繋ぎ直す
  share_advdiff_cache(list(obj), cache = adcache)
  obj

}

get_single_obj <- function(ids){
  
  if(is_full_sampling){
    
    return(obj_single_fixed)
    
  } else {
    
    return(make_single_obj(ids))
    
  }
  
}

# starting values --------------------------------------

initpar_test <- generate_init(secrad_obj)

initpar_test["dens_0"] <- -1
initpar_test["conn_0"] <- -2
initpar_test["g0_1"] <- -5

current_par <- initpar_test
#current_par <- secrad_res$par


print(round(current_par, 5))

# Adam settings --------------------------------------

max_iter <- 100

alpha_vec <- rep(
  0.01,
  length(current_par)
)

names(alpha_vec) <- names(current_par)
  
alpha_vec["dens_0"] <- 0.02
alpha_vec["conn_0"] <- 0.03

if("conn_agri" %in% names(alpha_vec)){
  alpha_vec["conn_agri"] <- 0.03
}

if("conn_wtr" %in% names(alpha_vec)){
  alpha_vec["conn_wtr"] <- 0.03
}

if("g0_1" %in% names(alpha_vec)){
  alpha_vec["g0_1"] <- 0.02
}

beta1 <- 0.9
beta2 <- 0.999
eps_adam <- 1e-8

m <- rep(0, length(current_par))
v <- rep(0, length(current_par))

names(m) <- names(current_par)
names(v) <- names(current_par)

max_step <- rep(
  0.05,
  length(current_par)
)

names(max_step) <- names(current_par)

max_step["dens_0"] <- 0.05
max_step["conn_0"] <- 0.10

if("conn_agri" %in% names(max_step)){
  max_step["conn_agri"] <- 0.05
}

if("conn_wtr" %in% names(max_step)){
  max_step["conn_wtr"] <- 0.05
}

if("g0_1" %in% names(max_step)){
  max_step["g0_1"] <- 0.05
}

# grad_cachewise は前進差分のみ。grad_method は記録として残してあるだけで、
# 勾配の計算には使われない（save.image の中身を過去の結果と揃えるため）。
grad_method <- "simple"
grad_eps <- 1e-4

# storage objects --------------------------------------

trace_par <- matrix(
  NA,
  nrow = max_iter,
  ncol = length(current_par)
)

colnames(trace_par) <- names(current_par)

trace_grad <- matrix(
  NA,
  nrow = max_iter,
  ncol = length(current_par)
)

colnames(trace_grad) <- names(current_par)

trace_step <- matrix(
  NA,
  nrow = max_iter,
  ncol = length(current_par)
)

colnames(trace_step) <- names(current_par)

trace_ll <- rep(
  NA,
  max_iter
)

# initial objective check --------------------------------------

if(is_full_sampling){
  
  current_single_sample <- single_ids
  
} else {
  
  current_single_sample <- sample(
    single_ids,
    size = sample_size
  )
  
}

obj_single_check <- get_single_obj(
  current_single_sample
)

initial_objfun <- function(p){
  
  names(p) <- names(current_par)
  
  wrapper_sgd_loglf_fast(
    par = p,
    obj_multi = obj_multi_fixed,
    obj_single = obj_single_check,
    n_detected = n_detected,
    sampling_rate = sampling_rate
  )
  
}

initial_ll <- initial_objfun(current_par)

cat("\n")
cat("initial objective =", initial_ll, "\n")

if(
  is.na(initial_ll) ||
  !is.finite(initial_ll)
){
  stop("Initial objective is NA or Inf.")
}

# Adam SGD optimization --------------------------------------

cat("\n")
cat("Adam-SGD started\n")

time_adam_sgd <- system.time(
  
  for(iter in 1:max_iter){
    
    if(is_full_sampling){
      
      current_single_sample <- single_ids
      
    } else {
      
      current_single_sample <- sample(
        single_ids,
        size = min(sample_size, length(single_ids))
      )
      
    }
    
    obj_single_iter <- get_single_obj(
      current_single_sample
    )
    
    objfun_iter <- function(p){
      
      names(p) <- names(current_par)
      
      wrapper_sgd_loglf_fast(
        par = p,
        obj_multi = obj_multi_fixed,
        obj_single = obj_single_iter,
        n_detected = n_detected,
        sampling_rate = sampling_rate
      )
      
    }
    
    # numDeriv::grad(method="simple") と同じ前進差分。評価の順序だけが違う。
    # cpar に効かない係数（dens_0, g0_1）を先に揺らすので、その間キャッシュが
    # 効き、最後に cpar0 へ戻る余分なミスが出ない。値は完全に一致する
    # （tests/cache_bench.R で最大差 0 を確認）。
    g <- tryCatch(

      grad_cachewise(
        func = objfun_iter,
        x = current_par,
        eps = grad_eps
      ),

      error = function(e){
        
        cat(
          sprintf(
            "\nGradient error at iter %d : %s\n",
            iter,
            e$message
          )
        )
        
        return(
          rep(
            NA,
            length(current_par)
          )
        )
        
      }
      
    )
    
    names(g) <- names(current_par)
    
    if(
      any(is.na(g)) ||
      any(!is.finite(g))
    ){
      cat("Gradient contains NA/Inf.\n")
      break
    }
    
    m <- beta1 * m +
      (1 - beta1) * g
    
    v <- beta2 * v +
      (1 - beta2) * g^2
    
    m_hat <- m /
      (1 - beta1^iter)
    
    v_hat <- v /
      (1 - beta2^iter)
    
    step <- alpha_vec *
      m_hat /
      (sqrt(v_hat) + eps_adam)
    
    step <- pmax(
      pmin(step, max_step),
      -max_step
    )
    
    current_par <- current_par + step
    
    trace_par[iter, ] <- current_par
    trace_grad[iter, ] <- g
    trace_step[iter, ] <- step
    
    curr_ll <- objfun_iter(
      current_par
    )
    
    trace_ll[iter] <- curr_ll
    
    if(iter == 1 || iter %% 5 == 0){
      
      cat(
        sprintf(
          "Iter %4d  objective %.8f  max|g| %.4e  max|step| %.4e\n",
          iter,
          curr_ll,
          max(abs(g)),
          max(abs(step))
        )
      )
      
      print(
        round(
          current_par,
          5
        )
      )
      
    }
    
  }
  
)

# result summary --------------------------------------

cat("\n")
cat("Adam-SGD completed\n")

print(time_adam_sgd)

final_par <- current_par

print(round(final_par, 5))

# save results --------------------------------------

save(
  secrad_res,
  final_par,
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
  file = "SGD_Adam_result_20260903.Rdata"
)
trace_par_t  <- trace_par[1:iter, ]
trace_ll_t   <- trace_ll[1:iter]
trace_grad_t <- trace_grad[1:iter, ]
trace_step_t <- trace_step[1:iter, ]

load("SGD_Adam_result_20260902.Rdata")
trace_par_n <- rbind(trace_par, trace_par_t)
trace_ll_n <- rbind(trace_ll, trace_ll_t)
trace_grad_n <- rbind(trace_grad, trace_grad_t)
trace_step_n <- rbind(trace_step, trace_step_t) 

load("SGD_Adam_result_20260903.Rdata")
trace_par  <- trace_par_n
trace_ll   <- trace_ll_n
trace_grad <- trace_grad_n
trace_step <- trace_step_n

# load("SGD_Adam_result_20260821.Rdata")
# trace_par_0821  <- trace_par[1:200, ]
# trace_ll_0821   <- trace_ll[1:200]
# trace_grad_0821 <- trace_grad[1:200, ]
# trace_step_0821 <- trace_step[1:200, ]
# 
# load("SGD_Adam_result_20260824.Rdata")
# trace_par_0824  <- trace_par[1:30, ]
# trace_ll_0824   <- trace_ll[1:30]
# trace_grad_0824 <- trace_grad[1:30, ]
# trace_step_0824 <- trace_step[1:30, ]
# 
# load("SGD_Adam_result_20260831-1.Rdata")
# trace_par_0831  <- trace_par[200:300, ]
# trace_ll_0831   <- trace_ll[200:300]
# trace_grad_0831 <- trace_grad
# trace_step_0831 <- trace_step
# 
# load("SGD_Adam_result_20260831-2.Rdata")
# trace_par_0831_2  <- trace_par[1:100, ]
# trace_ll_0831_2  <- trace_ll[1:100]
# trace_grad_0831_2 <- trace_grad[1:100, ]
# trace_step_0831_2 <- trace_step[1:100, ]
# 
# trace_par <- rbind(trace_par_0821, trace_par_0824) %>% rbind(trace_par_0831) %>% rbind(trace_par_0831_2)
# trace_ll <- rbind(trace_ll_0821, trace_ll_0824) %>% rbind(trace_ll_0831) %>% rbind(trace_ll_0831_2)
# trace_grad <- rbind(trace_grad_0821, trace_grad_0824) %>% rbind(trace_grad_0831) %>% rbind(trace_grad_0831_2)
# trace_step <- rbind(trace_step_0821, trace_step_0824) %>% rbind(trace_step_0831) %>% rbind(trace_step_0831_2)

save(
  secrad_res,
  final_par,
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
  file = "SGD_Adam_result_20260904.Rdata"
)

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
