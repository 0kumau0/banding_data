# examples/adcr_sgd_sampling.R ----------------------------------------------
#
# sampling_rate < 1 の Adam-SGD が BFGS 解に到達するかを合成データで検証する。
#
#   Rscript examples/adcr_sgd_sampling.R gen              # データ生成 + BFGS 参照解
#   Rscript examples/adcr_sgd_sampling.R run <label>      # 1条件を実行
#
# 条件は下の CONDITIONS で定義する。gen を先に1回だけ回し、
# 全条件が同じデータ・同じ参照解を使う（simulate() は再現しないため必須）。
#
# 背景: 2026-09-09 の調査で、loglf のコストは個体数ではなく ncell で決まると分かった
# （docs/2026-09-09_作業記録.md）。したがって sampling_rate は速度対策にはならない。
# ここで確かめたいのは速度ではなく「統計的に成立するか」の一点。
#
# 定数学習率の Adam は最適点の周りのノイズ球に留まるだけで収束しない。
# 減衰（alpha_t = alpha/(1+t/tau)）と Polyak 平均を入れた条件を併せて用意してある。
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
DATAFILE   <- "results/sampling_dataset.RData"     # *.RData は gitignore 済み

## --- 合成データの設定 -------------------------------------------------------
SEED   <- 20260909
NX     <- 20                          # ncell = 400。実データ(8497)より小さく取る
NT     <- 200
DPAR   <- 0.3
CPAR   <- c(-1.0, 0.35, -0.30)        # C ~ 1 + X1 + X2（クマの conn_0/agri/wtr に対応）
ADPAR  <- -0.5
G0PAR  <- -3.2
STEPSPERTIME <- 1; STEPAD <- 100; TIMEBURNIN <- 20

PAR_NAMES <- c("dens_0", "conn_0", "conn_X1", "conn_X2", "g0_1")
TRUEPAR   <- setNames(c(DPAR, CPAR, G0PAR), PAR_NAMES)
INIT      <- setNames(c(-1.0, -2.0, 0.0, 0.0, -5.0), PAR_NAMES)

## --- Adam の共通設定 --------------------------------------------------------
MAX_ITER    <- 200
BETA1 <- 0.9; BETA2 <- 0.999; EPS_ADAM <- 1e-8
GRAD_EPS    <- 1e-4
GRAD_METHOD <- "simple"
ALPHA    <- c(dens_0 = 0.02, conn_0 = 0.03, conn_X1 = 0.03, conn_X2 = 0.03, g0_1 = 0.02)
MAX_STEP <- c(dens_0 = 0.05, conn_0 = 0.10, conn_X1 = 0.05, conn_X2 = 0.05, g0_1 = 0.05)
EVAL_EVERY <- 10                      # 全データ尤度で評価する間隔（高いので間引く）

## --- 条件 -------------------------------------------------------------------
# decay_tau = NA で定数学習率。polyak_from = NA で平均を取らない。
CONDITIONS <- list(
  r100_const  = list(rate = 1.0, decay_tau = NA, polyak_from = NA),
  r050_const  = list(rate = 0.5, decay_tau = NA, polyak_from = NA),
  r020_const  = list(rate = 0.2, decay_tau = NA, polyak_from = NA),
  r010_const  = list(rate = 0.1, decay_tau = NA, polyak_from = NA),
  # 減衰と平均は「到達してから」でないと害になる。下の2条件がその対比。
  # decay_tau=50 は iter 200 時点で学習率が 1/5 まで落ちるため、
  # まだ最適点に到達していない段階で失速する（2026-09-09 の実測）。
  r020_decay  = list(rate = 0.2, decay_tau = 50, polyak_from = 100),
  r010_decay  = list(rate = 0.1, decay_tau = 50, polyak_from = 100),
  # 定数学習率のまま、定常状態に入ってから平均を取る（Polyak-Ruppert の本来の使い方）
  r020_polyak = list(rate = 0.2, decay_tau = NA, polyak_from = 150)
)

# ===========================================================================

args  <- commandArgs(trailingOnly = TRUE)
MODE  <- if (length(args) >= 1) args[1] else "gen"
LABEL <- if (length(args) >= 2) args[2] else NA_character_

suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
stopifnot("sgd" %in% names(formals(secrad$public_methods$loglf)))
stopifnot(requireNamespace("numDeriv", quietly = TRUE))

ENVMODEL <- list(D ~ 1, C ~ X1 + X2, A ~ 0)
INDMODEL <- c(A = FALSE, g0 = FALSE)
OCCMODEL <- c(A = FALSE, g0 = FALSE)

make_subset <- function(sd_obj, ids) {
  o <- sd_obj$obs[[1]]
  s <- secrad_data$new(coords = sd_obj$coords, area = sd_obj$area,
                       grid_cov = sd_obj$grid_cov, resolution = sd_obj$resolution)
  s$add_obs(type = o$type, effort = o$effort, effort_loc = o$effort_loc,
            effort_occ = o$effort_occ,
            detect = as.matrix(o$detect[, ids, drop = FALSE]))
  s$ind_cov <- rep(1, length(ids))
  obj <- secrad$new(secrdata = s)
  obj$set_model(envmodel = ENVMODEL, indmodel = INDMODEL, occmodel = OCCMODEL)
  obj
}

## multi と single の部品から目的関数値を合成する。
## single 側を 1/rate 倍することで全データ尤度の不偏推定になる。
sgd_loglik <- function(p, obj_multi, obj_single, n_detected, rate) {
  om <- obj_multi$loglf(p,  loglfscale = 1, sgd = TRUE)
  os <- obj_single$loglf(p, loglfscale = 1, sgd = TRUE)
  sum(dpois(n_detected, lambda = exp(om$lambda_grp), log = TRUE)) +
    om$loglfmulti + os$loglfmulti / rate
}


# ===========================================================================
# gen: データ生成 + BFGS 参照解
# ===========================================================================
if (MODE == "gen") {

  cat("== 合成データの生成 ==\n")
  set.seed(SEED)
  ncell  <- NX * NX
  xcoord <- rep(1:NX, each = NX); ycoord <- rep(1:NX, NX)
  trap_at <- unique(round(seq(2, NX - 1, length.out = 6)))
  effort_loc <- match(paste(rep(trap_at, length(trap_at)),
                            rep(trap_at, each = length(trap_at))),
                      paste(xcoord, ycoord))
  X1 <- as.numeric(scale(sin(xcoord / 2) + cos(ycoord / 2)))
  X2 <- as.numeric(scale(cos(xcoord / 3) * sin(ycoord / 4)))

  simdata <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                             area = rep(1, ncell),
                             grid_cov = data.frame(X1 = X1, X2 = X2),
                             resolution = c(x = 1, y = 1))
  simdata$add_obs(type = "poisson", effort = rep(NT, length(effort_loc)),
                  effort_loc = effort_loc, effort_occ = rep(1, length(effort_loc)))
  simdata$set_truemodel(envmodel = list(D ~ 1, C ~ X1 + X2, A ~ 1),
                        indmodel = INDMODEL, occmodel = OCCMODEL,
                        dpar = DPAR, cpar = CPAR, adpar = ADPAR, g0par = G0PAR)
  simdata$set_sim(NT, STEPSPERTIME, STEPAD, TIMEBURNIN)
  simdata$generate_ind(); simdata$simulate(verbose = FALSE); simdata$update_detect()

  ncap <- colSums(simdata$obs[[1]]$detect)
  multi_ids <- which(ncap > 1); single_ids <- which(ncap == 1)
  cat(sprintf("   ncell=%d 検出器=%d 全個体=%d 検出=%d（複数回 %d / 単回 %d）\n",
              ncell, length(effort_loc), nrow(simdata$trueind), simdata$nind,
              length(multi_ids), length(single_ids)))
  if (length(single_ids) < 60)
    stop("単回検出個体が少なすぎます。G0PAR / NT / DPAR を調整してください。")
  if (length(multi_ids) < 5)
    stop("複数回検出個体が少なすぎます。")

  obj_multi  <- make_subset(simdata, multi_ids)
  obj_single <- make_subset(simdata, single_ids)
  n_detected <- simdata$nind
  ll_full <- function(p) { names(p) <- PAR_NAMES
    sgd_loglik(p, obj_multi, obj_single, n_detected, 1.0) }

  cat("== BFGS 参照解 ==\n")
  t0 <- Sys.time()
  fit <- optim(INIT, function(p) -ll_full(p), method = "BFGS",
               control = list(maxit = 200, trace = 1, REPORT = 10))
  cat(sprintf("   %.1f秒 convergence=%d logL=%.4f\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              fit$convergence, -fit$value))
  bfgs_par <- setNames(fit$par, PAR_NAMES)
  print(round(rbind(true = TRUEPAR, bfgs = bfgs_par), 4))

  dir.create("results", showWarnings = FALSE)
  save(simdata, multi_ids, single_ids, n_detected, bfgs_par, fit, file = DATAFILE)
  cat("保存:", DATAFILE, "\n")
}


# ===========================================================================
# run: 1条件の Adam-SGD
# ===========================================================================
if (MODE == "run") {

  stopifnot(!is.na(LABEL), LABEL %in% names(CONDITIONS))
  cond <- CONDITIONS[[LABEL]]
  if (!file.exists(DATAFILE)) stop("先に gen を実行してください: ", DATAFILE)
  load(DATAFILE)

  cat(sprintf("== %s : rate=%.2f decay_tau=%s polyak_from=%s ==\n",
              LABEL, cond$rate, cond$decay_tau, cond$polyak_from))

  obj_multi      <- make_subset(simdata, multi_ids)
  obj_single_all <- make_subset(simdata, single_ids)
  ll_full <- function(p) { names(p) <- PAR_NAMES
    sgd_loglik(p, obj_multi, obj_single_all, n_detected, 1.0) }

  full_sampling <- isTRUE(all.equal(cond$rate, 1.0))
  sample_size   <- max(1, floor(length(single_ids) * cond$rate))
  draw_single <- function() {
    if (full_sampling) return(obj_single_all)
    make_subset(simdata, sample(single_ids, min(sample_size, length(single_ids))))
  }

  current <- INIT
  m <- v <- setNames(rep(0, length(current)), PAR_NAMES)
  polyak_sum <- setNames(rep(0, length(current)), PAR_NAMES); polyak_n <- 0L

  trace_par <- matrix(NA_real_, MAX_ITER, length(current), dimnames = list(NULL, PAR_NAMES))
  trace_llb <- rep(NA_real_, MAX_ITER)   # ミニバッチ推定値
  trace_llf <- rep(NA_real_, MAX_ITER)   # 全データ値（EVAL_EVERY ごと）

  t0 <- Sys.time(); iter_done <- 0L
  for (iter in seq_len(MAX_ITER)) {
    obj_single_iter <- draw_single()
    objfun <- function(p) { names(p) <- PAR_NAMES
      sgd_loglik(p, obj_multi, obj_single_iter, n_detected, cond$rate) }

    g <- tryCatch(numDeriv::grad(objfun, current, method = GRAD_METHOD,
                                 method.args = list(eps = GRAD_EPS)),
                  error = function(e) rep(NA_real_, length(current)))
    if (!all(is.finite(g))) { cat(sprintf("   iter %d: 勾配が NA/Inf。停止\n", iter)); break }

    alpha_t <- if (is.na(cond$decay_tau)) ALPHA else ALPHA / (1 + iter / cond$decay_tau)
    m <- BETA1 * m + (1 - BETA1) * g
    v <- BETA2 * v + (1 - BETA2) * g^2
    step <- alpha_t * (m / (1 - BETA1^iter)) / (sqrt(v / (1 - BETA2^iter)) + EPS_ADAM)
    step <- pmax(pmin(step, MAX_STEP), -MAX_STEP)
    current <- current + step

    if (!is.na(cond$polyak_from) && iter >= cond$polyak_from) {
      polyak_sum <- polyak_sum + current; polyak_n <- polyak_n + 1L
    }

    trace_par[iter, ] <- current
    trace_llb[iter]   <- objfun(current)
    if (iter %% EVAL_EVERY == 0L || iter == 1L) {
      trace_llf[iter] <- ll_full(current)
      cat(sprintf("   iter %4d  llb %11.3f  llf %11.3f  %s\n", iter,
                  trace_llb[iter], trace_llf[iter],
                  paste(sprintf("%s=%+.3f", PAR_NAMES, current), collapse = " ")))
    }
    iter_done <- iter
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("   %d iter / %.1f秒 (%.2f秒/iter)\n", iter_done, elapsed, elapsed / iter_done))

  final_par  <- current
  polyak_par <- if (polyak_n > 0L) polyak_sum / polyak_n else NULL

  cmp <- rbind(true = TRUEPAR, bfgs = bfgs_par, sgd = final_par)
  if (!is.null(polyak_par)) cmp <- rbind(cmp, polyak = polyak_par)
  cat("\n"); print(round(cmp, 4))
  cat(sprintf("\nlogL(全データ): bfgs %.4f / sgd %.4f%s\n",
              ll_full(bfgs_par), ll_full(final_par),
              if (!is.null(polyak_par)) sprintf(" / polyak %.4f", ll_full(polyak_par)) else ""))
  cat(sprintf("最大絶対誤差 vs BFGS: sgd %.4f%s\n",
              max(abs(final_par - bfgs_par)),
              if (!is.null(polyak_par))
                sprintf(" / polyak %.4f", max(abs(polyak_par - bfgs_par))) else ""))

  dir.create("results", showWarnings = FALSE)
  write.csv(data.frame(iter = seq_len(iter_done),
                       ll_batch = trace_llb[seq_len(iter_done)],
                       ll_full  = trace_llf[seq_len(iter_done)],
                       trace_par[seq_len(iter_done), , drop = FALSE]),
            sprintf("results/sampling_trace_%s.csv", LABEL), row.names = FALSE)

  summ <- data.frame(label = LABEL, rate = cond$rate,
                     decay_tau = cond$decay_tau, polyak_from = cond$polyak_from,
                     iter = iter_done, sec = elapsed, par = PAR_NAMES,
                     true = TRUEPAR, bfgs = bfgs_par, sgd = final_par,
                     polyak = if (is.null(polyak_par)) NA_real_ else polyak_par,
                     ll_bfgs = ll_full(bfgs_par), ll_sgd = ll_full(final_par),
                     ll_polyak = if (is.null(polyak_par)) NA_real_ else ll_full(polyak_par),
                     row.names = NULL)
  write.csv(summ, sprintf("results/sampling_summary_%s.csv", LABEL), row.names = FALSE)
  cat("保存: results/sampling_{trace,summary}_", LABEL, ".csv\n", sep = "")
}
