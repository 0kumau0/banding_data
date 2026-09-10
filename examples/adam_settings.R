# examples/adam_settings.R ---------------------------------------------------
#
# Adam の終盤の失速をどう直すかの比較実験。
#
#   Rscript examples/adam_settings.R run <label>
#   （作業ディレクトリはリポジトリルート = banding_data）
#
# データは Step 1 の合成データをそのまま使う（results/sampling_dataset.RData）。
# 無ければ先に  Rscript examples/adcr_sgd_sampling.R gen  を回すこと。
# BFGS 解が既知で、sampling_rate = 1.0 ならミニバッチノイズも無いので、
# 「最適化の設定だけ」を切り分けて比べられる。
#
# ---------------------------------------------------------------------------
# 何を調べるのか
# ---------------------------------------------------------------------------
#
# 2026-09-09 に、クマ実データの旧実行（630反復）で妙なことが起きていたと分かった。
# 451反復以降、**勾配の符号は一貫しているのにステップが alpha の 1/1000 まで
# 落ちていた。** 収束して止まったのではなく、動けなくなって止まっていた。
#
# 同じことが Step 1 の合成データでも起きている。r100_const（完全バッチ、
# ノイズ無し）の |step|/alpha は
#
#   反復 1-50: 0.90   51-100: 0.11   101-150: 0.022   151-200: 0.0066
#
# と落ち、BFGS 解との最大誤差は 0.035 で頭打ちになった。
#
# 原因は Adam の v。ステップは alpha * m_hat / sqrt(v_hat) で、
# v は beta2 = 0.999 の指数移動平均、すなわち約1000反復ぶんの記憶を持つ。
# 序盤の大きな勾配（クマデータでは g0_1 で |g| ~ 107）がいつまでも
# v に残り、勾配が小さくなった後半のステップを抑え込む。
#
# 実データでは1反復 2200 秒なので、これは「あと数日回せば済む」問題ではない。
# 設定で直せるなら直しておきたい。
#
# ---------------------------------------------------------------------------
# 比べる設定
# ---------------------------------------------------------------------------
#
#   base       現行（beta2 = 0.999）。失速の再現
#   b2_099     beta2 = 0.99  … v の記憶を約100反復に短くする
#   b2_090     beta2 = 0.9   … さらに短く（約10反復）
#   reset100   100反復ごとに m / v / 反復カウンタを初期化する
#   alpha_hi   alpha を 2.5 倍（失速そのものは直さないが速く動く）
#   b2_099_hi  beta2 = 0.99 かつ alpha 2.5 倍
#
# 判定は「BFGS 解との最大絶対誤差が閾値を下回るまでの反復数」。
# 実データでは 1反復 = 約35分なので、反復数がそのまま日数になる。
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
UTILPATH   <- "adcrsgd/sgd_utils.R"
DATAFILE   <- "results/sampling_dataset.RData"

MAX_ITER   <- 400
RATE       <- 1.0        # 完全バッチ。最適化の設定だけを切り分けたいので固定
GRAD_EPS   <- 1e-4
BETA1      <- 0.9
EPS_ADAM   <- 1e-8
EVAL_EVERY <- 10         # 全データ尤度を評価する間隔

PAR_NAMES <- c("dens_0", "conn_0", "conn_X1", "conn_X2", "g0_1")
INIT      <- setNames(c(-1.0, -2.0, 0.0, 0.0, -5.0), PAR_NAMES)
ALPHA0    <- c(dens_0 = 0.02, conn_0 = 0.03, conn_X1 = 0.03,
               conn_X2 = 0.03, g0_1 = 0.02)
MAX_STEP  <- c(dens_0 = 0.05, conn_0 = 0.10, conn_X1 = 0.05,
               conn_X2 = 0.05, g0_1 = 0.05)

# 到達判定の閾値（BFGS 解との最大絶対誤差）
THRESH <- c(0.05, 0.02, 0.01)

CONDITIONS <- list(
  base      = list(beta2 = 0.999, reset_every = NA, alpha_mult = 1.0),
  b2_099    = list(beta2 = 0.99,  reset_every = NA, alpha_mult = 1.0),
  b2_090    = list(beta2 = 0.9,   reset_every = NA, alpha_mult = 1.0),
  reset100  = list(beta2 = 0.999, reset_every = 100, alpha_mult = 1.0),
  alpha_hi  = list(beta2 = 0.999, reset_every = NA, alpha_mult = 2.5),
  b2_099_hi = list(beta2 = 0.99,  reset_every = NA, alpha_mult = 2.5)
)

# ===========================================================================

args  <- commandArgs(trailingOnly = TRUE)
LABEL <- if (length(args) >= 2 && args[1] == "run") args[2] else
         stop("使い方: Rscript examples/adam_settings.R run <label>\n  label: ",
              paste(names(CONDITIONS), collapse = ", "))
stopifnot(LABEL %in% names(CONDITIONS))
cond <- CONDITIONS[[LABEL]]

suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
source(UTILPATH, encoding = "UTF-8")
if (!file.exists(DATAFILE))
  stop("先に Rscript examples/adcr_sgd_sampling.R gen を回してください: ", DATAFILE)
load(DATAFILE)     # simdata, multi_ids, single_ids, n_detected, bfgs_par, fit

ENVMODEL <- list(D ~ 1, C ~ X1 + X2, A ~ 0)
INDMODEL <- c(A = FALSE, g0 = FALSE)
OCCMODEL <- c(A = FALSE, g0 = FALSE)

make_subset <- function(ids) {
  o <- simdata$obs[[1]]
  s <- secrad_data$new(coords = simdata$coords, area = simdata$area,
                       grid_cov = simdata$grid_cov, resolution = simdata$resolution)
  s$add_obs(type = o$type, effort = o$effort, effort_loc = o$effort_loc,
            effort_occ = o$effort_occ,
            detect = as.matrix(o$detect[, ids, drop = FALSE]))
  s$ind_cov <- rep(1, length(ids))
  obj <- secrad$new(secrdata = s)
  obj$set_model(envmodel = ENVMODEL, indmodel = INDMODEL, occmodel = OCCMODEL)
  obj
}

sgd_loglik <- function(p, om_obj, os_obj, rate) {
  om <- om_obj$loglf(p,  loglfscale = 1, sgd = TRUE)
  os <- os_obj$loglf(p, loglfscale = 1, sgd = TRUE)
  sum(dpois(n_detected, lambda = exp(om$lambda_grp), log = TRUE)) +
    om$loglfmulti + os$loglfmulti / rate
}

obj_multi  <- make_subset(multi_ids)
obj_single <- make_subset(single_ids)
share_advdiff_cache(list(obj_multi, obj_single))    # Step 2 の高速化

objfun <- function(p) { names(p) <- PAR_NAMES
  sgd_loglik(p, obj_multi, obj_single, RATE) }

cat(sprintf("== %s : beta2=%.3f reset=%s alpha x%.1f ==\n",
            LABEL, cond$beta2, cond$reset_every, cond$alpha_mult))

alpha <- ALPHA0 * cond$alpha_mult
ll_bfgs <- objfun(bfgs_par)
cat(sprintf("   BFGS 解 logL = %.6f\n", ll_bfgs))

current <- INIT
m <- v <- setNames(rep(0, length(PAR_NAMES)), PAR_NAMES)
t_adam <- 0L                      # Adam の内部カウンタ（reset で 0 に戻す）

trace_par  <- matrix(NA_real_, MAX_ITER, length(PAR_NAMES),
                     dimnames = list(NULL, PAR_NAMES))
trace_step <- trace_grad <- trace_par
trace_ll   <- rep(NA_real_, MAX_ITER)
trace_err  <- rep(NA_real_, MAX_ITER)

t0 <- Sys.time()
for (iter in seq_len(MAX_ITER)) {

  if (!is.na(cond$reset_every) && iter > 1L && (iter - 1L) %% cond$reset_every == 0L) {
    # v の記憶を捨てる。勾配が小さくなった後の局面で、
    # 序盤の大きな勾配に引きずられたステップ抑制を解除するのが狙い。
    m <- v <- setNames(rep(0, length(PAR_NAMES)), PAR_NAMES)
    t_adam <- 0L
    cat(sprintf("   iter %4d: Adam の状態をリセット\n", iter))
  }
  t_adam <- t_adam + 1L

  g <- grad_cachewise(objfun, current, eps = GRAD_EPS)
  if (!all(is.finite(g))) { cat("   勾配が NA/Inf。停止\n"); break }

  m <- BETA1 * m + (1 - BETA1) * g
  v <- cond$beta2 * v + (1 - cond$beta2) * g^2
  step <- alpha * (m / (1 - BETA1^t_adam)) /
          (sqrt(v / (1 - cond$beta2^t_adam)) + EPS_ADAM)
  step <- pmax(pmin(step, MAX_STEP), -MAX_STEP)
  current <- current + step

  trace_par[iter, ]  <- current
  trace_grad[iter, ] <- g
  trace_step[iter, ] <- step
  trace_err[iter]    <- max(abs(current - bfgs_par))
  if (iter %% EVAL_EVERY == 0L || iter == 1L) {
    trace_ll[iter] <- objfun(current)
    cat(sprintf("   iter %4d  err %.5f  logL %.4f  |step|/alpha %.4f\n",
                iter, trace_err[iter], trace_ll[iter],
                median(abs(step) / alpha)))
  }
}
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("   %d iter / %.1f秒 (%.2f秒/iter)\n", MAX_ITER, elapsed, elapsed / MAX_ITER))

# --- 到達判定 ---------------------------------------------------------------
first_below <- function(th) { w <- which(trace_err <= th); if (length(w)) w[1] else NA_integer_ }
reach <- vapply(THRESH, first_below, integer(1))
names(reach) <- paste0("iter_to_", THRESH)

cat("\n閾値に到達した反復数:\n"); print(reach)
cat(sprintf("最終誤差 %.5f / 最終 logL %.4f (BFGS %.4f, 差 %.4f)\n",
            trace_err[MAX_ITER], objfun(current), ll_bfgs, objfun(current) - ll_bfgs))
cat("最終反復での |step|/alpha:\n")
print(round(abs(trace_step[MAX_ITER, ]) / alpha, 5))

dir.create("results", showWarnings = FALSE)
write.csv(data.frame(iter = seq_len(MAX_ITER), err = trace_err, ll = trace_ll,
                     trace_par,
                     setNames(as.data.frame(trace_step), paste0("step_", PAR_NAMES)),
                     setNames(as.data.frame(trace_grad), paste0("grad_", PAR_NAMES))),
          sprintf("results/adam_settings_trace_%s.csv", LABEL), row.names = FALSE)
write.csv(data.frame(label = LABEL, beta2 = cond$beta2,
                     reset_every = cond$reset_every, alpha_mult = cond$alpha_mult,
                     sec_per_iter = elapsed / MAX_ITER,
                     as.list(reach),
                     final_err = trace_err[MAX_ITER],
                     final_ll = objfun(current), ll_bfgs = ll_bfgs,
                     final_step_ratio = median(abs(trace_step[MAX_ITER, ]) / alpha)),
          sprintf("results/adam_settings_summary_%s.csv", LABEL), row.names = FALSE)
cat("保存: results/adam_settings_{trace,summary}_", LABEL, ".csv\n", sep = "")
