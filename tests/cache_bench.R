# tests/cache_bench.R --------------------------------------------------------
#
# adcrsgd/sgd_utils.R の2つの最適化を検証して効果を測る。
#
#   Rscript tests/cache_bench.R      # 作業ディレクトリはリポジトリルート
#
# 実データ不要。合成データで
#
#   A) 現行         … obj_multi / obj_single が別キャッシュ、numDeriv::grad
#   B) 共有のみ     … キャッシュ共有、numDeriv::grad
#   C) 共有＋順序   … キャッシュ共有、grad_cachewise
#
# の3つについて、1反復あたりの advdiff.eigen 呼び出し回数と所要時間を測る。
# 併せて C の勾配が A と一致することを確かめる（順序を変えただけで
# 値が変わっていないこと）。
#
# 結果は results/cache_bench.csv に残す。
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
UTILPATH   <- "adcrsgd/sgd_utils.R"

NX      <- 30      # ncell = 900。advdiff のコストが見える大きさ
NT      <- 200
DPAR    <- 0.0
CPAR    <- c(-1.0, 0.35, -0.30)
ADPAR   <- -0.5
G0PAR   <- -3.2
N_ITER  <- 3       # 「反復」を何回まわして測るか
GRAD_EPS <- 1e-4
RATE    <- 0.2     # ミニバッチ率。obj_single を毎回作り直す現実の流れを再現する

PAR_NAMES <- c("dens_0", "conn_0", "conn_X1", "conn_X2", "g0_1")
PAR0 <- setNames(c(DPAR, CPAR, G0PAR), PAR_NAMES)

stopifnot(file.exists(SOURCEPATH), file.exists(UTILPATH))
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
source(UTILPATH, encoding = "UTF-8")
stopifnot(requireNamespace("numDeriv", quietly = TRUE))

cat("== 合成データ ==\n")
set.seed(20260909)
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
                      indmodel = c(A = FALSE, g0 = FALSE),
                      occmodel = c(A = FALSE, g0 = FALSE),
                      dpar = DPAR, cpar = CPAR, adpar = ADPAR, g0par = G0PAR)
simdata$set_sim(NT, 1, 100, 20)
simdata$generate_ind(); simdata$simulate(verbose = FALSE); simdata$update_detect()

ncap <- colSums(simdata$obs[[1]]$detect)
multi_ids <- which(ncap > 1); single_ids <- which(ncap == 1)
n_detected <- simdata$nind
cat(sprintf("   ncell=%d 検出=%d（複数回 %d / 単回 %d）\n",
            ncell, n_detected, length(multi_ids), length(single_ids)))
stopifnot(length(multi_ids) >= 5, length(single_ids) >= 30)

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
  om <- om_obj$loglf(p, loglfscale = 1, sgd = TRUE)
  os <- os_obj$loglf(p, loglfscale = 1, sgd = TRUE)
  sum(dpois(n_detected, lambda = exp(om$lambda_grp), log = TRUE)) +
    om$loglfmulti + os$loglfmulti / rate
}


# --- advdiff.eigen の呼び出し回数を数える -----------------------------------
# secrad の R6 メソッドは globalenv を辿って advdiff.eigen を解決するので、
# ここで差し替えれば内部の呼び出しも捕まえられる。
ADCOUNT <- 0L
.advdiff_orig <- advdiff.eigen
advdiff.eigen <- function(...) { ADCOUNT <<- ADCOUNT + 1L; .advdiff_orig(...) }

reset_count <- function() ADCOUNT <<- 0L


# --- 1反復ぶんを回す --------------------------------------------------------
# share = TRUE なら obj_multi と（毎回作り直す）obj_single でキャッシュを共有する。
# ordered = TRUE なら grad_cachewise、FALSE なら numDeriv::grad。
run_variant <- function(share, ordered, n_iter = N_ITER, seed = 1L) {
  set.seed(seed)                        # 3変種で同じミニバッチを引くように
  obj_multi <- make_subset(multi_ids)
  cache <- if (share) share_advdiff_cache(list(obj_multi)) else NULL

  cache_idx <- cache_par_idx(PAR_NAMES)
  grads <- matrix(NA_real_, n_iter, length(PAR0), dimnames = list(NULL, PAR_NAMES))

  reset_count()
  t0 <- Sys.time()
  for (it in seq_len(n_iter)) {
    ids <- sample(single_ids, max(1, floor(length(single_ids) * RATE)))
    obj_single <- make_subset(ids)
    if (share) share_advdiff_cache(list(obj_single), cache = cache)

    objfun <- function(p) { names(p) <- PAR_NAMES
      sgd_loglik(p, obj_multi, obj_single, RATE) }

    # パラメータを反復ごとに少しずらす（実際の Adam と同じく毎回 cpar が変わる）
    x <- PAR0 + (it - 1L) * 0.01

    grads[it, ] <- if (ordered) {
      grad_cachewise(objfun, x, eps = GRAD_EPS, cache_idx = cache_idx)
    } else {
      numDeriv::grad(objfun, x, method = "simple",
                     method.args = list(eps = GRAD_EPS))
    }
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  list(sec = elapsed, sec_per_iter = elapsed / n_iter,
       advdiff = ADCOUNT, advdiff_per_iter = ADCOUNT / n_iter, grads = grads)
}

cat("== 計測 ==\n")
A <- run_variant(share = FALSE, ordered = FALSE)
cat(sprintf("   A 現行        : %.2f 秒/iter, advdiff %.1f 回/iter\n",
            A$sec_per_iter, A$advdiff_per_iter))
B <- run_variant(share = TRUE,  ordered = FALSE)
cat(sprintf("   B 共有のみ    : %.2f 秒/iter, advdiff %.1f 回/iter\n",
            B$sec_per_iter, B$advdiff_per_iter))
C <- run_variant(share = TRUE,  ordered = TRUE)
cat(sprintf("   C 共有＋順序  : %.2f 秒/iter, advdiff %.1f 回/iter\n",
            C$sec_per_iter, C$advdiff_per_iter))


# --- 検証 -------------------------------------------------------------------
cat("\n== 検証 ==\n")
ok <- TRUE
say <- function(pass, msg) { ok <<- ok && pass
  cat(sprintf("  %s  %s\n", if (pass) "OK " else "NG ", msg)) }

# 順序を変えただけなので勾配は一致するはず。前進差分そのものは同じ式。
d_AC <- max(abs(A$grads - C$grads))
say(d_AC < 1e-6,
    sprintf("grad_cachewise が numDeriv::grad(simple) と一致（最大差 %.3e）", d_AC))

d_AB <- max(abs(A$grads - B$grads))
say(d_AB < 1e-8,
    sprintf("キャッシュ共有で勾配が変わらない（最大差 %.3e）", d_AB))

say(C$advdiff_per_iter < A$advdiff_per_iter,
    sprintf("advdiff の回数が減っている（%.1f -> %.1f 回/iter）",
            A$advdiff_per_iter, C$advdiff_per_iter))

# 共有キャッシュを違う格子に付けようとしたら止まること
# 別の格子（5x5）を持つオブジェクト。1次元に並べると neighmat の構築で
# secrad_data 側が落ちるので、ちゃんと2次元の格子にしておく。
bad <- local({
  nx <- 5; nc <- nx * nx
  xs <- rep(1:nx, each = nx); ys <- rep(1:nx, nx)
  s <- secrad_data$new(coords = cbind(x = xs, y = ys), area = rep(1, nc),
                       grid_cov = data.frame(X1 = rep(0, nc), X2 = rep(0, nc)),
                       resolution = c(x = 1, y = 1))
  s$add_obs(type = "poisson", effort = 10, effort_loc = 1, effort_occ = 1,
            detect = matrix(1, nrow = 1, ncol = 1))
  s$ind_cov <- 1
  o <- secrad$new(secrdata = s)
  o$set_model(envmodel = ENVMODEL, indmodel = INDMODEL, occmodel = OCCMODEL)
  o
})
guarded <- tryCatch({ share_advdiff_cache(list(make_subset(multi_ids), bad)); FALSE },
                    error = function(e) TRUE)
say(guarded, "格子が違うオブジェクト同士の共有は拒否される")

cat(sprintf("\n速度: A -> C で %.2f 倍 (%.2f -> %.2f 秒/iter)\n",
            A$sec_per_iter / C$sec_per_iter, A$sec_per_iter, C$sec_per_iter))

dir.create("results", showWarnings = FALSE)
write.csv(data.frame(
  variant = c("A_current", "B_shared", "C_shared_ordered"),
  share   = c(FALSE, TRUE, TRUE),
  ordered = c(FALSE, FALSE, TRUE),
  ncell   = ncell, n_iter = N_ITER,
  sec_per_iter     = c(A$sec_per_iter, B$sec_per_iter, C$sec_per_iter),
  advdiff_per_iter = c(A$advdiff_per_iter, B$advdiff_per_iter, C$advdiff_per_iter)),
  "results/cache_bench.csv", row.names = FALSE)
cat("保存: results/cache_bench.csv\n")

if (!ok) quit(status = 1)
cat("\nすべて成功。\n")
