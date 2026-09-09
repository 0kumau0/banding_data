# tests/repro_check.R --------------------------------------------------------
#
# set.seed() がシミュレーションに効いているかを確認する診断。
#
#   Rscript tests/repro_check.R
#   （作業ディレクトリはリポジトリルート = git/banding_data）
#
# 同じ seed で同じ設定のシミュレーションを繰り返し、結果が一致するかを見る。
#
# 2026-09-08 時点の結果: 一致しない。
#   - generate_ind()（個体の発生と初期配置）は R の RNG なので完全に再現する
#   - simulate()（移動と検出）は再現しない
#   - OMP_NUM_THREADS=1 にしても再現しない（並列処理の競合ではない）
#
# 原因は secrad.r の C++ 関数 advdiff_t_core の冒頭:
#
#     std::random_device seed_gen;
#     std::mt19937 engine(seed_gen());
#
# std::random_device は OS のエントロピーを引く非決定的な乱数源で、
# R の set.seed() とは無関係に初期化される。
#
# 直すなら advdiff_t_core に seed を引数で渡すか、R::unif_rand() を
# GetRNGstate() / PutRNGstate() で挟んで R の乱数系に載せる。
# ただし尤度エンジン本体の数値挙動が変わるので、影響を見極めてから。
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
RESULT_CSV <- "results/repro_check.csv"
SEED       <- 20260908
N_REP      <- 3

cat("source(", SOURCEPATH, ") ... ", sep = "")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
cat("done\n\n")

# --- 同一条件のシミュレーションを1回走らせ、要約を返す ----------------------

run_once <- function(seed) {
  set.seed(seed)

  NX <- 10; ncell <- NX * NX
  xcoord <- rep(1:NX, each = NX)
  ycoord <- rep(1:NX, NX)
  tr <- c(2, 6, 9)
  trapx <- rep(tr, length(tr)); trapy <- rep(tr, each = length(tr))
  effort_loc <- match(paste(trapx, trapy), paste(xcoord, ycoord))
  X <- as.numeric(scale(sin(xcoord / 2) + cos(ycoord / 2)))

  d <- secrad_data$new(coords     = cbind(x = xcoord, y = ycoord),
                       area       = rep(1, ncell),
                       grid_cov   = data.frame(X = X),
                       resolution = c(x = 1, y = 1))
  d$add_obs(type = "poisson", effort = rep(200, length(effort_loc)),
            effort_loc = effort_loc, effort_occ = rep(1, length(effort_loc)))
  d$set_truemodel(envmodel = list(D ~ 1, C ~ X, A ~ 1),
                  indmodel = c(A = FALSE, g0 = FALSE),
                  occmodel = c(A = FALSE, g0 = FALSE),
                  dpar = 0, cpar = c(-1, 0.3), adpar = -0.5, g0par = -4)
  d$set_sim(200, 1, 100, 20)

  d$generate_ind()                      # ここまでは R の RNG
  n_all    <- nrow(d$trueind)
  loc_sum  <- sum(d$trueind[, "ind_loc"])

  d$simulate(verbose = FALSE)           # ここから C++ の RNG
  d$update_detect()
  ncap <- colSums(d$obs[[1]]$detect)

  data.frame(n_all        = n_all,       # 発生した全個体数
             init_loc_sum = loc_sum,     # 初期位置の和（配置の指紋）
             n_detected   = d$nind,      # 検出された個体数
             n_multi      = sum(ncap > 1),
             n_total_det  = sum(d$obs[[1]]$detect))
}

# --- 既定のスレッド数で N_REP 回 --------------------------------------------

cat("=== 同じ seed で", N_REP, "回（既定のスレッド数）===\n")
res_default <- do.call(rbind, lapply(seq_len(N_REP), function(i) {
  r <- run_once(SEED); r$trial <- i; r$threads <- "default"; r
}))
print(res_default[, c("trial", "n_all", "init_loc_sum", "n_detected", "n_multi", "n_total_det")],
      row.names = FALSE)

# --- スレッド数を1に固定して N_REP 回 ---------------------------------------

cat("\n=== 同じ seed で", N_REP, "回（OMP_NUM_THREADS=1）===\n")
Sys.setenv(OMP_NUM_THREADS = "1")
res_single <- do.call(rbind, lapply(seq_len(N_REP), function(i) {
  r <- run_once(SEED); r$trial <- i; r$threads <- "1"; r
}))
print(res_single[, c("trial", "n_all", "init_loc_sum", "n_detected", "n_multi", "n_total_det")],
      row.names = FALSE)

# --- 判定 -------------------------------------------------------------------

res <- rbind(res_default, res_single)
res <- res[, c("threads", "trial", "n_all", "init_loc_sum",
               "n_detected", "n_multi", "n_total_det")]

gen_ok <- length(unique(res$n_all)) == 1L && length(unique(res$init_loc_sum)) == 1L
sim_ok <- length(unique(res$n_detected)) == 1L &&
          length(unique(res$n_multi)) == 1L &&
          length(unique(res$n_total_det)) == 1L

cat("\n", strrep("-", 60), "\n", sep = "")
cat("generate_ind()（個体の発生・初期配置）: ", if (gen_ok) "再現する" else "★再現しない", "\n", sep = "")
cat("simulate()（移動・検出）              : ", if (sim_ok) "再現する" else "★再現しない", "\n", sep = "")
cat(strrep("-", 60), "\n", sep = "")

if (!sim_ok) {
  cat("\nsimulate() が再現しません。secrad.r の advdiff_t_core が\n",
      "std::random_device で独自に乱数を初期化しているためです。\n",
      "詳細はこのファイル冒頭のコメントを参照。\n", sep = "")
}

dir.create(dirname(RESULT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(res, RESULT_CSV, row.names = FALSE)
cat("\n保存:", RESULT_CSV, "\n")
