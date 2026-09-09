# examples/adcr_sgd_minimal.R ------------------------------------------------
#
# ADCR + Adam-SGD の最小の一気通しサンプル。
#
#   Rscript examples/adcr_sgd_minimal.R
#   （作業ディレクトリはリポジトリルート = git/banding_data）
#
# このファイル1つだけで、
#
#   合成データの生成 → secrad.r の読み込み → Adam-SGD による推定 → 検証
#
# までが完結する。実データも config.R も他のスクリプトも要らない。
# 依存するのは尤度エンジン adcrsgd/secrad.r のみ。
#
# 目的は「何がどの順に起きているか」を1画面ずつ追えるようにすること。
# 実際の解析（wrapper_20260302.R / SGD_bera_20260818.R）は同じ骨格を
# 実データに対して回している。
#
# 所要時間の目安: 2〜4分（うち約25秒は secrad.r の C++ コンパイル）
# ---------------------------------------------------------------------------


# ===========================================================================
# 0. 設定  ── 触るのはこのブロックだけで済むようにしてある
# ===========================================================================

SOURCEPATH <- "adcrsgd/secrad.r"     # 尤度エンジン（sgd=TRUE 対応版）

SEED <- 20260908
#
# ⚠ 注意: SEED を固定しても simulate() の結果は毎回変わる。
# secrad.r の C++ 関数 advdiff_t_core が
#     std::random_device seed_gen;
#     std::mt19937 engine(seed_gen());
# と OS のエントロピーから独自に乱数を初期化しており、R の set.seed() の
# 影響を受けないため。OMP_NUM_THREADS=1 にしても解決しない（並列の競合ではない）。
# generate_ind()（個体の発生と初期配置）は R の RNG なので再現する。
# 検出パターンだけが実行のたびに変わる。

## --- 景観と調査デザイン ---------------------------------------------------
NX <- 10                              # グリッドは NX × NX セル
TRAP_AT <- c(2, 6, 9)                 # 検出器を置く座標（x,y の総当たり = 9基）
NT <- 200                             # 1 occasion あたりの時間 = 努力量

## --- シミュレーションの刻み -----------------------------------------------
#
# advdiff_t_core は陽的な時間発展。dt = TIMEBURNIN/STEPAD が拡散係数
# exp(CPAR[1]) に対して大きすぎると CFL 条件を破り、
# "Negative probability produced" で落ちる。
# グリッドが小さいほど拡散が相対的に速くなるので STEPAD は大きめに取る。
STEPSPERTIME <- 1
STEPAD       <- 100                   # dt = 20/100 = 0.2
TIMEBURNIN   <- 20

## --- データを生成した「真の」パラメータ ------------------------------------
DPAR  <- 0.0                          # 密度   log D  … exp(0)*100 ≒ 100個体
CPAR  <- c(-1.0, 0.3)                 # 連結性 log C ~ 1 + X
ADPAR <- -0.5                         # 移流
G0PAR <- -4                           # 検出   log g0

## --- Adam-SGD ---------------------------------------------------------------
#
# 設定は SGD_bera_20260818.R（クマデータで収束が確認できている実装）に合わせてある。
# 学習率とステップ上限は係数ごとに変えるのが要点。パラメータによって
# 尤度曲面の曲率がまるで違うため、スカラー1つでは片方が動かず片方が飛ぶ。

MAX_ITER      <- 100
SAMPLING_RATE <- 1.0                  # 1.0 = 単回検出個体も全件使う（収束確認済みの設定）

BETA1    <- 0.9
BETA2    <- 0.999
EPS_ADAM <- 1e-8
GRAD_EPS <- 1e-4                      # numDeriv の差分幅
GRAD_METHOD <- "simple"

# 係数ごとの学習率（未指定は DEFAULT）
ALPHA_DEFAULT <- 0.01
ALPHA_BY_PAR  <- c(dens_0 = 0.02, conn_0 = 0.03, conn_X = 0.03, g0_1 = 0.02)

# 係数ごとの1ステップ上限（発散の抑制）
MAX_STEP_DEFAULT <- 0.05
MAX_STEP_BY_PAR  <- c(dens_0 = 0.05, conn_0 = 0.10, conn_X = 0.05, g0_1 = 0.05)

## --- 初期値 -----------------------------------------------------------------
# SGD_bera_20260818.R と同じ置き方（dens_0=-1, conn_0=-2, g0_1=-5）。
# 真値から意図的にずらしてあり、そこから収束するかを見る。
INIT <- c(dens_0 = -1.0, conn_0 = -2.0, conn_X = 0.0, g0_1 = -5.0)

REPORT_EVERY <- 10
RUN_BFGS     <- TRUE                  # 参照解（全データでの最尤推定）を計算するか
BFGS_MAXIT   <- 30                    # 数値微分なので重い。控えめに。
PLOT_FILE    <- "examples/adcr_sgd_minimal_trace.pdf"   # NULL で作図しない
TRACE_CSV    <- "results/adcr_sgd_minimal_trace.csv"    # NULL で保存しない
SUMMARY_CSV  <- "results/adcr_sgd_minimal_summary.csv"  # 推定結果の表


# ===========================================================================
# 1. 尤度エンジンの読み込み
# ===========================================================================
#
# secrad.r は R6 クラス secrad / secrad_data と、実行時に sourceCpp する
# OpenMP 付き C++ を定義する。毎回コンパイルするので約25秒かかる。

cat("== 1. secrad.r ==\n")

stopifnot(file.exists(SOURCEPATH))
t0 <- Sys.time()
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
cat(sprintf("   読み込み %.1f秒\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

stopifnot("sgd" %in% names(formals(secrad$public_methods$loglf)))
stopifnot(requireNamespace("numDeriv", quietly = TRUE))


# ===========================================================================
# 2. 合成データの生成
# ===========================================================================
#
# secrad_data オブジェクトに「景観 + 調査デザイン + 真のパラメータ」を与え、
# simulate() で個体の移動と検出をシミュレートする。
#
#   coords      : セルの中心座標
#   grid_cov    : セルごとの環境共変量（ここでは X の1列）
#   effort_loc  : 検出器が置かれたセルの番号
#   detect      : 行 = 検出努力(検出器), 列 = 個体   ← 向きに注意

cat("== 2. 合成データ ==\n")
set.seed(SEED)

ncell  <- NX * NX
xcoord <- rep(1:NX, each = NX)
ycoord <- rep(1:NX, NX)

# 検出器の位置 → セル番号
trapx <- rep(TRAP_AT, length(TRAP_AT))
trapy <- rep(TRAP_AT, each = length(TRAP_AT))
effort_loc <- match(paste(trapx, trapy), paste(xcoord, ycoord))
ntrap <- length(effort_loc)

# 環境共変量（連続的な起伏。平均0・分散1に標準化）
X <- as.numeric(scale(sin(xcoord / 2) + cos(ycoord / 2)))

simdata <- secrad_data$new(coords     = cbind(x = xcoord, y = ycoord),
                           area       = rep(1, ncell),
                           grid_cov   = data.frame(X = X),
                           resolution = c(x = 1, y = 1))

simdata$add_obs(type       = "poisson",
                effort     = rep(NT, ntrap),
                effort_loc = effort_loc,
                effort_occ = rep(1, ntrap))

simdata$set_truemodel(envmodel = list(D ~ 1, C ~ X, A ~ 1),
                      indmodel = c(A = FALSE, g0 = FALSE),
                      occmodel = c(A = FALSE, g0 = FALSE),
                      dpar = DPAR, cpar = CPAR, adpar = ADPAR, g0par = G0PAR)

simdata$set_sim(NT, STEPSPERTIME, STEPAD, TIMEBURNIN)
simdata$generate_ind()                # 真の個体を発生させる
simdata$simulate(verbose = FALSE)     # 移動と検出をシミュレート
simdata$update_detect()               # 1回以上検出された個体だけを obs に残す

detect <- simdata$obs[[1]]$detect
ncap   <- colSums(detect)             # 個体ごとの検出回数（列 = 個体）

cat(sprintf("   全個体 %d / 検出 %d（複数回 %d, 単回 %d）/ 検出器 %d\n",
            nrow(simdata$trueind), simdata$nind,
            sum(ncap > 1), sum(ncap == 1), ntrap))

if (sum(ncap > 1) < 2)
  stop("複数回検出された個体が少なすぎます。NT を増やすか G0PAR を上げてください。")


# ===========================================================================
# 3. 尤度の部品
# ===========================================================================
#
# ADCR の対数尤度は2つの部分からなる。
#
#   (a) ポアソン部  … 検出された個体数が Lambda に従う確率
#   (b) 捕獲履歴部  … 各個体の検出パターンの確率
#
# loglf(sgd = TRUE) は、この2つを合成する前の部品を返す。
#
#   $lambda_grp   … log Lambda（(a) の材料）
#   $loglfmulti   … (b) の合計
#   $res          … 通常どおり合成したスカラー（sgd = FALSE と同じ値）
#
# SGD ではここが効いてくる。個体を「複数回検出（multi）」と
# 「単回検出（single）」に分け、single だけをミニバッチで抽出して
# 1/sampling_rate 倍して全体を推定する。multi は情報量が大きいので毎回全部使う。
# 部品で受け取れるので、この重み付けを外側で自由に組める。

model_settings <- list(envmodel = list(D ~ 1, C ~ X, A ~ 0),
                       indmodel = c(A = FALSE, g0 = FALSE),
                       occmodel = c(A = FALSE, g0 = FALSE))

## 指定した個体だけを持つ secrad オブジェクトを作る。
## detect は 行=努力・列=個体 なので、個体で絞るのは「列」。
make_subset <- function(secrdata, ids) {
  o <- secrdata$obs[[1]]

  sub <- secrad_data$new(coords     = secrdata$coords,
                         area       = secrdata$area,
                         grid_cov   = secrdata$grid_cov,
                         resolution = secrdata$resolution)

  sub$add_obs(type       = o$type,
              effort     = o$effort,
              effort_loc = o$effort_loc,
              effort_occ = o$effort_occ,
              detect     = as.matrix(o$detect[, ids, drop = FALSE]))

  sub$ind_cov <- if (is.null(secrdata$ind_cov)) rep(1, length(ids))
                 else secrdata$ind_cov[ids]

  obj <- secrad$new(secrdata = sub)
  obj$set_model(envmodel = model_settings$envmodel,
                indmodel = model_settings$indmodel,
                occmodel = model_settings$occmodel)
  obj
}

## multi と single の部品から SGD 用の目的関数値を組み立てる。
sgd_loglik <- function(par, obj_multi, obj_single, n_detected, sampling_rate) {
  out_multi  <- obj_multi$loglf(par,  loglfscale = 1, sgd = TRUE)
  out_single <- obj_single$loglf(par, loglfscale = 1, sgd = TRUE)

  ll_pois <- sum(dpois(n_detected, lambda = exp(out_multi$lambda_grp), log = TRUE))
  ll_hist <- out_multi$loglfmulti + out_single$loglfmulti / sampling_rate

  ll_pois + ll_hist
}

## par に名前を付けて sgd_loglik を呼ぶ閉包を作る。
## Adam ループ・初期チェック・BFGS refine の3箇所で使い回す。
make_objfun <- function(obj_single, sampling_rate, par_names) {
  force(obj_single); force(sampling_rate); force(par_names)
  function(p) {
    names(p) <- par_names
    sgd_loglik(p, obj_multi_fixed, obj_single, n_detected, sampling_rate)
  }
}

multi_ids  <- which(ncap > 1)
single_ids <- which(ncap == 1)
n_detected <- simdata$nind

# multi は毎回同じなので1度だけ作って使い回す（ここが速度に効く）
obj_multi_fixed <- make_subset(simdata, multi_ids)

full_sampling <- isTRUE(all.equal(SAMPLING_RATE, 1.0))
obj_single_all <- make_subset(simdata, single_ids)   # 全件版。refine と full 用
sample_size <- max(1, floor(length(single_ids) * SAMPLING_RATE))

## ミニバッチ用の single オブジェクトを返す。full なら作り直さない。
draw_single <- function() {
  if (full_sampling) return(obj_single_all)
  make_subset(simdata, sample(single_ids, min(sample_size, length(single_ids))))
}


# ===========================================================================
# 4. Adam-SGD
# ===========================================================================
#
# 勾配は numDeriv の数値微分で取る（解析勾配は実装されていない）。
# 対数尤度を最大化するので、Adam のステップは + 方向に足す。
# ステップ幅は MAX_STEP でクリップして発散を防ぐ。

cat("== 4. Adam-SGD ==\n")

par_names <- names(INIT)
current   <- INIT

## 係数ごとの設定を、既定値 + 個別指定 から par_names の順に組み立てる。
by_par <- function(default, overrides) {
  v <- setNames(rep(default, length(par_names)), par_names)
  hit <- intersect(names(overrides), par_names)
  v[hit] <- overrides[hit]
  v
}
alpha_vec    <- by_par(ALPHA_DEFAULT,    ALPHA_BY_PAR)
max_step_vec <- by_par(MAX_STEP_DEFAULT, MAX_STEP_BY_PAR)

cat("   alpha   :", paste(sprintf("%s=%.3f", par_names, alpha_vec), collapse = " "), "\n")
cat("   max_step:", paste(sprintf("%s=%.3f", par_names, max_step_vec), collapse = " "), "\n")

# 初期値の健全性チェック
ll0 <- make_objfun(draw_single(), SAMPLING_RATE, par_names)(current)
cat(sprintf("   初期 objective = %.4f\n", ll0))
if (!is.finite(ll0)) stop("初期値で目的関数が有限になりません。INIT を見直してください。")

trace_par  <- matrix(NA_real_, MAX_ITER, length(current), dimnames = list(NULL, par_names))
trace_ll   <- rep(NA_real_, MAX_ITER)
trace_grad <- matrix(NA_real_, MAX_ITER, length(current), dimnames = list(NULL, par_names))

m <- v <- setNames(rep(0, length(current)), par_names)
iter_done <- 0L

elapsed <- system.time(
  for (iter in seq_len(MAX_ITER)) {

    objfun <- make_objfun(draw_single(), SAMPLING_RATE, par_names)

    g <- tryCatch(
      numDeriv::grad(objfun, current, method = GRAD_METHOD,
                     method.args = list(eps = GRAD_EPS)),
      error = function(e) rep(NA_real_, length(current)))

    if (!all(is.finite(g))) {
      cat(sprintf("   iter %d: 勾配が NA/Inf になったため停止\n", iter)); break
    }

    # Adam
    m <- BETA1 * m + (1 - BETA1) * g
    v <- BETA2 * v + (1 - BETA2) * g^2
    step <- alpha_vec * (m / (1 - BETA1^iter)) / (sqrt(v / (1 - BETA2^iter)) + EPS_ADAM)
    step <- pmax(pmin(step, max_step_vec), -max_step_vec)   # 係数ごとにクリップ

    current <- current + step            # 最大化なので +

    trace_par[iter, ]  <- current
    trace_grad[iter, ] <- g
    trace_ll[iter]     <- objfun(current)
    iter_done <- iter

    if (iter == 1L || iter %% REPORT_EVERY == 0L)
      cat(sprintf("   iter %4d  ll %12.4f  max|g| %.3e  %s\n",
                  iter, trace_ll[iter], max(abs(g)),
                  paste(sprintf("%s=%+.3f", par_names, current), collapse = " ")))
  }
)

cat(sprintf("   %d iter / %.1f秒\n", iter_done, elapsed[["elapsed"]]))

trace_par  <- trace_par[seq_len(iter_done), , drop = FALSE]
trace_grad <- trace_grad[seq_len(iter_done), , drop = FALSE]
trace_ll   <- trace_ll[seq_len(iter_done)]


# --- トレースを CSV に保存（BFGS の前に書く） -------------------------------
# 100x4 程度で数十KBしかないので git に載せられる。BFGS は時間がかかるので、
# 先にここで保存しておけば途中で止めてもトレースは残る。

save_csv <- function(x, path) {
  if (is.null(path)) return(invisible(NULL))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(x, path, row.names = FALSE)
  cat("   保存:", path, "\n")
}

save_csv(data.frame(iter = seq_len(iter_done),
                    loglik = trace_ll,
                    trace_par,
                    setNames(as.data.frame(trace_grad), paste0("grad_", par_names))),
         TRACE_CSV)


# ===========================================================================
# 5. 参照解との比較
# ===========================================================================
#
# ミニバッチを使わない全データの対数尤度を BFGS で最大化し、
# Adam-SGD の到達点と突き合わせる。
# さらに、データを生成した真値とも比べる。

results <- data.frame(true = c(DPAR, CPAR, G0PAR), sgd = current,
                      row.names = par_names)

if (RUN_BFGS) {
  cat("== 5. BFGS 参照解 ==\n")
  negll_full <- local({
    f <- make_objfun(obj_single_all, 1.0, par_names)
    function(p) -f(p)
  })
  t0 <- Sys.time()
  # 数値微分なので1反復あたり loglf を10回以上呼ぶ。反復数は控えめにする。
  fit <- optim(current, negll_full, method = "BFGS",
               control = list(maxit = BFGS_MAXIT))
  cat(sprintf("   %.1f秒 / convergence=%d / logL %.4f -> %.4f\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              fit$convergence, -negll_full(current), -fit$value))
  results$bfgs <- fit$par
}

# 対数尤度の行を足す。SGD と BFGS の到達点を「同じ全データの目的関数」で比べる。
# パラメータの差と対数尤度の差を並べて見ないと、尤度が平坦かどうかが判断できない。
ll_full <- local({
  f <- make_objfun(obj_single_all, 1.0, par_names)
  c(sgd = f(current), bfgs = if (RUN_BFGS) f(fit$par) else NA_real_)
})
results <- rbind(results,
                 "log L" = c(true = NA_real_,
                             sgd  = unname(ll_full["sgd"]),
                             bfgs = if (RUN_BFGS) unname(ll_full["bfgs"]) else NULL))

cat("\n", strrep("=", 58), "\n", sep = "")
cat("推定結果\n")
print(round(results, 4))
cat(strrep("=", 58), "\n", sep = "")
cat("true = データを生成した真値 / sgd = Adam-SGD / bfgs = 全データの最尤解\n")



cat("A（移流）は生成時 A~1、推定時 A~0 としているため adv は推定していない。\n")


# --- 推定結果の表を CSV に保存 ----------------------------------------------

save_csv(data.frame(par = rownames(results), results), SUMMARY_CSV)


# ===========================================================================
# 6. 収束の可視化
# ===========================================================================

if (!is.null(PLOT_FILE) && iter_done > 1L) {
  dir.create(dirname(PLOT_FILE), showWarnings = FALSE, recursive = TRUE)
  pdf(PLOT_FILE, width = 10, height = 6)
  op <- par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
  for (j in seq_along(par_names)) {
    plot(trace_par[, j], type = "l", col = "steelblue",
         xlab = "iteration", ylab = "value", main = par_names[j])
    abline(h = results$true[j], col = "red", lty = 2)
    if (RUN_BFGS) abline(h = results$bfgs[j], col = "darkgreen", lty = 3)
  }
  plot(trace_ll, type = "l", col = "black",
       xlab = "iteration", ylab = "log-likelihood", main = "objective")
  par(op)
  dev.off()
  cat("\n作図:", PLOT_FILE, "（赤破線=真値, 緑点線=BFGS解）\n")
}
