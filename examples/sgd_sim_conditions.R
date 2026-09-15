# examples/sgd_sim_conditions.R ----------------------------------------------
#
# examples/sgd_simulation.R の複数条件版。
# **同じシミュレーションデータに対して、設定だけを変えて何本も回す。**
#
#   Rscript examples/sgd_sim_conditions.R
#   （作業ディレクトリはリポジトリルート。.Rprofile が移してくれる）
#
# ---------------------------------------------------------------------------
# なぜ別スクリプトなのか
#
# `sgd_simulation.R` は1条件だけを回す単純な形のまま残してある。
# 読んで理解するにはそちらが適していて、2026-09-15 の実行もあの形で行った。
#
# こちらは条件を並べて比べるための版。**中身の計算は同じ。**
#
# ---------------------------------------------------------------------------
# なぜ1回の実行の中で全条件を回すのか
#
# **set.seed を固定してもシミュレーションは再現しない**（§2 の注意を参照）。
# secrad.r の C++ 側が std::random_device で独自に乱数を初期化するため。
#
# したがってスクリプトを条件ごとに走らせると、**条件ごとに別のデータ**に
# なってしまい、差が「設定のせい」なのか「データのせい」なのか分からない。
# **同じデータに対して回すこと自体が、比較の前提**になる。
#
# 副産物として BFGS 参照解も1回で済む。
#
# ---------------------------------------------------------------------------
# 何を確かめるための道具か（2026-09-15 時点）
#
# 足環データは **複数回捕獲 100 : 単回捕獲 1000** の見込み。この SGD は
# 「単回が圧倒的に多い」前提で単回だけを間引く設計なので、
# **その比率で間引きが成立するか**を確かめないと本番に進めない。
#
# 読む順序:
#   1. まず rate = 1.0 を収束させる。**ここが収束しないと、rate を下げたときの
#      失敗が「間引きのせい」なのか「もともと収束しない」のか区別できない**
#   2. 収束したら、それを基準に rate を下げていく
# ---------------------------------------------------------------------------


# ===========================================================================
# 0. 設定  ── 触るのはこのブロックだけで済むようにしてある
# ===========================================================================

## --- 読み込むもの -----------------------------------------------------------
SOURCEPATH <- "adcrsgd/secrad.r"      # 尤度エンジン（sgd = TRUE 対応版）
UTILPATH   <- "adcrsgd/sgd_utils.R"   # advdiff キャッシュ共有 + grad_cachewise

## --- 結果の保存先 -----------------------------------------------------------
# 実行するたびに別ファイルへ保存する。固定名にしてはいけない。
# 再現できない実行なので、上書きすると前回の結果は二度と復元できない。
RUN_TAG <- ""           # 例: "testbedB" → results/sgd_sim_conditions_testbedB_...

## --- パラメータ名 -----------------------------------------------------------
PAR_NAMES <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")

## --- 景観 -------------------------------------------------------------------
NX         <- 20        # メッシュは NX x NX セル（ncell = NX^2 = 400）
CELL_SIZE  <- 1         # セルの一辺
CELL_AREA  <- 1         # セルの面積

## --- 調査デザイン -----------------------------------------------------------
# TRAP_COORD は**片方の軸の座標**。x と y の総当たりが検出器の位置になる。
# n 個指定すると n^2 基。既定は 5 個なので 25 基。
TRAP_COORD <- c(3, 7, 11, 15, 18)
EFFORT     <- 200       # 検出器あたりの努力量
N_OCCASION <- 1         # 調査機会の数

## --- データを生成した「真の」パラメータ -------------------------------------
#
# **検証台 B**: 足環データの比率（複数回:単回 = 1:10）に寄せた設定。
#
# 検証台 A（sgd_simulation.R の既定。TRUE_G0 = -3.0 / TRUE_DENS = -0.5）は
# 42 : 58 の半々で、**その比率では間引きの検証にならない**。
# 単回だけを間引く設計なので、単回が少なければ rate を下げても実効 nind が減らない。
#
#   複数:単回    rate 0.1 での実効 nind    コスト（個体数の約1.8乗）
#   42 : 58   →  42 + 6  = 48 / 100     →  2.1倍速
#   22 : 211  →  22 + 21 = 43 / 233     →  **21倍速**
#
# そこで検出率を下げ（再捕獲が減る）密度を上げる（個体が増える）。A とは逆向き。
TRUE_DENS  <- 2.0       # log 密度。exp(2) * 400セル ≒ 2960 個体
TRUE_CONN  <- c(conn_0 = -1.0, conn_agri = 0.6, conn_wtr = -0.5)
TRUE_ADV   <- -0.5      # log 移流。**推定モデルは A ~ 0 なので推定しない**
TRUE_G0    <- -5.0      # log 検出率。検出 230前後（複数回 22 / 単回 210 の見込み）

## --- シミュレーションの時間刻み ---------------------------------------------
# dt = TIMEBURNIN / STEPAD が拡散係数に対して大きすぎると CFL 条件を破り
# "Negative probability produced" で落ちる。落ちたら STEPAD を増やす。
# ここを増やしてもデータ生成が遅くなるだけで、最適化の速度には影響しない。
STEPSPERTIME <- 1
STEPAD       <- 200
TIMEBURNIN   <- 20

## --- 比較する条件 ← このスクリプトの主役 ------------------------------------
#
# 複数回捕獲された個体は毎回全部使い、単回捕獲の個体だけを間引いて
# 1/sampling_rate 倍で重みを戻す。1.0 なら間引かない（完全バッチ）。
#
# tag は出力ファイル名と図の凡例に使う。重複させないこと。
CONDITIONS <- data.frame(
  tag           = c("r100", "r050", "r020", "r010"),
  sampling_rate = c( 1.00,   0.50,   0.20,   0.10),
  beta2         = c(0.999,  0.999,  0.999,  0.999),
  alpha_mult    = c( 1.0,    1.0,    1.0,    1.0),
  stringsAsFactors = FALSE
)

# rate = 1.0 が失速したときに試す例:
#   CONDITIONS <- data.frame(
#     tag           = c("r100", "r100_b99", "r100_b99_a2"),
#     sampling_rate = c( 1.00,   1.00,       1.00),
#     beta2         = c(0.999,   0.99,       0.99),
#     alpha_mult    = c( 1.0,    1.0,        2.0),
#     stringsAsFactors = FALSE)

## --- Adam の設定（条件で上書きしない共通部分）------------------------------
MAX_ITER <- 200

# 係数ごとの学習率。尤度曲面の曲率が係数ごとに違うので、スカラー1つだと
# 片方が動かず片方が飛ぶ。条件の alpha_mult でまとめて倍率をかけられる。
ALPHA <- c(dens_0 = 0.02, conn_0 = 0.03, conn_agri = 0.03,
           conn_wtr = 0.03, g0_1 = 0.02)

# 係数ごとの1ステップ上限。発散の歯止めで、通常は出番がない。
MAX_STEP <- c(dens_0 = 0.05, conn_0 = 0.10, conn_agri = 0.05,
              conn_wtr = 0.05, g0_1 = 0.05)

BETA1    <- 0.9         # 1次モーメントの減衰（向き）。条件では振らない
EPS_ADAM <- 1e-8        # ゼロ割り防止
GRAD_EPS <- 1e-4        # 前進差分の幅

## --- 初期値 -----------------------------------------------------------------
# 真値から意図的に離してある。全条件で同じ点から出発する。
INIT <- c(dens_0 = -1.0, conn_0 = -2.0, conn_agri = 0.0,
          conn_wtr = 0.0, g0_1 = -5.0)

## --- 収束の診断 -------------------------------------------------------------
#
# **max|g| と max|step| は、単独では収束判定に使えない。**
# 勾配の大きさには決まったスケールがなく（個体数やパラメータ化で変わる）、
# Adam の歩幅は常に alpha 前後に正規化されるため。
#
# 代わりにスケールに依らない3つを見る。trace から計算できるので追加コスト無し。
#   1. 勾配の符号の一貫性 … 100%に近い＝一方向に押されている／50%前後＝振動
#   2. 正味の移動 / (alpha × 窓幅) … 1に近い＝直進中／0に近い＝足踏み
#   3. logL の上昇率 … 頭打ちになったか
DIAG_WINDOW <- 20

## --- 実行の制御 -------------------------------------------------------------
SEED         <- 20260914
RUN_OPTIM    <- TRUE    # BFGS で参照解を出すか（全条件で共有する）
RUN_HESSIAN  <- TRUE    # optim でヘッセ行列（＝標準誤差）も出すか
OPTIM_MAXIT  <- 1000
REPORT_EVERY <- 20      # 何反復ごとに途中経過を表示するか
MAKE_PLOT    <- TRUE    # トレースの図を PNG に出すか

## --- 保存先の組み立て（ここから下は触らない）-------------------------------
RUN_STAMP  <- format(Sys.time(), "%Y%m%d_%H%M%S")
RESULTFILE <- file.path(
  "results",
  sprintf("sgd_sim_conditions_%s%s.RData",
          if (nzchar(RUN_TAG)) paste0(RUN_TAG, "_") else "",
          RUN_STAMP))

## --- 設定の整合性チェック ---------------------------------------------------
stopifnot(
  identical(names(ALPHA),    PAR_NAMES),
  identical(names(MAX_STEP), PAR_NAMES),
  identical(names(INIT),     PAR_NAMES),
  length(TRUE_CONN) == 3L,
  nrow(CONDITIONS) >= 1L,
  !anyDuplicated(CONDITIONS$tag),
  all(CONDITIONS$sampling_rate >  0),
  all(CONDITIONS$sampling_rate <= 1),
  all(CONDITIONS$beta2 > 0), all(CONDITIONS$beta2 < 1)
)


# ===========================================================================
# 1. 尤度エンジンの読み込み
# ===========================================================================
# 毎回 C++ をコンパイルするので約25秒かかる。

cat("== 1. 尤度エンジン ==\n")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
source(UTILPATH, encoding = "UTF-8")


# ===========================================================================
# 2. シミュレーションデータの生成（1回だけ。全条件で共有する）
# ===========================================================================
#   coords     : セルの中心座標
#   area       : セルの面積
#   grid_cov   : セルごとの環境共変量（agri, wtr）
#   effort_loc : 検出器が置かれたセルの番号
#   detect     : 行 = 検出努力（検出器）, 列 = 個体   ← 向きに注意

cat("== 2. シミュレーションデータ ==\n")
set.seed(SEED)

ncell  <- NX * NX
xcoord <- rep(seq_len(NX), each = NX) * CELL_SIZE
ycoord <- rep(seq_len(NX), times = NX) * CELL_SIZE

## 環境共変量。**平均0・分散1に標準化しておく。**
## 標準化しておけば係数がそのまま効果の大きさになる。
agri <- as.numeric(scale(sin(xcoord / 3) + cos(ycoord / 4)))
wtr  <- as.numeric(scale(cos(xcoord / 5) * sin(ycoord / 3)))
grid_cov <- data.frame(agri = agri, wtr = wtr)

## 検出器の位置 → セル番号。expand.grid が x と y の総当たりを作る。
traps <- expand.grid(x = TRAP_COORD * CELL_SIZE,
                     y = TRAP_COORD * CELL_SIZE)
ntrap <- nrow(traps)

effort_loc <- match(paste(traps$x, traps$y), paste(xcoord, ycoord))
if (any(is.na(effort_loc)))
  stop("検出器が格子の外にあります。TRAP_COORD は 1〜", NX, " の範囲で指定してください。")

simdata <- secrad_data$new(coords     = cbind(x = xcoord, y = ycoord),
                           area       = rep(CELL_AREA, ncell),
                           grid_cov   = grid_cov,
                           resolution = c(x = CELL_SIZE, y = CELL_SIZE))

simdata$add_obs(type       = "poisson",
                effort     = rep(EFFORT, ntrap),
                effort_loc = effort_loc,
                effort_occ = rep(N_OCCASION, ntrap))

## 真のモデル。推定側は A ~ 0（移流なし）なので、移流は生成にだけ入る。
simdata$set_truemodel(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 1),
                      indmodel = c(A = FALSE, g0 = FALSE),
                      occmodel = c(A = FALSE, g0 = FALSE),
                      dpar     = TRUE_DENS,
                      cpar     = unname(TRUE_CONN),
                      adpar    = TRUE_ADV,
                      g0par    = TRUE_G0)

simdata$set_sim(EFFORT, STEPSPERTIME, STEPAD, TIMEBURNIN)
simdata$generate_ind()               # 真の個体を発生させる
simdata$simulate(verbose = FALSE)    # 移動と検出をシミュレート
simdata$update_detect()              # 1回以上検出された個体だけ残す

## ⚠ set.seed を固定しても検出パターンは毎回変わる（tests/repro_check.R）。
## **だからこそ、全条件を1回の実行の中で回す必要がある。**

detect         <- simdata$obs[[1]]$detect
capture_counts <- colSums(detect)    # 列 = 個体
multi_ids      <- which(capture_counts >  1)
single_ids     <- which(capture_counts == 1)
n_detected     <- simdata$nind

cat(sprintf("   ncell %d / 検出器 %d / 真の個体 %d\n",
            ncell, ntrap, nrow(simdata$trueind)))
cat(sprintf("   検出 %d（複数回 %d / 単回 %d）= 1 : %.1f\n",
            n_detected, length(multi_ids), length(single_ids),
            length(single_ids) / max(1L, length(multi_ids))))

if (length(multi_ids) < 2)
  stop("複数回検出された個体が少なすぎます。EFFORT か TRUE_G0 を上げてください。")


# ===========================================================================
# 3. 推定用の secrad オブジェクト
# ===========================================================================
# A ~ 0 は移流を推定しないという意味。

MODEL <- list(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

secrad_obj <- secrad$new(secrdata = simdata)
secrad_obj$set_model(envmodel = MODEL$envmodel,
                     indmodel = MODEL$indmodel,
                     occmodel = MODEL$occmodel)

stopifnot(identical(names(generate_init(secrad_obj)), PAR_NAMES))


# ===========================================================================
# 4. BFGS 参照解（1回だけ。全条件で共有する）
# ===========================================================================
# loglfscale = -1 で符号を反転し、optim に最小化させる。

if (RUN_OPTIM) {
  cat("== 4. BFGS 参照解 ==\n")
  t0 <- Sys.time()
  secrad_res <- optim(INIT, secrad_obj$loglf, method = "BFGS",
                      control = list(maxit = OPTIM_MAXIT, trace = 1, REPORT = 10),
                      loglfscale = -1, hessian = RUN_HESSIAN)
  ref_par <- setNames(secrad_res$par, PAR_NAMES)

  cat(sprintf("   %.1f分 / convergence = %d / logL = %.6f\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              secrad_res$convergence, -secrad_res$value))
  print(round(ref_par, 5))

  ref_se <- NULL
  if (RUN_HESSIAN) {
    # secrad_res$hessian は -logL のヘッセ行列（＝観測情報行列）。
    # その逆行列の対角の平方根が標準誤差。
    ref_se <- tryCatch(sqrt(diag(solve(secrad_res$hessian))), error = function(e) NULL)
    if (!is.null(ref_se) && all(is.finite(ref_se))) {
      cat("   標準誤差:\n")
      print(round(setNames(ref_se, PAR_NAMES), 5))
    } else {
      cat("   標準誤差: ヘッセ行列が反転できません\n")
      ref_se <- NULL
    }
  }
} else {
  secrad_res <- NULL; ref_par <- NULL; ref_se <- NULL
  cat("== 4. BFGS をスキップ ==\n")
}


# ===========================================================================
# 5. SGD の部品
# ===========================================================================

## --- 指定した個体だけを持つ secrad オブジェクトを作る ----------------------
# detect は 行 = 努力・列 = 個体 なので、個体で絞るのは「列」。
create_subset_secrad <- function(secrdata, ids) {
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
  obj$set_model(envmodel = MODEL$envmodel,
                indmodel = MODEL$indmodel,
                occmodel = MODEL$occmodel)
  obj
}

## --- SGD の目的関数 ---------------------------------------------------------
# loglf(sgd = TRUE) は合成前の部品を返す。
# 単回捕獲側を 1/sampling_rate 倍して全体を推定する。
sgd_loglf <- function(par, obj_multi, obj_single, n_detected, sampling_rate) {
  out_multi  <- obj_multi$loglf(par,  loglfscale = 1, sgd = TRUE)
  out_single <- obj_single$loglf(par, loglfscale = 1, sgd = TRUE)

  ll_pois <- sum(dpois(n_detected, lambda = exp(out_multi$lambda_grp), log = TRUE))
  ll_hist <- out_multi$loglfmulti + out_single$loglfmulti / sampling_rate

  ll_pois + ll_hist
}

## --- advdiff キャッシュの共有 -----------------------------------------------
# advdiff の結果は格子とパラメータだけで決まり、どの個体を持つかに依存しない。
# 同じキャッシュを指させると ncell x ncell の計算が1回で済む。
# 複数回捕獲の側は全条件で共通なので、ここで1回だけ作る。
obj_multi <- create_subset_secrad(simdata, multi_ids)
adcache   <- share_advdiff_cache(list(obj_multi))

## 単回捕獲の側は条件（sampling_rate）ごとに変わる。
## 戻り値は「ids を渡すと secrad オブジェクトを返す関数」。
make_single_getter <- function(rate) {
  if (isTRUE(all.equal(rate, 1.0))) {
    ## 完全バッチなら中身が毎回同じなので、1つ作って使い回す
    o <- create_subset_secrad(simdata, single_ids)
    share_advdiff_cache(list(o), cache = adcache)
    return(function(ids) o)
  }
  ## 間引くなら毎反復作り直す。そのつどキャッシュに繋ぎ直すこと
  function(ids) {
    o <- create_subset_secrad(simdata, ids)
    share_advdiff_cache(list(o), cache = adcache)
    o
  }
}


# ===========================================================================
# 6. Adam-SGD（条件ごとに1本ずつ）
# ===========================================================================

cat("\n== 6. Adam-SGD ==\n")

## 直近 DIAG_WINDOW 反復から、スケールに依らない3つの指標を出す。
diagnose <- function(iter, tp, tg, tl, alpha) {
  w <- min(DIAG_WINDOW, iter)
  if (w < 4L) return(invisible(NULL))
  idx <- seq.int(iter - w + 1L, iter)

  ## ① 勾配の符号の一貫性。1.0 = ずっと同じ向き（進行中）、0.5 = 振動（到達）
  sign_rate <- apply(tg[idx, , drop = FALSE], 2,
                     function(g) max(mean(g > 0), mean(g < 0)))
  ## ② 正味の移動を「歩き続けた場合の距離」で割る
  travel <- abs(tp[iter, ] - tp[idx[1L], ]) / (alpha * (w - 1L))
  ## ③ logL の上昇率
  rate <- mean(diff(tl[idx]))

  cat(sprintf("  ├ 直近%d反復 : logL 上昇率 %+.5f / 反復\n", w, rate))
  cat("  ├ 符号一致率 :",
      paste(sprintf("%s %3.0f%%", PAR_NAMES, 100 * sign_rate), collapse = "  "), "\n")
  cat("  ├ 正味移動/α :",
      paste(sprintf("%s %.2f", PAR_NAMES, travel), collapse = "  "), "\n")

  ## 2つを組み合わせて初めて意味が出る。
  ## 「押されている（符号が揃う）のに進んでいない（正味の移動が小さい）」が失速。
  pushed <- any(sign_rate > 0.8)
  moving <- any(travel > 0.3)
  cat("  └ 判定       :",
      if (pushed && moving)        "一方向に進行中（未到達）"
      else if (pushed && !moving)  "★ 押されているのに進んでいない＝失速（未到達）"
      else if (!pushed && !moving) "振動している。頂上に着いた可能性（最後にヘッセ行列で確認）"
      else                         "過渡的",
      "\n")
  invisible(list(sign_rate = sign_rate, travel = travel, rate = rate))
}

## 1条件ぶんの Adam-SGD。CONDITIONS の1行を受け取って結果を返す。
run_adam <- function(cond) {
  rate  <- cond$sampling_rate
  beta2 <- cond$beta2
  alpha <- ALPHA    * cond$alpha_mult
  mstep <- MAX_STEP * cond$alpha_mult

  get_single  <- make_single_getter(rate)
  is_full     <- isTRUE(all.equal(rate, 1.0))
  sample_size <- if (is_full) length(single_ids)
                 else max(1L, floor(length(single_ids) * rate))

  cat(sprintf("\n---- 条件 %s : rate=%.2f  beta2=%.3f  alpha x%.1f ----\n",
              cond$tag, rate, beta2, cond$alpha_mult))
  cat(sprintf("   1反復で使う個体: 複数回 %d ＋ 単回 %d ＝ %d（検出 %d）\n",
              length(multi_ids), sample_size,
              length(multi_ids) + sample_size, n_detected))

  current_par <- INIT
  m <- v <- setNames(rep(0, length(PAR_NAMES)), PAR_NAMES)

  trace_par  <- matrix(NA_real_, MAX_ITER, length(PAR_NAMES),
                       dimnames = list(NULL, PAR_NAMES))
  trace_grad <- trace_step <- trace_par
  trace_ll   <- rep(NA_real_, MAX_ITER)
  iter_done  <- 0L

  tm <- system.time(
    for (iter in seq_len(MAX_ITER)) {

      ## この反復で使う単回捕獲個体
      ids <- if (is_full) single_ids
             else sample(single_ids, size = min(sample_size, length(single_ids)))
      obj_single <- get_single(ids)

      ## この反復の目的関数。勾配の差分評価が同じ部分集合を見るように、
      ## ループの中で閉包を作る（評価ごとに引き直すと差分がノイズに埋もれる）。
      objfun <- function(p) {
        names(p) <- PAR_NAMES
        sgd_loglf(p, obj_multi, obj_single, n_detected, rate)
      }

      ## numDeriv::grad(method = "simple") と同じ前進差分。評価の順序だけが違う。
      g <- tryCatch(grad_cachewise(func = objfun, x = current_par, eps = GRAD_EPS),
                    error = function(e) {
                      cat(sprintf("反復 %d で勾配が計算できません: %s\n",
                                  iter, conditionMessage(e)))
                      rep(NA_real_, length(PAR_NAMES))
                    })
      names(g) <- PAR_NAMES
      if (any(!is.finite(g))) { cat("勾配に NA / Inf が出たので中断します。\n"); break }

      ## Adam の更新
      m <- BETA1 * m + (1 - BETA1) * g
      v <- beta2 * v + (1 - beta2) * g^2
      m_hat <- m / (1 - BETA1^iter)      # 初期の 0 バイアスの補正
      v_hat <- v / (1 - beta2^iter)

      step <- alpha * m_hat / (sqrt(v_hat) + EPS_ADAM)
      step <- pmax(pmin(step, mstep), -mstep)   # 発散の歯止め

      current_par <- current_par + step

      trace_par[iter, ]  <- current_par
      trace_grad[iter, ] <- g
      trace_step[iter, ] <- step
      trace_ll[iter]     <- objfun(current_par)
      iter_done <- iter

      if (iter == 1L || iter %% REPORT_EVERY == 0L) {
        cat(sprintf("反復 %4d/%d  logL %.6f  max|g| %.3e  max|step| %.3e\n",
                    iter, MAX_ITER, trace_ll[iter], max(abs(g)), max(abs(step))))
        print(round(current_par, 5))
        diagnose(iter, trace_par, trace_grad, trace_ll, alpha)
      }
    }
  )

  cat(sprintf("→ %s: %d 反復 / %.1f 分（%.1f 秒/反復）\n",
              cond$tag, iter_done, tm[["elapsed"]] / 60,
              tm[["elapsed"]] / max(1L, iter_done)))

  list(tag = cond$tag, cond = cond, final_par = current_par,
       trace_par = trace_par, trace_grad = trace_grad,
       trace_step = trace_step, trace_ll = trace_ll,
       iter_done = iter_done, elapsed = tm[["elapsed"]],
       alpha = alpha, n_used = length(multi_ids) + sample_size)
}

## 全条件を、同じデータに対して順に回す
runs <- lapply(seq_len(nrow(CONDITIONS)), function(i) run_adam(CONDITIONS[i, ]))
names(runs) <- CONDITIONS$tag


# ===========================================================================
# 7. 条件どうしの比較
# ===========================================================================

cat("\n== 7. 結果 ==\n")

true_par <- c(dens_0 = TRUE_DENS, conn_0 = unname(TRUE_CONN["conn_0"]),
              conn_agri = unname(TRUE_CONN["conn_agri"]),
              conn_wtr  = unname(TRUE_CONN["conn_wtr"]), g0_1 = TRUE_G0)

## --- 推定値を並べる ---------------------------------------------------------
est <- t(sapply(runs, function(r) r$final_par[PAR_NAMES]))
tab <- rbind(`真値` = true_par[PAR_NAMES], est)
if (!is.null(ref_par)) tab <- rbind(tab, `BFGS（参照解）` = ref_par[PAR_NAMES])
cat("\n-- 推定値 --\n")
print(round(tab, 5))

## --- 条件ごとの要約 ---------------------------------------------------------
# 判定の基準は標準誤差。データが決められる精度より細かく最適化しても意味がない。
# 残りの距離が標準誤差の 1% を切っていれば、実用上そこが答え。
Hinv <- if (!is.null(secrad_res) && !is.null(secrad_res$hessian))
          tryCatch(solve(secrad_res$hessian), error = function(e) NULL) else NULL

summ <- do.call(rbind, lapply(runs, function(r) {
  maxerr <- if (!is.null(ref_par))
              max(abs(r$final_par[PAR_NAMES] - ref_par[PAR_NAMES])) else NA_real_
  llgap  <- if (!is.null(secrad_res))
              (-secrad_res$value) - r$trace_ll[r$iter_done] else NA_real_
  ## 残りの距離 ≒ H^-1 g を標準誤差で割る
  dse <- NA_real_
  if (!is.null(Hinv) && !is.null(ref_se) && r$iter_done >= 1L) {
    rem <- as.vector(Hinv %*% r$trace_grad[r$iter_done, ])
    dse <- max(abs(rem / ref_se))
  }
  data.frame(tag = r$tag, rate = r$cond$sampling_rate, beta2 = r$cond$beta2,
             alpha_mult = r$cond$alpha_mult, n_used = r$n_used,
             `秒/反復` = r$elapsed / max(1L, r$iter_done),
             `最大誤差` = maxerr, `logL差` = llgap, `最大 距離/SE` = dse,
             check.names = FALSE, stringsAsFactors = FALSE)
}))
rownames(summ) <- NULL

cat("\n-- 条件ごとの到達点 --\n")
print(format(summ, digits = 4), row.names = FALSE)

cat("\n判定の目安（最大 距離/SE）:\n")
cat("  < 0.01 到達 / < 0.1 ほぼ到達 / < 1 標準誤差の範囲内 / >= 1 未到達\n")
cat("＊ 比べる相手は真値ではなく BFGS 解。同じ尤度を別の方法で最大化しているので、\n")
cat("  一致すべきはこちら。真値とのずれは推定量の誤差であって最適化の失敗ではない。\n")
cat("＊ rate = 1.0 が到達していないうちは、rate を下げた条件の失敗を\n")
cat("  「間引きのせい」と解釈してはいけない。\n")


# ===========================================================================
# 8. 図と保存
# ===========================================================================

## --- トレースの図（条件を重ね描き）------------------------------------------
# 黒・青・緑・橙 … 各条件 / 赤の破線 = BFGS 解 / 灰の点線 = 真値
# ラベルは ASCII。Windows の png() で日本語が化けることがあるため。

if (MAKE_PLOT && any(sapply(runs, function(r) r$iter_done) >= 2L)) {
  plotfile <- sub("\\.RData$", "_trace.png", RESULTFILE)
  cols <- c("black", "#1f6f80", "#557a4b", "#a8761f", "#7a3b8f", "#4a4a4a")
  cols <- rep_len(cols, length(runs))

  png(plotfile, width = 1200, height = 900, res = 120)
  op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

  for (i in seq_along(PAR_NAMES)) {
    nm <- PAR_NAMES[i]
    yr <- range(c(unlist(lapply(runs, function(r) r$trace_par[seq_len(r$iter_done), i])),
                  if (!is.null(ref_par)) ref_par[nm], true_par[nm]), na.rm = TRUE)
    plot(NA, xlim = c(1, MAX_ITER), ylim = yr,
         main = nm, xlab = "Iter", ylab = "Value")
    for (k in seq_along(runs)) {
      r <- runs[[k]]
      lines(seq_len(r$iter_done), r$trace_par[seq_len(r$iter_done), i],
            col = cols[k], lwd = 1.6)
    }
    if (!is.null(ref_par)) abline(h = ref_par[nm], col = "red", lty = 2, lwd = 2)
    abline(h = true_par[nm], col = "grey55", lty = 3, lwd = 1.6)
  }

  ## 6枚目: 対数尤度。
  ## ⚠ ylim に BFGS の logL を必ず含めること。含めないと基準線が描画範囲の外に出て
  ## 切り捨てられ、「きれいに収束した」ように見えてしまう。
  llr <- range(c(unlist(lapply(runs, function(r) r$trace_ll[seq_len(r$iter_done)])),
                 if (!is.null(secrad_res)) -secrad_res$value), na.rm = TRUE)
  plot(NA, xlim = c(1, MAX_ITER), ylim = llr,
       main = "log-likelihood", xlab = "Iter", ylab = "logL")
  for (k in seq_along(runs)) {
    r <- runs[[k]]
    lines(seq_len(r$iter_done), r$trace_ll[seq_len(r$iter_done)],
          col = cols[k], lwd = 1.6)
  }
  if (!is.null(secrad_res)) abline(h = -secrad_res$value, col = "red", lty = 2, lwd = 2)
  legend("bottomright", legend = names(runs), col = cols, lwd = 1.6,
         bty = "n", cex = 0.8)

  par(op)
  dev.off()
  cat("\n図: ", plotfile, "\n", sep = "")
  cat("   条件ごとの色 = ", paste(names(runs), collapse = " / "),
      " / 赤の破線 = BFGS 解 / 灰の点線 = 真値\n", sep = "")
}

## --- 保存 -------------------------------------------------------------------
## **データそのものも保存する。** trace だけ残しても、元のデータが無ければ
## 「別の設定を同じデータで試す」ができない。再現できない以上、
## ここで残さなければその実行は永久に失われる。
dir.create("results", showWarnings = FALSE)
trueind <- simdata$trueind            # 検出されなかった個体も含む真の配置

save(
  ## --- データ（これが無いと後から何もできない）---
  detect, grid_cov, xcoord, ycoord, effort_loc, trueind,
  ## --- 参照解 ---
  secrad_res, ref_par, ref_se, true_par,
  ## --- 全条件の結果と要約 ---
  runs, summ, CONDITIONS,
  ## --- 再現に要る設定 ---
  RUN_TAG, RUN_STAMP, SEED, NX, ncell, CELL_SIZE, CELL_AREA,
  TRAP_COORD, EFFORT, N_OCCASION, STEPSPERTIME, STEPAD, TIMEBURNIN,
  TRUE_DENS, TRUE_CONN, TRUE_ADV, TRUE_G0,
  MAX_ITER, ALPHA, MAX_STEP, BETA1, EPS_ADAM, GRAD_EPS, INIT, DIAG_WINDOW,
  ## --- 個体の内訳 ---
  n_detected, multi_ids, single_ids,
  file = RESULTFILE)

cat("\n保存: ", RESULTFILE, "\n", sep = "")
cat("（実行ごとに別ファイル。過去の結果は上書きされません）\n")
