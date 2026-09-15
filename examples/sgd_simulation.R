# examples/sgd_simulation.R --------------------------------------------------
#
# クマ実データの解析（SGD_bera_20260818.R）を、そのままスケールダウンして
# シミュレーションデータで動かす。
#
#   Rscript examples/sgd_simulation.R
#   （作業ディレクトリはリポジトリルート。.Rprofile が移してくれる）
#
# 実データは要らない。このファイル1つで
#
#   シミュレーションデータの生成 → BFGS 参照解 → Adam-SGD → 比較
#
# まで完結する。依存は adcrsgd/ の2ファイルだけ。
#
# ---------------------------------------------------------------------------
# 実データ版と何が同じで何が違うか
#
#   同じ: モデル（D ~ 1 / C ~ agri + wtr / A ~ 0、poisson 観測）
#         パラメータ名（dens_0, conn_0, conn_agri, conn_wtr, g0_1）
#         BFGS 参照解の取り方（同じ初期値から optim）
#         複数回捕獲と単回捕獲の分け方、ミニバッチの重み戻し
#         Adam の実装（係数ごとの alpha と max_step、前進差分の勾配）
#
#   違う: データが実測ではなくシミュレーション
#         メッシュが小さい（ncell = NX^2。実データは 8497）
#         共変量を**正しく**標準化する（実データ版は agri を2回標準化し
#         wtr を生のまま渡していた。2026-09-10 に判明したバグ）
#
# 実データ版から削ったもの:
#   - 使っていないライブラリ（raster, gdistance, viridis, secr, cowplot ほか）
#   - プロット（コメントアウトされていた）
#   - 過去の .Rdata を読み込んで trace を rbind で繋ぐ処理（実行のたびに
#     書き換える前提の一時コードで、再利用できない）
#   - 係数名の存在チェック。モデルが固定なら名前は決まっている
# ---------------------------------------------------------------------------


# ===========================================================================
# 0. 設定  ── 触るのはこのブロックだけで済むようにしてある
# ===========================================================================

## --- 読み込むもの -----------------------------------------------------------
SOURCEPATH <- "adcrsgd/secrad.r"      # 尤度エンジン（sgd = TRUE 対応版）
UTILPATH   <- "adcrsgd/sgd_utils.R"   # advdiff キャッシュ共有 + grad_cachewise

## --- 結果の保存先 -----------------------------------------------------------
#
# **実行するたびに別ファイルへ保存する。固定名にしてはいけない。**
#
# このシミュレーションは set.seed を固定しても再現しない（§2 の注意を参照。
# secrad.r の C++ 側が std::random_device で独自に乱数を初期化するため）。
# つまり同じファイル名で上書きすると、**前回の結果は二度と復元できない**。
# 「再現できないから保存する」のに上書きしたら意味がない。
#
# RUN_TAG に短い文字列を入れると、ファイル名の先頭に付いて後から見分けやすい。
# 例: RUN_TAG <- "agri_strong" → results/sgd_simulation_agri_strong_20260915_143022.RData
RUN_TAG <- ""

## --- パラメータ名 -----------------------------------------------------------
# モデルが D ~ 1 / C ~ agri + wtr / A ~ 0 なので、この5つで固定。
# 下の設定ベクトルはすべてこの順・この名前で書く。
PAR_NAMES <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")

## --- 景観（ここを小さくするのがスケールダウン）-----------------------------
NX         <- 20        # メッシュは NX x NX セル（ncell = NX^2 = 400）
CELL_SIZE  <- 1         # セルの一辺。座標の単位（実データでは km）
CELL_AREA  <- 1         # セルの面積。密度の単位を決める

## --- 調査デザイン -----------------------------------------------------------
# 検出器は格子状に置く。TRAP_COORD は**片方の軸の座標**で、
# x と y の総当たり（全組み合わせ）が検出器の位置になる。
#
#   TRAP_COORD が n 個  →  検出器は n^2 基
#   既定は 5 個なので 5 x 5 = 25 基（NX=20 の 400 セルに対して 6.3%）
#
#   y=18  ·  ·  ·  ·  ·
#   y=15  ·  ·  ·  ·  ·      · = 検出器
#   y=11  ·  ·  ·  ·  ·
#   y=7   ·  ·  ·  ·  ·
#   y=3   ·  ·  ·  ·  ·
#        x=3  7  11 15 18
TRAP_COORD <- c(3, 7, 11, 15, 18)

EFFORT     <- 200       # 検出器あたりの努力量（実データの effort に相当）
N_OCCASION <- 1         # 調査機会の数。実データ版も実質1

## --- データを生成した「真の」パラメータ -------------------------------------
# cpar は C ~ agri + wtr なので（切片, agri, wtr）の3つ。
# 共変量は標準化してあるので、係数はそのまま効果の大きさとして読める。
TRUE_DENS  <- 0.0       # log 密度。exp(0) * 400セル ≒ 400 個体
TRUE_CONN  <- c(conn_0 = -1.0, conn_agri = 0.6, conn_wtr = -0.3)
TRUE_ADV   <- -0.5      # log 移流。**推定モデルは A ~ 0 なので推定しない**
TRUE_G0    <- -4.0      # log 検出率

## --- シミュレーションの時間刻み ---------------------------------------------
# advdiff_t_core は陽的な時間発展。dt = TIMEBURNIN / STEPAD が拡散係数に対して
# 大きすぎると CFL 条件を破り "Negative probability produced" で落ちる。
# 落ちたら STEPAD を増やす。
STEPSPERTIME <- 1
STEPAD       <- 100     # dt = TIMEBURNIN / STEPAD
TIMEBURNIN   <- 20

## --- SGD のサンプリング -----------------------------------------------------
# 複数回捕獲された個体は毎回全部使い、単回捕獲の個体だけを間引いて
# 1/SAMPLING_RATE 倍で重みを戻す。1.0 なら間引かない（完全バッチ）。
SAMPLING_RATE <- 1.0

## --- Adam の設定 ------------------------------------------------------------
MAX_ITER <- 100

# 係数ごとの学習率。尤度曲面の曲率が係数ごとに違うので、スカラー1つだと
# 片方が動かず片方が飛ぶ。実データ版と同じ値。
ALPHA <- c(dens_0 = 0.02, conn_0 = 0.03, conn_agri = 0.03,
           conn_wtr = 0.03, g0_1 = 0.02)

# 係数ごとの1ステップ上限。発散の歯止めで、通常は出番がない
# （実データの実行では一度も当たらなかった）。
MAX_STEP <- c(dens_0 = 0.05, conn_0 = 0.10, conn_agri = 0.05,
              conn_wtr = 0.05, g0_1 = 0.05)

BETA1    <- 0.9         # 1次モーメントの減衰。勾配の平均
BETA2    <- 0.999       # 2次モーメントの減衰。約 1/(1-BETA2) 反復ぶんの記憶
EPS_ADAM <- 1e-8        # ゼロ割り防止
GRAD_EPS <- 1e-4        # 前進差分の幅

## --- 初期値 -----------------------------------------------------------------
# 実データ版と同じ置き方。真値から意図的に離してあり、そこから収束するかを見る。
INIT <- c(dens_0 = -1.0, conn_0 = -2.0, conn_agri = 0.0,
          conn_wtr = 0.0, g0_1 = -5.0)

## --- 実行の制御 -------------------------------------------------------------
SEED         <- 20260914
RUN_OPTIM    <- TRUE    # BFGS で参照解を出すか
RUN_HESSIAN  <- TRUE    # optim でヘッセ行列（＝標準誤差）も出すか
OPTIM_MAXIT  <- 1000
REPORT_EVERY <- 5       # 何反復ごとに途中経過を表示するか

## --- 保存先の組み立て（ここから下は触らない）-------------------------------
# 秒まで入れるので、実行ごとに必ず別ファイルになる。
# secrad.r のコンパイルだけで約25秒かかるため、同じ秒に2回走ることはない。
RUN_STAMP  <- format(Sys.time(), "%Y%m%d_%H%M%S")
RESULTFILE <- file.path(
  "results",
  sprintf("sgd_simulation_%s%s.RData",
          if (nzchar(RUN_TAG)) paste0(RUN_TAG, "_") else "",
          RUN_STAMP))

## --- 設定の整合性チェック ---------------------------------------------------
stopifnot(
  identical(names(ALPHA),    PAR_NAMES),
  identical(names(MAX_STEP), PAR_NAMES),
  identical(names(INIT),     PAR_NAMES),
  length(TRUE_CONN) == 3L,
  SAMPLING_RATE > 0, SAMPLING_RATE <= 1
)


# ===========================================================================
# 1. 尤度エンジンの読み込み
# ===========================================================================
# secrad.r は R6 クラス secrad / secrad_data と、実行時に sourceCpp する
# OpenMP 付き C++ を定義する。毎回コンパイルするので約25秒かかる。

cat("== 1. 尤度エンジン ==\n")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
source(UTILPATH, encoding = "UTF-8")


# ===========================================================================
# 2. シミュレーションデータの生成
# ===========================================================================
# 実データ版ではここが csv と shapefile の読み込みだった。
# secrad_data に「景観 + 調査デザイン + 真のパラメータ」を与えて
# simulate() で個体の移動と検出を作る。
#
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

## 環境共変量。実データの agri（農地率）/ wtr（水域率）に対応する連続場。
## **平均0・分散1に標準化しておく。** 実データ版はここを誤っていた
## （agri を2回標準化し wtr は生のまま。2026-09-10 に判明）。
## 標準化しておけば係数がそのまま効果の大きさになり、
## 係数ごとに alpha を手で調整する必要も減る。
agri <- as.numeric(scale(sin(xcoord / 3) + cos(ycoord / 4)))
wtr  <- as.numeric(scale(cos(xcoord / 5) * sin(ycoord / 3)))

grid_cov <- data.frame(agri = agri, wtr = wtr)

## 検出器の位置 → セル番号
## expand.grid が x と y の総当たりを作る（5 値 × 5 値 = 25 行）。
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
## 実データ版も推定は A ~ 0 だった。
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

## ⚠ set.seed を固定しても検出パターンは毎回変わる。secrad.r の C++ 関数
## advdiff_t_core が std::random_device で独自に乱数を初期化しており、
## R の RNG の影響を受けないため（tests/repro_check.R）。

detect         <- simdata$obs[[1]]$detect
capture_counts <- colSums(detect)    # 列 = 個体
multi_ids      <- which(capture_counts >  1)
single_ids     <- which(capture_counts == 1)
n_detected     <- simdata$nind

cat(sprintf("   ncell %d / 検出器 %d / 真の個体 %d\n",
            ncell, ntrap, nrow(simdata$trueind)))
cat(sprintf("   検出 %d（複数回 %d / 単回 %d）\n",
            n_detected, length(multi_ids), length(single_ids)))

if (length(multi_ids) < 2)
  stop("複数回検出された個体が少なすぎます。EFFORT か TRUE_G0 を上げてください。")


# ===========================================================================
# 3. 推定用の secrad オブジェクト
# ===========================================================================
# 推定するモデル。実データ版と同じ。A ~ 0 は移流を推定しないという意味。

MODEL <- list(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

secrad_obj <- secrad$new(secrdata = simdata)
secrad_obj$set_model(envmodel = MODEL$envmodel,
                     indmodel = MODEL$indmodel,
                     occmodel = MODEL$occmodel)

## モデルから決まる係数名が、設定ブロックの想定と一致しているか
stopifnot(identical(names(generate_init(secrad_obj)), PAR_NAMES))


# ===========================================================================
# 4. BFGS 参照解
# ===========================================================================
# SGD の到達点を比べる相手。全個体・全データでの最尤解。
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

  if (RUN_HESSIAN) {
    # loglfscale = -1 で最小化したので secrad_res$hessian は -logL の
    # ヘッセ行列（＝観測情報行列）。その逆行列の対角の平方根が標準誤差。
    se <- tryCatch(sqrt(diag(solve(secrad_res$hessian))), error = function(e) NULL)
    if (!is.null(se) && all(is.finite(se))) {
      cat("   標準誤差:\n")
      print(round(setNames(se, PAR_NAMES), 5))
    } else {
      cat("   標準誤差: ヘッセ行列が反転できません\n")
    }
  }
} else {
  secrad_res <- NULL
  ref_par    <- NULL
  cat("== 4. BFGS をスキップ ==\n")
}


# ===========================================================================
# 5. SGD の部品
# ===========================================================================

## --- 指定した個体だけを持つ secrad オブジェクトを作る ----------------------
# detect は 行 = 努力・列 = 個体 なので、個体で絞るのは「列」。
# 行で切ると検出器数に化ける。
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
#   $lambda_grp … log Lambda（ポアソン部の材料）
#   $loglfmulti … 捕獲履歴部の合計
# 単回捕獲側を 1/sampling_rate 倍して全体を推定する。
sgd_loglf <- function(par, obj_multi, obj_single, n_detected, sampling_rate) {
  out_multi  <- obj_multi$loglf(par,  loglfscale = 1, sgd = TRUE)
  out_single <- obj_single$loglf(par, loglfscale = 1, sgd = TRUE)

  ll_pois <- sum(dpois(n_detected, lambda = exp(out_multi$lambda_grp), log = TRUE))
  ll_hist <- out_multi$loglfmulti + out_single$loglfmulti / sampling_rate

  ll_pois + ll_hist
}

## --- 固定オブジェクトと advdiff キャッシュの共有 ---------------------------
# advdiff の結果は格子とパラメータだけで決まり、どの個体を持つかに依存しない。
# multi 側と single 側で同じキャッシュを指させると、同じ cpar に対する
# ncell x ncell の計算が1回で済む（1反復の advdiff 呼び出しが 10回 → 4回）。
is_full_sampling <- isTRUE(all.equal(SAMPLING_RATE, 1.0))
sample_size      <- max(1L, floor(length(single_ids) * SAMPLING_RATE))

obj_multi <- create_subset_secrad(simdata, multi_ids)
adcache   <- share_advdiff_cache(list(obj_multi))

if (is_full_sampling) {
  obj_single_fixed <- create_subset_secrad(simdata, single_ids)
  share_advdiff_cache(list(obj_single_fixed), cache = adcache)
} else {
  obj_single_fixed <- NULL
}

## 完全バッチなら固定オブジェクトを使い回し、間引くなら毎反復作り直す。
get_single_obj <- function(ids) {
  if (is_full_sampling) return(obj_single_fixed)
  obj <- create_subset_secrad(simdata, ids)
  share_advdiff_cache(list(obj), cache = adcache)   # 作り直すたびに繋ぎ直す
  obj
}


# ===========================================================================
# 6. Adam-SGD
# ===========================================================================

cat("== 6. Adam-SGD ==\n")

current_par <- INIT
m <- v <- setNames(rep(0, length(PAR_NAMES)), PAR_NAMES)

trace_par  <- matrix(NA_real_, MAX_ITER, length(PAR_NAMES),
                     dimnames = list(NULL, PAR_NAMES))
trace_grad <- trace_step <- trace_par
trace_ll   <- rep(NA_real_, MAX_ITER)

iter_done <- 0L

time_adam <- system.time(
  for (iter in seq_len(MAX_ITER)) {

    ## この反復で使う単回捕獲個体
    ids <- if (is_full_sampling) single_ids
           else sample(single_ids, size = min(sample_size, length(single_ids)))
    obj_single <- get_single_obj(ids)

    ## この反復の目的関数。勾配の差分評価が同じ部分集合を見るように、
    ## ループの中で閉包を作る（評価ごとに引き直すと差分がノイズに埋もれる）。
    objfun <- function(p) {
      names(p) <- PAR_NAMES
      sgd_loglf(p, obj_multi, obj_single, n_detected, SAMPLING_RATE)
    }

    ## numDeriv::grad(method = "simple") と同じ前進差分。評価の順序だけが違う。
    ## cpar に効かない係数を先に揺らすので、その間 advdiff のキャッシュが効く。
    ## 値は完全に一致する（tests/cache_bench.R で最大差 0）。
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
    v <- BETA2 * v + (1 - BETA2) * g^2
    m_hat <- m / (1 - BETA1^iter)      # 初期の 0 バイアスの補正
    v_hat <- v / (1 - BETA2^iter)

    step <- ALPHA * m_hat / (sqrt(v_hat) + EPS_ADAM)
    step <- pmax(pmin(step, MAX_STEP), -MAX_STEP)   # 発散の歯止め

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
    }
  }
)

final_par <- current_par
cat(sprintf("\n%d 反復 / %.1f 分（%.1f 秒/反復）\n",
            iter_done, time_adam[["elapsed"]] / 60,
            time_adam[["elapsed"]] / max(1L, iter_done)))


# ===========================================================================
# 7. 比較と保存
# ===========================================================================

cat("\n== 7. 結果 ==\n")

true_par <- c(dens_0 = TRUE_DENS, conn_0 = unname(TRUE_CONN["conn_0"]),
              conn_agri = unname(TRUE_CONN["conn_agri"]),
              conn_wtr  = unname(TRUE_CONN["conn_wtr"]), g0_1 = TRUE_G0)

cmp <- rbind(`真値` = true_par[PAR_NAMES], `Adam-SGD` = final_par[PAR_NAMES])
if (!is.null(ref_par)) {
  cmp <- rbind(cmp, `BFGS（参照解）` = ref_par[PAR_NAMES],
               `差（SGD - BFGS）` = final_par[PAR_NAMES] - ref_par[PAR_NAMES])
}
print(round(cmp, 5))

if (!is.null(ref_par)) {
  cat(sprintf("\nBFGS 解との最大絶対誤差: %.6f\n",
              max(abs(final_par[PAR_NAMES] - ref_par[PAR_NAMES]))))
  cat("＊ 比べる相手は真値ではなく BFGS 解。\n")
  cat("  同じ尤度を別の方法で最大化しているので、一致すべきはこちら。\n")
  cat("  真値とのずれは推定量の誤差であって、最適化の失敗ではない。\n")
}

dir.create("results", showWarnings = FALSE)

## **データそのものも保存する。** trace だけ残しても、元のデータが無ければ
## 「別の最適化を同じデータで試す」「BFGS を回し直す」ができない。
## 再現できない以上、ここで残さなければその実行は永久に失われる。
## detect も共変量も小さい（数十 KB）ので、惜しむ理由がない。
trueind <- simdata$trueind            # 検出されなかった個体も含む真の配置

save(
  ## --- データ（これが無いと後から何もできない）---
  detect, grid_cov, xcoord, ycoord, effort_loc, trueind,
  ## --- 推定結果 ---
  secrad_res, ref_par, true_par, final_par,
  ## --- 経過の記録 ---
  trace_par, trace_ll, trace_grad, trace_step, iter_done, time_adam,
  ## --- 再現に要る設定 ---
  RUN_TAG, RUN_STAMP, SEED, NX, ncell, CELL_SIZE, CELL_AREA,
  TRAP_COORD, EFFORT, N_OCCASION, STEPSPERTIME, STEPAD, TIMEBURNIN,
  TRUE_DENS, TRUE_CONN, TRUE_ADV, TRUE_G0,
  SAMPLING_RATE, ALPHA, MAX_STEP, BETA1, BETA2, EPS_ADAM, GRAD_EPS, INIT,
  ## --- 個体の内訳 ---
  n_detected, multi_ids, single_ids,
  file = RESULTFILE)

cat("\n保存: ", RESULTFILE, "\n", sep = "")
cat("（実行ごとに別ファイル。過去の結果は上書きされません）\n")
