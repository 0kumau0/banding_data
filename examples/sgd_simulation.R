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
# 2026-09-15 に調整した。初回の実行では検出86・複数回捕獲16しか出ず、
# 標準誤差が 0.33〜0.60 と大きくて最適化の検証台として信号が弱かった。
#
#   TRUE_G0 を上げる … 検出率が上がり、1頭あたりの捕獲回数が増える。
#                       **再捕獲を増やす主役はこれ。** EFFORT でも増やせるが、
#                       EFFORT は set_sim にも渡っていて動物が動く時間を兼ねるので、
#                       2つのことが同時に変わってしまう
#   TRUE_DENS を下げる … 個体数を抑えて計算時間を相殺する。loglf のコストは
#                        個体数の約1.8乗なので、増やしすぎると1反復が重くなる。
#                        再捕獲の情報は「何頭いるか」ではなく
#                        「1頭が何回捕まるか」から来るので、この組み合わせでよい
#   conn_wtr を強める … 初回は真値 -0.3 に対し標準誤差 0.33 で、
#                       **真値が誤差より小さく識別できていなかった**（符号も合わず）。
#                       収束の判定に使える係数にするため絶対値を上げた
TRUE_DENS  <- -0.5      # log 密度。exp(-0.5) * 400セル ≒ 240 個体
TRUE_CONN  <- c(conn_0 = -1.0, conn_agri = 0.6, conn_wtr = -0.5)
TRUE_ADV   <- -0.5      # log 移流。**推定モデルは A ~ 0 なので推定しない**
TRUE_G0    <- -3.0      # log 検出率。exp(-3) = 0.0498

## --- シミュレーションの時間刻み ---------------------------------------------
# advdiff_t_core は陽的な時間発展。dt = TIMEBURNIN / STEPAD が拡散係数に対して
# 大きすぎると CFL 条件を破り "Negative probability produced" で落ちる。
# 落ちたら STEPAD を増やす。
STEPSPERTIME <- 1
STEPAD       <- 200     # dt = TIMEBURNIN / STEPAD
TIMEBURNIN   <- 20
# 連結性を上げる（conn_wtr の絶対値を上げた）と CFL の限界が厳しくなるので、
# 2026-09-15 に 100 から 200 へ増やして余裕を取った。
# ここを増やしてもデータ生成が少し遅くなるだけで、最適化の速度には影響しない。

## --- SGD のサンプリング -----------------------------------------------------
# 複数回捕獲された個体は毎回全部使い、単回捕獲の個体だけを間引いて
# 1/SAMPLING_RATE 倍で重みを戻す。1.0 なら間引かない（完全バッチ）。
SAMPLING_RATE <- 1.0

## --- Adam の設定 ------------------------------------------------------------
MAX_ITER <- 200

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

## --- 収束の診断 -------------------------------------------------------------
#
# **max|g| と max|step| は、単独では収束判定に使えない。**
#
#   - 勾配の大きさには決まったスケールがない。個体数が増えれば勾配も大きくなるし、
#     パラメータの取り方（log か生か）でも変わる。「|g| < 0.1 なら収束」のような
#     絶対的な閾値は作れない
#   - Adam の歩幅は m/sqrt(v) が ±1 に正規化されるので常に alpha 前後になる。
#     「歩幅が小さい」は、収束したからかもしれないし進めないだけかもしれない
#     （2026-09-14 にここを取り違えた。reports/20260914_report.md）
#
# 代わりに、スケールに依らない3つを見る。どれも trace から計算できるので追加コスト無し。
#
#   1. 勾配の符号の一貫性 … 直近 DIAG_WINDOW 反復で符号が揃っていれば、まだ一方向に
#                            押されている（未到達）。50% 前後なら最適点の周りで
#                            振動している（到達）。**単位を持たないのが利点**
#   2. logL の上昇率      … 頭打ちになったか。残りの反復数の見積もりにも使える
#   3. 正味の移動 / (alpha × 窓幅)
#                          … 1 に近ければ一方向に進行中、0 に近ければその場で振動
#
# 最後に、ヘッセ行列があれば「残りの距離 ≒ H^-1 g」を標準誤差と比べる。
# **標準誤差の 1% まで詰めれば実用上そこが答え**で、それ以上細かくしても
# データが決められる精度を超える。
DIAG_WINDOW <- 20       # 診断に使う直近の反復数

## --- 実行の制御 -------------------------------------------------------------
SEED         <- 20260914
RUN_OPTIM    <- TRUE    # BFGS で参照解を出すか
RUN_HESSIAN  <- TRUE    # optim でヘッセ行列（＝標準誤差）も出すか
OPTIM_MAXIT  <- 1000
REPORT_EVERY <- 10      # 何反復ごとに途中経過を表示するか（診断が4行出るので控えめに）
MAKE_PLOT    <- TRUE    # トレースの図を PNG に出すか（SGD_bera_20260818.R 由来）

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

## 直近 DIAG_WINDOW 反復から、スケールに依らない3つの指標を出す。
## trace を読むだけなので追加の尤度計算は要らない。
diagnose <- function(iter) {
  w <- min(DIAG_WINDOW, iter)
  if (w < 4L) return(invisible(NULL))
  idx <- seq.int(iter - w + 1L, iter)

  ## ① 勾配の符号の一貫性。1.0 = ずっと同じ向き（進行中）、0.5 = 振動（到達）
  sign_rate <- apply(trace_grad[idx, , drop = FALSE], 2,
                     function(g) max(mean(g > 0), mean(g < 0)))

  ## ② 正味の移動を「歩き続けた場合の距離」で割る。1 なら直進、0 ならその場足踏み
  net    <- abs(trace_par[iter, ] - trace_par[idx[1L], ])
  travel <- net / (ALPHA * (w - 1L))

  ## ③ logL の上昇率
  rate <- mean(diff(trace_ll[idx]))

  cat(sprintf("  ├ 直近%d反復 : logL 上昇率 %+.5f / 反復\n", w, rate))
  cat("  ├ 符号一致率 :",
      paste(sprintf("%s %3.0f%%", PAR_NAMES, 100 * sign_rate), collapse = "  "), "\n")
  cat("  ├ 正味移動/α :",
      paste(sprintf("%s %.2f", PAR_NAMES, travel), collapse = "  "), "\n")

  ## 2つを組み合わせて初めて意味が出る。
  ## 「押されている（符号が揃う）のに進んでいない（正味の移動が小さい）」が失速で、
  ## この track で繰り返し起きている状態。片方だけ見ても区別できない。
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
      diagnose(iter)     # max|g| と max|step| だけでは判定できないので
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

## --- 収束の最終判定 ---------------------------------------------------------
#
# 最適点の近くでは logL(θ) ≒ logL(θ̂) - ½(θ-θ̂)ᵀ H (θ-θ̂) と近似できる。
# ここから
#
#   残りの距離        θ̂ - θ ≒ H⁻¹ g
#   残りの対数尤度    logL(θ̂) - logL(θ) ≒ ½ gᵀ H⁻¹ g      （Newton decrement）
#
# H は optim が loglfscale = -1 で返したヘッセ行列（＝ -logL のもの）なので、
# そのまま使える。g は最終反復の勾配（SAMPLING_RATE = 1 なら全データの勾配）。
#
# **判定の基準は標準誤差。** データが決められる精度より細かく最適化しても意味がない。
# 残りの距離が標準誤差の 1% を切っていれば、実用上そこが答え。

if (RUN_HESSIAN && !is.null(secrad_res) && !is.null(secrad_res$hessian) && iter_done >= 1L) {
  cat("\n-- 収束の最終判定（ヘッセ行列による）--\n")
  g_last <- trace_grad[iter_done, ]
  ok <- tryCatch({
    Hinv      <- solve(secrad_res$hessian)
    remaining <- as.vector(Hinv %*% g_last)          # 残りの距離
    se        <- sqrt(diag(Hinv))                    # 標準誤差
    decrement <- 0.5 * sum(g_last * remaining)       # 残りの対数尤度
    names(remaining) <- names(se) <- PAR_NAMES

    print(round(rbind(`残りの距離` = remaining,
                      `標準誤差`   = se,
                      `距離/SE`    = remaining / se), 5))
    cat(sprintf("\n残りの対数尤度（Newton decrement）: %.6f\n", decrement))
    ## ⚠ これは2次近似なので、最適点から離れているときは**大幅に過小評価する**。
    ## 実測では decrement 2.2 に対して実際の差が 40.4 だったことがある。
    ## 参照解があるなら logL を直接比べるほうが確実。decrement は
    ## 「十分小さく、かつ他の指標も揃っている」ことの確認に使う。
    if (!is.null(secrad_res)) {
      actual <- (-secrad_res$value) - trace_ll[iter_done]
      cat(sprintf("参照解との実際の logL 差       : %.6f\n", actual))
      if (actual > 5 * max(decrement, 1e-9))
        cat("  ＊ decrement が実際の差より大幅に小さい。2次近似が効く範囲の外にいます。\n")
    }

    worst <- max(abs(remaining / se))
    cat(sprintf("最大の 距離/SE : %.4f\n", worst))
    cat("判定: ",
        if (worst < 0.01) "到達。標準誤差の 1% 未満まで詰まっている"
        else if (worst < 0.1) "ほぼ到達。標準誤差の 10% 未満"
        else if (worst < 1)   "未到達だが標準誤差の範囲内。反復を増やせば届く"
        else                  "未到達。標準誤差を超えてずれている",
        "\n", sep = "")
    TRUE
  }, error = function(e) {
    cat("  ヘッセ行列が反転できないので判定できません: ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
} else {
  cat("\n-- 収束の最終判定はスキップ（ヘッセ行列が無い）--\n")
  cat("   RUN_HESSIAN = TRUE にすると、残りの距離を標準誤差と比べて判定します。\n")
}

## --- トレースの図 -----------------------------------------------------------
# SGD_bera_20260818.R の末尾にあった図を移したもの。
# 元は青線＋赤の破線だったが、線の役割を分けやすいのでトレースは黒にした。
#   黒の実線 = Adam-SGD の推移 / 赤の破線 = BFGS 解 / 灰の点線 = 真値
# 6枚目のパネルには対数尤度の推移を置く（元コードでは空いていた）。

if (MAKE_PLOT && iter_done >= 2L) {
  plotfile <- sub("\\.RData$", "_trace.png", RESULTFILE)
  png(plotfile, width = 1200, height = 800, res = 120)
  op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

  ## ラベルは ASCII にしておく。Windows の png() で日本語が化けることがあるため
  ## （元コードも xlab="Iter", ylab="Value" だった）。
  for (i in seq_along(PAR_NAMES)) {
    nm <- PAR_NAMES[i]
    yr <- range(c(trace_par[seq_len(iter_done), i],
                  if (!is.null(ref_par)) ref_par[nm],
                  true_par[nm]), na.rm = TRUE)
    plot(seq_len(iter_done), trace_par[seq_len(iter_done), i], type = "l",
         col = "black", lwd = 1.6, ylim = yr,
         main = nm, xlab = "Iter", ylab = "Value")
    if (!is.null(ref_par)) abline(h = ref_par[nm], col = "red", lty = 2, lwd = 2)
    abline(h = true_par[nm], col = "grey55", lty = 3, lwd = 1.6)
  }

  ## 6枚目: 対数尤度。元コードでは空いていたパネル
  ## ⚠ ylim に BFGS の logL を必ず含めること。含めないと基準線が描画範囲の外に出て
  ## 切り捨てられ、「きれいに収束した」ように見えてしまう（最初これで嵌まった）。
  llr <- range(c(trace_ll[seq_len(iter_done)],
                 if (!is.null(secrad_res)) -secrad_res$value), na.rm = TRUE)
  plot(seq_len(iter_done), trace_ll[seq_len(iter_done)], type = "l",
       col = "black", lwd = 1.6, ylim = llr, main = "log-likelihood",
       xlab = "Iter", ylab = "logL")
  if (!is.null(secrad_res)) abline(h = -secrad_res$value, col = "red", lty = 2, lwd = 2)

  par(op)
  ## 凡例だけの余白が無いので、図の外側に説明を出す
  dev.off()
  cat("\n図: ", plotfile, "\n", sep = "")
  cat("   黒の実線 = Adam-SGD / 赤の破線 = BFGS 解 / 灰の点線 = 真値\n")
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
