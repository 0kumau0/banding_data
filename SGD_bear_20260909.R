# SGD_bear_20260909.R --------------------------------------------------------
#
# クマ実データ（Fukasawa & Higashide 2025, Dryad）での Adam-SGD。
# SGD_bera_20260818.R を必要な部分だけに絞り、共変量の標準化漏れを直したもの。
#
#   Rscript SGD_bear_20260909.R
#   （作業ディレクトリはリポジトリルート = banding_data）
#
# 依存はリポジトリ内の2ファイルと sf / numDeriv だけ。
# 他のスクリプトを先に走らせる必要はない。
#
# ---------------------------------------------------------------------------
# 何を直したか
# ---------------------------------------------------------------------------
#
# 本家 ADCR/adcrtest2/secrad_bear.r:46 と、それを引き継いだ
# SGD_bera_20260818.R:51 に、こう書かれていた。
#
#   grid_cov_std <- grid_cov %>% mutate(agri = (agri-mu_agri)/sd_agri,
#                                       agri = (agri-mu_agri)/sd_agri)
#                                       ^^^^ 2回とも agri。wtr が標準化されない
#
# mu_wtr / sd_wtr はその直前で計算されているのに使われていなかった。
# 結果として wtr だけ生スケールのまま C ~ agri + wtr に入っていた。
#
# 実害は推定値ではなく最適化の幾何に出る。sd(wtr) = 0.0724 と小さいので、
# 同じ効果を表すのに conn_wtr は 1/0.0724 ≒ 14 倍の大きさを持つ必要がある。
# 実際 optim 解は conn_agri = 0.361 に対し conn_wtr = -6.411 だった。
#
# Adam の1反復あたりの移動は高々 alpha（0.02〜0.03）なので、初期値 -1 から
# -6.4 まで 5.41 も動くには 300 反復ほどかかる。1反復 2200 秒なので約1週間。
# 標準化すればこの距離が 0 -> -0.464 の 0.46 に縮む（約12分の1）。
#
# 旧実行のトレース（630反復）を解析して分かった注意点が2つある。
#
#   - **max_step は一度も効いていない**（全係数で 0 回）。効いている上限は alpha。
#   - **conn_wtr は1反復あたりではむしろ最も速く動いていた**（|step|/alpha が
#     全係数中で最大）。遅かったのは距離が長かったからで、勾配が
#     ノイズに埋もれていたわけではない（終盤でも符号一致率 100%）。
#
# したがって **標準化の修正だけで劇的に速くなるわけではない。** 初期値 -5 から
# 解 -1.79 まで 3.21 動く必要のある g0_1 が、~230 反復のボトルネックとして残る。
# 見込める短縮は 300 反復前後 -> 230 反復前後、2〜3割。
# 標準化を直す本当の価値は、係数ごとの alpha / max_step の手調整が要らなくなること、
# および係数が解釈可能なスケールになることのほう。
#
# もう一点。旧実行では 451 反復以降、**勾配の符号が一貫しているのにステップが
# alpha の 1/1000 まで落ちていた。**「収束して止まった」のではなく
# 「動けなくなって止まった」。beta2 = 0.999 の v が序盤の大きな勾配を
# 1000反復規模の記憶として保持し、後半のステップを抑え込むため。
# **良い初期値から始めるとこれが起きにくい**（序盤に大きな勾配が出ないので）。
# INIT_MODE = "transformed" が本番向きなのはそのため。
#
# BFGS は逆ヘッセ近似がスケール差を吸収するので、本家の optim 解析では
# この問題は表に出ない。Adam でだけ致命的になる。
#
# ---------------------------------------------------------------------------
# 実行の流れ
# ---------------------------------------------------------------------------
#
#   1. データを読み、agri と wtr の両方を標準化する
#   2. 旧解（生スケール wtr）を新しいパラメータ化へ厳密に座標変換し、
#      対数尤度が一致することを確かめる  ← ここが安価で強力な検算
#   3. その点から BFGS を回して参照解を得る（既に最適点なのですぐ終わるはず）
#   4. Adam-SGD を回す（チェックポイント付き。途中で止めて再開できる）
#   5. 生スケールへ逆変換して要約する
#
# 2 が通れば「標準化の修正は厳密な再パラメータ化であって、モデルは
# 変わっていない」ことが確定する。数日かけて回す前にここで止まれる。
#
# ---------------------------------------------------------------------------
# 推奨手順（既定値はこの「第1段」になっている）
# ---------------------------------------------------------------------------
#
# チェックポイントがあるので **MAX_ITER を先に決め打ちする必要はない。**
# 少なく回して様子を見てから増やせばよく、再実行すれば続きから走る。
#
#   第1段（既定。数時間〜半日）
#     INIT_MODE = "transformed", MAX_ITER = 20
#     → 対数尤度の照合、BFGS 参照解、1反復あたりの実測時間が得られる。
#       さらに「最適点に置いた Adam がそこに留まるか」の安定性確認になる。
#       ここで全部の前提が確かめられる。
#
#   第2段（第1段の実測時間を見てから決める）
#     MAX_ITER を増やして同じコマンドを再実行 → 続きから走る
#
#   第3段（収束の実証をしたい場合。数日〜1週間）
#     INIT_MODE = "far", MAX_ITER = 250 程度
#     → 遠い初期値から解にたどり着けることを示す run。
#       チェックポイントと結果のファイル名に INIT_MODE が入るので、
#       第1〜2段の結果とは別物として並存する。
#
# sampling_rate は 1.0 のままでよい。合成データでの検証（Step 1、
# reports/sampling_report.md）で、**下げても BFGS 解には届くが速度は
# ほとんど変わらない**（rate 1.0 → 0.1 で 4.4%）ことが分かっている。
# 下げる価値があるのは実データでの再現性を確認したいときだけで、
# その場合も標準化の修正とは別の run にすること（2つ同時に変えない）。
# ---------------------------------------------------------------------------


# ===========================================================================
# 0. 設定  ── 触るのはこのブロックだけで済むようにしてある
# ===========================================================================

## --- パス -----------------------------------------------------------------
SOURCEPATH <- "adcrsgd/secrad.r"       # 尤度エンジン（sgd=TRUE 対応版）
UTILPATH   <- "adcrsgd/sgd_utils.R"    # advdiff キャッシュ共有と順序つき有限差分

# Dryad のクマデータ。リポジトリルートから見た相対パス。
# 別PCでレイアウトが違う場合はここだけ書き換える。
BEAR_DIR <- "../../ADCR/doi_10_5061_dryad_ksn02v7bq__v20250117"

## --- 出力 -----------------------------------------------------------------
# チェックポイントと結果のファイル名には INIT_MODE を入れる。
# 初期値を変えたのに前の途中経過から再開してしまう事故を防ぐため。
# BFGS 参照解は初期値に依存しないので共通。
TAG        <- "SGD_bear_20260909"
OPTIM_FILE <- paste0(TAG, "_optim.rds")       # BFGS 参照解（一度作れば再利用）
LOG_FILE   <- paste0(TAG, "_log.txt")
# CKPT_FILE と RESULT_FILE は INIT_MODE 確定後に組み立てる（下の「1. 準備」）

## --- 実行する段階 ----------------------------------------------------------
RUN_CHECK <- TRUE     # 2. 旧解との照合。安いので必ず通すこと
RUN_OPTIM <- TRUE     # 3. BFGS 参照解。OPTIM_FILE があればそれを読む
RUN_SGD   <- TRUE     # 4. Adam-SGD
RESUME    <- TRUE     # CKPT_FILE があれば続きから

# TRUE にすると実データの代わりに小さな合成データを使う。
# クマデータが無い環境でも、全体の流れ（座標変換・チェックポイント・再開・
# 要約）が動くことを数分で確かめられる。**本番では必ず FALSE。**
DRY_RUN <- FALSE

# 照合で対数尤度がこれ以上ずれたら止める。
# 座標変換は厳密なので、ずれたらデータかモデルが当時と違うということ。
CHECK_TOL <- 0.1

# BFGS の最大反復数。変換した旧解から出発するのですぐ終わるはず。
OPTIM_MAXIT <- 200

## --- Adam ------------------------------------------------------------------
# 到達目標の総反復数。再開すればこの数まで回すので、**先に決め打ちしなくてよい。**
# まず 20 で回して1反復あたりの時間を見てから増やすこと（冒頭の「推奨手順」）。
MAX_ITER        <- 20

# 1.0 で単回捕獲個体も全件使う（＝完全バッチ）。下げても速くならないので
# このままでよい。理由は冒頭の「推奨手順」を参照。
SAMPLING_RATE   <- 1.0
CHECKPOINT_EVERY<- 5       # 何反復ごとに保存するか。1反復が長いので短めに
REPORT_EVERY    <- 1

BETA1 <- 0.9; BETA2 <- 0.999; EPS_ADAM <- 1e-8
GRAD_EPS <- 1e-4

# 共変量のスケールが揃ったので、係数ごとに変える必要がなくなった。
# 旧コードは conn_wtr だけ桁が違ったので手当てが要ったが、その理由が消えた。
ALPHA_DEFAULT    <- 0.02
MAX_STEP_DEFAULT <- 0.05
ALPHA_BY_PAR     <- c(conn_0 = 0.03)   # 切片だけ少し大きく（旧設定を踏襲）
MAX_STEP_BY_PAR  <- c(conn_0 = 0.10)

## --- 初期値 ---------------------------------------------------------------
#
#   "transformed" … 旧解を変換した点（＝既に最適解）。**まずはこちら。**
#                   序盤に大きな勾配が出ないので Adam の v が膨らまず、
#                   旧実行で起きた終盤の失速を避けられる。
#                   ただし「1反復で終わる」わけではない。Adam のステップは
#                   alpha * m/sqrt(v) で、最適点でも m/sqrt(v) は 1 程度に
#                   正規化されるため、alpha (0.02〜0.03) 程度の幅で
#                   解の周りを揺れ動く。20〜30 反復回して、その揺れが
#                   小さく収まっているかを見るのが目的。
#
#   "far"         … 遠い点から出発する。収束の実証用（旧コードと同じ置き方）。
#                   g0_1 が -5 から -1.79 まで動く必要があり、1反復あたり
#                   高々 alpha=0.02 なので 230 反復程度かかる。
#
# チェックポイントと結果のファイル名にこの値が入るので、両方を並存させられる。
INIT_MODE <- "transformed"

INIT_FAR <- c(dens_0 = -1, conn_0 = -2, conn_agri = 0, conn_wtr = 0, g0_1 = -5)

## --- 旧解（生スケール wtr のときの optim 解） ------------------------------
# SGD_Adam_result_20260904.Rdata の secrad_res から取った値。
# 対数尤度は -685.5202223（optim は loglfscale=-1 で最小化したので $value の符号を反転）。
OLD_PAR <- c(dens_0    = -1.450490709,
             conn_0    =  0.5579097412,
             conn_agri =  0.3612219522,
             conn_wtr  = -6.410855799,
             g0_1      = -1.791229872)
OLD_LOGLIK <- -685.5202223


# ===========================================================================
# 1. 準備
# ===========================================================================

t_start <- Sys.time()

stopifnot(INIT_MODE %in% c("far", "transformed"))

# DRY_RUN の上書きは sink より前に済ませる。出力ファイル名を別にして
# 本番の結果とログを上書きしないため。
if (DRY_RUN) {
  # 旧解は実データのものなので合成データには当てはまらない。照合は無効にし、
  # 反復数も切り詰める。
  RUN_CHECK <- FALSE
  MAX_ITER  <- 6L
  CHECKPOINT_EVERY <- 2L
  OPTIM_MAXIT <- 10L
  TAG <- paste0(TAG, "_dryrun")
  OPTIM_FILE <- paste0(TAG, "_optim.rds")
  LOG_FILE   <- paste0(TAG, "_log.txt")
}

CKPT_FILE   <- paste0(TAG, "_", INIT_MODE, "_checkpoint.rds")
RESULT_FILE <- paste0(TAG, "_", INIT_MODE, "_result.RData")

# 画面と LOG_FILE の両方に出す。数日かかるので記録が残らないと追えない。
# sink が拾うのは標準出力だけ。警告とエラーは端末にしか出ないので、
# 全部を残したいときは Rscript SGD_bear_20260909.R 2>&1 | tee run.log のように叩く。
sink(LOG_FILE, split = TRUE)

say <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

say("== ", TAG, " ==")
say("作業ディレクトリ: ", getwd())
if (DRY_RUN) say("★ DRY_RUN。合成データで流れだけを確認する。本番では DRY_RUN <- FALSE。")

for (p in c(SOURCEPATH, UTILPATH)) {
  if (!file.exists(p))
    stop(p, " が見つかりません。作業ディレクトリがリポジトリルート",
         "（banding_data）か確認してください。現在: ", getwd())
}
if (!DRY_RUN && !dir.exists(BEAR_DIR))
  stop("クマデータが見つかりません: ", BEAR_DIR,
       "\nBEAR_DIR を環境に合わせて書き換えてください。",
       "\n（手元にデータが無いまま流れだけ確認したいなら DRY_RUN <- TRUE）")

if (!DRY_RUN) suppressPackageStartupMessages(library(sf))
stopifnot(requireNamespace("numDeriv", quietly = TRUE))

say("secrad.r を読み込み中（C++ のコンパイルで30秒ほど）...")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
source(UTILPATH, encoding = "UTF-8")
stopifnot("sgd" %in% names(formals(secrad$public_methods$loglf)))
say("読み込み完了")


# ===========================================================================
# 2. データと共変量  ── ここが修正箇所
# ===========================================================================

say("== データ ==")

## --- 生データの取得 -------------------------------------------------------
# ここを抜けた時点で、以降の共通処理に必要な変数がすべて揃っている:
#   coords / area / resolution / grid_cov_raw / effort_vec / effort_occ_vec
#   / effort_loc / detect_mat

if (!DRY_RUN) {

  effort   <- read.csv(file.path(BEAR_DIR, "effort_231225.csv"))
  detect   <- read.csv(file.path(BEAR_DIR, "detectmat_231225.csv"))
  griddata <- st_read(BEAR_DIR, "meshutm_0.5km_buff_land", quiet = TRUE)

  coords <- cbind(x = griddata$x, y = griddata$y) / 1000     # m -> km
  area   <- griddata$area / 1e6                              # m^2 -> km^2

  # 格子間隔。旧コードは 8497 x 8497 の総当たり行列を2つ作っていたが
  # （それだけで 1.1GB）、一意な座標の差の最小値と同じ値になる。
  dx <- min(diff(sort(unique(coords[, "x"]))))
  dy <- min(diff(sort(unique(coords[, "y"]))))
  resolution <- c(x = dx, y = dy)

  grid_cov_raw <- data.frame(agri = griddata$agri_mean,
                             wtr  = griddata$wtr_mean)

  # 検出努力の位置
  effort_st <- st_as_sf(effort, coords = c("x", "y"), crs = 3100)
  hit <- st_intersects(effort_st, griddata)

  # 旧コードは unlist するだけだった。どの努力点もどのセルとも交わらないと
  # 黙って要素数が減り、effort と effort_loc の対応がずれる。ここで止める。
  if (any(lengths(hit) != 1L))
    stop("検出努力の点がセルにちょうど1つ対応していません（0個または2個以上）: ",
         sum(lengths(hit) != 1L), " 点。CRS とメッシュを確認してください。")
  effort_loc <- unlist(hit)

  effort_vec     <- effort$effort
  effort_occ_vec <- effort$effort_occ
  detect_mat <- as.matrix(detect[, -1])       # 1列目は ID
  storage.mode(detect_mat) <- "numeric"

} else {

  # 合成データ。実データと同じ列名（agri / wtr）にしておくと、以降の共通処理も
  # モデル式も本番とまったく同じ経路を通る。
  # wtr の sd をわざと小さくして、実データのスケール差を再現している。
  nx <- 14; ncell0 <- nx * nx
  xs <- rep(1:nx, each = nx); ys <- rep(1:nx, nx)
  coords <- cbind(x = xs, y = ys)
  area <- rep(1, ncell0)
  resolution <- c(x = 1, y = 1)
  set.seed(20260909)
  grid_cov_raw <- data.frame(
    agri = pmax(0, 0.13 + 0.27 * sin(xs / 2) * cos(ys / 3)),
    wtr  = pmax(0, 0.014 + 0.072 * cos(xs / 3) * sin(ys / 2)))

  tr <- unique(round(seq(2, nx - 1, length.out = 5)))
  effort_loc <- match(paste(rep(tr, length(tr)), rep(tr, each = length(tr))),
                      paste(xs, ys))
  effort_vec     <- rep(200, length(effort_loc))
  effort_occ_vec <- rep(1, length(effort_loc))

  # 検出をシミュレートする。標準化した共変量で回すので、本番と同じ土俵になる。
  tmp_cov <- as.data.frame(scale(grid_cov_raw))
  sim <- secrad_data$new(coords = coords, area = area,
                         grid_cov = tmp_cov, resolution = resolution)
  sim$add_obs(type = "poisson", effort = effort_vec,
              effort_loc = effort_loc, effort_occ = effort_occ_vec)
  sim$set_truemodel(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 1),
                    indmodel = c(A = FALSE, g0 = FALSE),
                    occmodel = c(A = FALSE, g0 = FALSE),
                    dpar = 0.3, cpar = c(-1.0, 0.35, -0.30),
                    adpar = -0.5, g0par = -3.2)
  sim$set_sim(200, 1, 100, 20)
  sim$generate_ind(); sim$simulate(verbose = FALSE); sim$update_detect()
  detect_mat <- as.matrix(sim$obs[[1]]$detect)
  rm(sim, tmp_cov)
}

ncell <- nrow(coords)

## --- ★ 標準化 ------------------------------------------------------------
# 旧コードはここで agri を2回書いて wtr を素通りさせていた。両方を標準化する。

COV_MU <- vapply(grid_cov_raw, mean, numeric(1))
COV_SD <- vapply(grid_cov_raw, sd,   numeric(1))

grid_cov <- data.frame(agri = (grid_cov_raw$agri - COV_MU["agri"]) / COV_SD["agri"],
                       wtr  = (grid_cov_raw$wtr  - COV_MU["wtr"])  / COV_SD["wtr"])

# 修正が効いていることをここで断言しておく。
# 片方だけ標準化されている状態を二度と通さないための番人。
for (v in c("agri", "wtr")) {
  stopifnot(abs(mean(grid_cov[[v]]))     < 1e-8,
            abs(sd(grid_cov[[v]]) - 1)   < 1e-8)
}

say("ncell = ", ncell, " / resolution = (",
    signif(resolution["x"], 6), ", ", signif(resolution["y"], 6), ")")
say("標準化前 mean/sd : agri ", sprintf("%.5f / %.5f", COV_MU["agri"], COV_SD["agri"]),
    " , wtr ",              sprintf("%.5f / %.5f", COV_MU["wtr"],  COV_SD["wtr"]))
say("標準化後は両方とも mean 0 / sd 1（検証済み）")
say("sd(wtr)/sd(agri) = ", sprintf("%.4f", COV_SD["wtr"] / COV_SD["agri"]),
    " … この比が旧コードで conn_wtr だけ桁が違っていた理由。")

# detect は 行 = 検出努力 / 列 = 個体。行数が努力数と一致することを確かめる。
# 転置して渡すと nind が検出器数に化けるが、エラーにはならず黙って進む。
stopifnot(nrow(detect_mat) == length(effort_loc),
          length(effort_vec) == length(effort_loc),
          length(effort_occ_vec) == length(effort_loc))

secrdata <- secrad_data$new(coords = coords, area = area,
                            grid_cov = grid_cov, resolution = resolution)
secrdata$add_obs(type = "poisson",
                 effort = effort_vec,
                 effort_loc = effort_loc,
                 effort_occ = effort_occ_vec,
                 detect = detect_mat)

capture_counts <- colSums(detect_mat)
multi_ids  <- which(capture_counts > 1)
single_ids <- which(capture_counts == 1)
n_detected <- secrdata$nind

say("検出個体 ", n_detected, "（複数回 ", length(multi_ids),
    " / 単回 ", length(single_ids), "） / 検出努力 ", length(effort_loc))

MODEL <- list(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

secrad_obj <- secrad$new(secrdata = secrdata)
secrad_obj$set_model(envmodel = MODEL$envmodel,
                     indmodel = MODEL$indmodel,
                     occmodel = MODEL$occmodel)

PAR_NAMES <- names(generate_init(secrad_obj))
say("パラメータ: ", paste(PAR_NAMES, collapse = ", "))
stopifnot(identical(PAR_NAMES, names(OLD_PAR)))


# ===========================================================================
# 3. 座標変換と検算
# ===========================================================================
#
# 旧: C = b0 + b_agri * (agri-mu_a)/sd_a + b_wtr * wtr        （wtr は生）
# 新: C = c0 + c_agri * (agri-mu_a)/sd_a + c_wtr * (wtr-mu_w)/sd_w
#
# 同じ関数になる条件は
#   c_wtr  = b_wtr * sd_w
#   c0     = b0 + b_wtr * mu_w
#   c_agri = b_agri
# 他の係数（dens_0, g0_1）は連結性と独立なので変わらない。
#
# つまりこの修正はモデルを変えない厳密な再パラメータ化。
# したがって変換した点での対数尤度は旧解のそれと一致しなければならない。

to_std <- function(p) {                     # 旧（wtr 生）-> 新（wtr 標準化）
  q <- p
  q["conn_wtr"] <- p["conn_wtr"] * COV_SD["wtr"]
  q["conn_0"]   <- p["conn_0"] + p["conn_wtr"] * COV_MU["wtr"]
  q
}

to_raw <- function(q) {                     # 新 -> 生スケール（両共変量とも生）
  b_agri <- q["conn_agri"] / COV_SD["agri"]
  b_wtr  <- q["conn_wtr"]  / COV_SD["wtr"]
  c(dens_0    = unname(q["dens_0"]),
    conn_0    = unname(q["conn_0"] - q["conn_agri"] * COV_MU["agri"] / COV_SD["agri"]
                                   - q["conn_wtr"]  * COV_MU["wtr"]  / COV_SD["wtr"]),
    conn_agri = unname(b_agri),
    conn_wtr  = unname(b_wtr),
    g0_1      = unname(q["g0_1"]))
}

OLD_PAR_STD <- to_std(OLD_PAR)

say("== 旧解の座標変換 ==")
print(round(rbind(`旧（wtr 生スケール）` = OLD_PAR,
                  `新（wtr 標準化）`     = OLD_PAR_STD), 6))
say("conn_wtr が ", sprintf("%.4f -> %.4f", OLD_PAR["conn_wtr"], OLD_PAR_STD["conn_wtr"]),
    " になり、conn_agri (", sprintf("%.4f", OLD_PAR["conn_agri"]), ") と同程度になる。")
say("これが Adam を苦しめていたスケール差の正体。")

if (RUN_CHECK) {
  say("変換した点で対数尤度を評価中（1回で数分かかる）...")
  t0 <- Sys.time()
  ll_check <- secrad_obj$loglf(OLD_PAR_STD)
  sec_per_loglf <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  d <- ll_check - OLD_LOGLIK
  say(sprintf("loglf = %.6f / 旧記録 = %.6f / 差 = %.3e （%.1f秒）",
              ll_check, OLD_LOGLIK, d, sec_per_loglf))

  if (abs(d) > CHECK_TOL) {
    stop("対数尤度が旧解と一致しません（差 ", signif(d, 4), "）。\n",
         "座標変換は厳密なので、一致しないならデータかモデル設定が当時と違います。\n",
         "数日かかる計算に入る前にここで確認してください。\n",
         "（意図的に先へ進めたい場合は CHECK_TOL を大きくする）")
  }
  say("★ 一致。標準化の修正は厳密な再パラメータ化であり、モデルは変わっていない。")
} else {
  sec_per_loglf <- NA_real_
  say("検算をスキップ（RUN_CHECK = FALSE）")
}


# ===========================================================================
# 4. BFGS 参照解
# ===========================================================================
#
# 変換した旧解は既に最適点のはずなので、そこから始めればすぐ終わる。
# 遠い点から始めると数日かかるので、そうしない。

if (RUN_OPTIM) {
  if (file.exists(OPTIM_FILE)) {
    secrad_res <- readRDS(OPTIM_FILE)
    say("== BFGS 参照解を ", OPTIM_FILE, " から読み込み ==")
  } else {
    say("== BFGS 参照解を計算中 ==")
    say("変換した旧解から出発するので、反復数は少ないはず。")
    t0 <- Sys.time()
    secrad_res <- optim(OLD_PAR_STD, secrad_obj$loglf, method = "BFGS",
                        control = list(maxit = OPTIM_MAXIT, trace = 1, REPORT = 1),
                        loglfscale = -1, hessian = TRUE)
    say(sprintf("%.1f分 / convergence = %d / logL = %.6f",
                as.numeric(difftime(Sys.time(), t0, units = "mins")),
                secrad_res$convergence, -secrad_res$value))
    saveRDS(secrad_res, OPTIM_FILE)
    say("保存: ", OPTIM_FILE)
  }
  ref_par <- secrad_res$par
  names(ref_par) <- PAR_NAMES
  print(round(rbind(`変換した旧解` = OLD_PAR_STD, `BFGS 解` = ref_par), 6))
} else {
  secrad_res <- NULL
  ref_par <- OLD_PAR_STD          # 参照解として変換した旧解を使う
  say("optim をスキップ（RUN_OPTIM = FALSE）。参照解には変換した旧解を使う。")
}


# ===========================================================================
# 5. Adam-SGD
# ===========================================================================

## --- 部分集合と目的関数 ---------------------------------------------------

make_subset <- function(ids) {
  o <- secrdata$obs[[1]]
  s <- secrad_data$new(coords = secrdata$coords, area = secrdata$area,
                       grid_cov = secrdata$grid_cov, resolution = secrdata$resolution)
  s$add_obs(type = o$type, effort = o$effort, effort_loc = o$effort_loc,
            effort_occ = o$effort_occ,
            detect = as.matrix(o$detect[, ids, drop = FALSE]))
  s$ind_cov <- if (is.null(secrdata$ind_cov)) rep(1, length(ids))
               else secrdata$ind_cov[ids]
  obj <- secrad$new(secrdata = s)
  obj$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
                occmodel = MODEL$occmodel)
  obj
}

## 複数回捕獲個体は毎反復すべて使い、単回捕獲個体だけをサンプリングして
## 1/rate で重みを戻す。これで全データ尤度の不偏推定になる。
sgd_loglik <- function(p, obj_multi, obj_single, rate) {
  om <- obj_multi$loglf(p,  loglfscale = 1, sgd = TRUE)
  os <- obj_single$loglf(p, loglfscale = 1, sgd = TRUE)
  sum(dpois(n_detected, lambda = exp(om$lambda_grp), log = TRUE)) +
    om$loglfmulti + os$loglfmulti / rate
}

if (RUN_SGD) {

  say("== Adam-SGD ==")

  obj_multi <- make_subset(multi_ids)

  # advdiff の結果は格子とパラメータだけで決まり、どの個体を持っているかに
  # 依存しない。multi 側と single 側に同じキャッシュを指させると、
  # ncell x ncell の計算が1反復あたり 10回 -> 4回 になる（tests/cache_bench.R）。
  adcache <- share_advdiff_cache(list(obj_multi))

  full_sampling <- isTRUE(all.equal(SAMPLING_RATE, 1.0))
  sample_size   <- max(1, floor(length(single_ids) * SAMPLING_RATE))

  obj_single_all <- NULL
  if (full_sampling) {
    obj_single_all <- make_subset(single_ids)
    share_advdiff_cache(list(obj_single_all), cache = adcache)
  }

  draw_single <- function() {
    if (full_sampling) return(obj_single_all)
    o <- make_subset(sample(single_ids, min(sample_size, length(single_ids))))
    share_advdiff_cache(list(o), cache = adcache)   # 作り直すたびに繋ぎ直す
    o
  }

  by_par <- function(default, overrides) {
    v <- setNames(rep(default, length(PAR_NAMES)), PAR_NAMES)
    hit <- intersect(names(overrides), PAR_NAMES)
    v[hit] <- overrides[hit]
    v
  }
  alpha_vec <- by_par(ALPHA_DEFAULT,    ALPHA_BY_PAR)
  max_step  <- by_par(MAX_STEP_DEFAULT, MAX_STEP_BY_PAR)

  ## --- 状態の初期化または再開 --------------------------------------------

  init_par <- switch(INIT_MODE,
    far         = INIT_FAR[PAR_NAMES],
    transformed = OLD_PAR_STD,
    stop("INIT_MODE は \"far\" か \"transformed\"")
  )

  trace_par  <- matrix(NA_real_, MAX_ITER, length(PAR_NAMES),
                       dimnames = list(NULL, PAR_NAMES))
  trace_grad <- trace_step <- trace_par
  trace_ll   <- rep(NA_real_, MAX_ITER)

  current_par <- init_par
  m <- v <- setNames(rep(0, length(PAR_NAMES)), PAR_NAMES)
  iter_done <- 0L

  if (RESUME && file.exists(CKPT_FILE)) {
    ck <- readRDS(CKPT_FILE)
    if (!identical(ck$par_names, PAR_NAMES))
      stop("チェックポイントのパラメータ名が一致しません。CKPT_FILE を消すか確認を。")
    # ファイル名に INIT_MODE が入っているので普通は起きないが、念のため。
    if (!is.null(ck$init_mode) && !identical(ck$init_mode, INIT_MODE))
      stop("チェックポイントの INIT_MODE (", ck$init_mode, ") が現在の設定 (",
           INIT_MODE, ") と違います。別の初期値の途中経過から再開しかけています。")
    current_par <- ck$current_par; m <- ck$m; v <- ck$v
    iter_done <- ck$iter_done
    n <- min(iter_done, MAX_ITER)
    trace_par[seq_len(n), ]  <- ck$trace_par[seq_len(n), , drop = FALSE]
    trace_grad[seq_len(n), ] <- ck$trace_grad[seq_len(n), , drop = FALSE]
    trace_step[seq_len(n), ] <- ck$trace_step[seq_len(n), , drop = FALSE]
    trace_ll[seq_len(n)]     <- ck$trace_ll[seq_len(n)]
    say("チェックポイントから再開: iter ", iter_done, " まで完了済み")
  } else {
    say("初期値 (INIT_MODE = ", INIT_MODE, "):")
    print(round(current_par, 5))
  }

  say("sampling_rate = ", SAMPLING_RATE, " / alpha = ",
      paste(sprintf("%s:%.3f", PAR_NAMES, alpha_vec), collapse = " "))
  say("max_step = ", paste(sprintf("%s:%.3f", PAR_NAMES, max_step), collapse = " "))

  save_ckpt <- function(iter) {
    saveRDS(list(par_names = PAR_NAMES, init_mode = INIT_MODE,
                 current_par = current_par, m = m, v = v,
                 iter_done = iter, trace_par = trace_par, trace_grad = trace_grad,
                 trace_step = trace_step, trace_ll = trace_ll,
                 sampling_rate = SAMPLING_RATE, alpha_vec = alpha_vec,
                 max_step = max_step, ref_par = ref_par),
            CKPT_FILE)
  }

  ## --- 反復 ---------------------------------------------------------------

  # seq(iter_done+1, MAX_ITER) は既に到達済みのとき降順の列になってしまう。
  # 空の列を明示的に作る。
  iters <- if (iter_done < MAX_ITER) (iter_done + 1L):MAX_ITER else integer(0)
  if (length(iters) == 0L)
    say("MAX_ITER (", MAX_ITER, ") に到達済み。追加で回すなら MAX_ITER を増やすこと。")

  t_sgd <- Sys.time()
  for (iter in iters) {
    t_iter <- Sys.time()

    obj_single <- draw_single()
    objfun <- function(p) { names(p) <- PAR_NAMES
      sgd_loglik(p, obj_multi, obj_single, SAMPLING_RATE) }

    # numDeriv::grad(method="simple") と同じ前進差分。cpar に効かない係数を
    # 先に揺らすので advdiff のキャッシュが効く。値は完全に一致する。
    g <- tryCatch(grad_cachewise(objfun, current_par, eps = GRAD_EPS),
                  error = function(e) { say("勾配でエラー: ", conditionMessage(e))
                                        rep(NA_real_, length(current_par)) })
    if (!all(is.finite(g))) {
      say("勾配が NA/Inf。iter ", iter, " で停止。チェックポイントは残っている。")
      break
    }

    m <- BETA1 * m + (1 - BETA1) * g
    v <- BETA2 * v + (1 - BETA2) * g^2
    step <- alpha_vec * (m / (1 - BETA1^iter)) / (sqrt(v / (1 - BETA2^iter)) + EPS_ADAM)
    step <- pmax(pmin(step, max_step), -max_step)
    current_par <- current_par + step          # 最大化なので +

    trace_par[iter, ]  <- current_par
    trace_grad[iter, ] <- g
    trace_step[iter, ] <- step
    trace_ll[iter]     <- objfun(current_par)
    iter_done <- iter

    sec <- as.numeric(difftime(Sys.time(), t_iter, units = "secs"))
    if (iter %% REPORT_EVERY == 0L) {
      say(sprintf("iter %4d/%d  ll %.6f  max|g| %.3e  max|step| %.3e  %.0f秒  残り約%.1f時間",
                  iter, MAX_ITER, trace_ll[iter], max(abs(g)), max(abs(step)),
                  sec, sec * (MAX_ITER - iter) / 3600))
      print(round(rbind(current = current_par, ref = ref_par,
                        diff = current_par - ref_par), 5))
    }
    if (iter %% CHECKPOINT_EVERY == 0L) { save_ckpt(iter); say("  チェックポイント保存") }
  }
  save_ckpt(iter_done)
  time_adam_sgd <- as.numeric(difftime(Sys.time(), t_sgd, units = "secs"))
  say(sprintf("Adam-SGD 終了: %d 反復 / %.1f 時間", iter_done, time_adam_sgd / 3600))

  final_par <- current_par

} else {
  say("Adam-SGD をスキップ（RUN_SGD = FALSE）")
  final_par <- NULL; time_adam_sgd <- NA_real_; iter_done <- 0L
  trace_par <- trace_grad <- trace_step <- NULL; trace_ll <- NULL
  alpha_vec <- max_step <- NULL; sample_size <- NA_integer_
}


# ===========================================================================
# 6. 要約
# ===========================================================================

say("== 結果 ==")

if (!is.null(final_par)) {
  cmp <- rbind(`BFGS（参照解）` = ref_par,
               `Adam-SGD`       = final_par,
               `差`             = final_par - ref_par)
  print(round(cmp, 5))
  say(sprintf("最大絶対誤差 vs 参照解: %.5f", max(abs(final_par - ref_par))))

  say("生スケールへ逆変換（agri も wtr も生の値に対する係数）:")
  print(round(rbind(`BFGS`     = to_raw(ref_par),
                    `Adam-SGD` = to_raw(final_par),
                    `旧解`     = to_raw(OLD_PAR_STD)), 5))
  say("旧解の行の conn_wtr は ", sprintf("%.4f", OLD_PAR["conn_wtr"]),
      " に一致するはず（旧モデルでは wtr が生スケールだったため）。")
  say("conn_agri は旧モデルでも標準化済みだったので、旧解の行の値は ",
      sprintf("%.4f", OLD_PAR["conn_agri"] / COV_SD["agri"]),
      " になる（元の表示 ", sprintf("%.4f", OLD_PAR["conn_agri"]), " とは基底が違う）。")
}


save(secrad_res, ref_par, final_par, trace_par, trace_ll, trace_grad, trace_step,
     iter_done, time_adam_sgd, alpha_vec, max_step, sample_size,
     SAMPLING_RATE, BETA1, BETA2, EPS_ADAM, GRAD_EPS, INIT_MODE,
     COV_MU, COV_SD, OLD_PAR, OLD_PAR_STD, OLD_LOGLIK,
     multi_ids, single_ids, n_detected,
     file = RESULT_FILE)
say("保存: ", RESULT_FILE)
say(sprintf("全体 %.1f 時間", as.numeric(difftime(Sys.time(), t_start, units = "hours"))))

sink()
