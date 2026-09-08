# tests/smoke_test.R ---------------------------------------------------------
#
# 実データを使わない動作確認。編集機（kumada 機）で、別PCへ送る前に走らせる。
#
#   Rscript tests/smoke_test.R
#
# 作業ディレクトリはリポジトリルート（git/banding_data）であること。
# secrad_data$simulate() で小さな合成データを作るので、外部データは一切不要。
# 所要時間の大半は secrad.r の C++ コンパイル（約26秒）。
#
# 確認すること:
#   1. secrad.r が source できる（C++ のコンパイルを含む）
#   2. 合成データで識別可能な捕獲履歴が生成される
#   3. loglf(par) が有限のスカラーを返す（sgd=FALSE の従来動作＝後方互換）
#   4. loglf(par, sgd=TRUE) が $res / $lambda_grp / $loglfmulti を持つリストを返す
#   5. loglf(sgd=TRUE)$res が loglf() のスカラーと一致する（改変が既存経路を壊していない）
#   6. create_subset_secrad() が個体（列）で切り出している
#   7. wrapper_sgd_loglf_fast() が有限値を返す
# ---------------------------------------------------------------------------

t_start <- Sys.time()

# --- ごく小さなテスト枠組み -------------------------------------------------

.results <- list()

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    .results[[length(.results) + 1L]] <<- list(label = label, ok = FALSE,
                                               msg = conditionMessage(e))
    NULL
  })
  if (is.null(ok)) return(invisible(FALSE))
  .results[[length(.results) + 1L]] <<- list(label = label, ok = ok, msg = "")
  invisible(ok)
}

# --- 設定 -------------------------------------------------------------------

SOURCEPATH  <- "adcrsgd/secrad.r"      # リポジトリ相対。sourcepath 付け替え後の正式な場所
WRAPPERPATH <- "wrapper_20260302.R"

set.seed(20260908)

nx <- 10; ny <- 10                      # 100セル。小さいほど速い
ncell <- nx * ny
xcoord <- rep(1:nx, each = ny)
ycoord <- rep(1:ny, nx)

tr <- unique(round(seq(2, nx - 1, length.out = 3)))   # 3x3 = 9 検出器
trapx <- rep(tr, length(tr))
trapy <- rep(tr, each = length(tr))

nt <- 200                               # 1 occasion あたりの時間 = 努力量

# シミュレーションの刻み。advdiff_t_core は陽的な時間発展なので、
# 拡散係数 exp(logc) に対して dt = difft/stepad が大きすぎると
# CFL 条件を破って「Negative probability produced」で落ちる。
# ここでは burn-in 20 に対して stepad=100（dt=0.2）と十分細かく取る。
stepspertime <- 1
stepad       <- 100
timeburnin   <- 20

# 真のパラメータ。connectivity を低め（exp(-1)≒0.37）に抑えて
# 小さいグリッドでも数値的に安定させている。
dpar_true  <- 0.0                       # exp(0)*100 ≒ 100個体
cpar_true  <- c(-1.0, 0.3)              # C ~ X
adpar_true <- -0.5
g0par_true <- -4

# --- 1. secrad.r を source --------------------------------------------------

cat("[1/7] source(", SOURCEPATH, ") ... ", sep = "")

if (!file.exists(SOURCEPATH)) {
  stop("secrad.r が見つかりません: ", SOURCEPATH,
       "\n作業ディレクトリがリポジトリルートか確認してください。現在: ", getwd())
}

t0 <- Sys.time()
source(SOURCEPATH, encoding = "UTF-8")
cat(sprintf("%.1f秒\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

check("secrad クラスが定義されている",       inherits(secrad, "R6ClassGenerator"))
check("secrad_data クラスが定義されている",  inherits(secrad_data, "R6ClassGenerator"))
check("generate_init が定義されている",      is.function(generate_init))
check("loglf が sgd 引数を持つ",
      "sgd" %in% names(formals(secrad$public_methods$loglf)))

# --- 2. 合成データの生成 ----------------------------------------------------

cat("[2/7] 合成データを生成 ... ")

effort_loc <- integer(length(trapx))
for (i in seq_along(trapx)) {
  effort_loc[i] <- which(xcoord == trapx[i] & ycoord == trapy[i])
}
ntrap <- length(effort_loc)

# 連続的な景観共変量（flsgen は使わない）
X <- as.numeric(scale(sin(xcoord / 2) + cos(ycoord / 2)))

simdata <- secrad_data$new(coords     = cbind(x = xcoord, y = ycoord),
                           area       = rep(1, ncell),
                           grid_cov   = data.frame(X = X),
                           resolution = c(x = 1, y = 1))

simdata$add_obs(type       = "poisson",
                effort     = rep(nt, ntrap),
                effort_loc = effort_loc,
                effort_occ = rep(1, ntrap))

simdata$set_truemodel(envmodel = list(D ~ 1, C ~ X, A ~ 1),
                      indmodel = c(A = FALSE, g0 = FALSE),
                      occmodel = c(A = FALSE, g0 = FALSE),
                      dpar     = dpar_true,
                      cpar     = cpar_true,
                      adpar    = adpar_true,
                      g0par    = g0par_true)

simdata$set_sim(nt, stepspertime, stepad, timeburnin)
simdata$generate_ind()
simdata$simulate(return = FALSE, verbose = FALSE)
simdata$update_detect()

detect <- simdata$obs[[1]]$detect
ncap   <- colSums(detect)               # 個体ごとの検出回数（列 = 個体）

cat(sprintf("%.1f秒 / 全個体 %d, 検出個体 %d, 複数回検出 %d\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")),
            nrow(simdata$trueind), simdata$nind, sum(ncap > 1)))

check("検出された個体がいる",                simdata$nind > 0)
check("detect の行数が検出器数と一致（行=努力）", nrow(detect) == ntrap)
check("detect の列数が個体数と一致（列=個体）",   ncol(detect) == simdata$nind)
check("複数回検出された個体がいる（識別可能性）", sum(ncap > 1) >= 2)
check("単回検出の個体がいる（multi/single 分割が意味を持つ）", sum(ncap == 1) >= 1)

# --- 3〜5. loglf の2つの経路 ------------------------------------------------

cat("[3/7] loglf(sgd=FALSE) ... ")

obj <- secrad$new(secrdata = simdata)
obj$set_model(envmodel = list(D ~ 1, C ~ X, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

par0 <- generate_init(obj)
par0["dens_0"] <- dpar_true
par0["conn_0"] <- cpar_true[1]
if ("conn_X" %in% names(par0)) par0["conn_X"] <- cpar_true[2]
if ("g0_1"   %in% names(par0)) par0["g0_1"]   <- g0par_true

t0 <- Sys.time()
ll_scalar <- obj$loglf(par0)
cat(sprintf("%.1f秒 / loglf = %.6f\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), ll_scalar))

check("loglf() が長さ1の数値を返す",
      is.numeric(ll_scalar) && length(ll_scalar) == 1L)
check("loglf() が有限値を返す", is.finite(ll_scalar))

cat("[4/7] loglf(sgd=TRUE) ... ")
ll_list <- obj$loglf(par0, sgd = TRUE)
cat("done\n")

check("loglf(sgd=TRUE) がリストを返す", is.list(ll_list))
check("戻り値に res / lambda_grp / loglfmulti が揃っている",
      all(c("res", "lambda_grp", "loglfmulti") %in% names(ll_list)))
check("$loglfmulti が有限値", is.finite(ll_list$loglfmulti))
check("$lambda_grp が有限値", all(is.finite(ll_list$lambda_grp)))

cat("[5/7] 後方互換の回帰テスト ... ")
same <- isTRUE(all.equal(ll_list$res, ll_scalar, tolerance = 1e-10))
cat(if (same) "一致\n" else "★不一致\n")
check("loglf(sgd=TRUE)$res が loglf() のスカラーと一致する", same)

# --- 6〜7. wrapper 側の関数 -------------------------------------------------
#
# wrapper_20260302.R はトップレベルでデータ読み込みと optim まで走る「スクリプト」
# なので、そのまま source すると実データを要求して落ちる。
# ここでは関数定義（`名前 <- function(...)`）だけを抜き出して評価する。
# 将来この関数群を別ファイル（例 R/wrapper_sgd.R）に分離すれば、この細工は不要になる。

source_functions_only <- function(path, envir = parent.frame()) {
  exprs <- parse(path, encoding = "UTF-8")
  n <- 0L
  for (e in exprs) {
    if (is.call(e) &&
        as.character(e[[1]])[1] %in% c("<-", "=") &&
        is.call(e[[3]]) &&
        identical(as.character(e[[3]][[1]])[1], "function")) {
      eval(e, envir = envir)
      n <- n + 1L
    }
  }
  n
}

cat("[6/7] create_subset_secrad ... ")

if (file.exists(WRAPPERPATH)) {
  nfun <- source_functions_only(WRAPPERPATH)
  cat(sprintf("(%s から関数 %d 個を取り込み) ", WRAPPERPATH, nfun))

  model_settings <- list(envmodel = list(D ~ 1, C ~ X, A ~ 0),
                         indmodel = c(A = FALSE, g0 = FALSE),
                         occmodel = c(A = FALSE, g0 = FALSE))

  multi_ids  <- which(ncap > 1)
  single_ids <- which(ncap == 1)

  check("create_subset_secrad が取り込めている", exists("create_subset_secrad"))

  if (exists("create_subset_secrad")) {
    sub_multi  <- create_subset_secrad(simdata, multi_ids,  model_settings)
    sub_single <- create_subset_secrad(simdata, single_ids, model_settings)
    cat("done\n")

    # ids は個体番号。detect は 行=努力・列=個体 なので、列で切り出すのが正しい。
    # 行で切ってしまうと nind が検出器数に化ける。これを検出するテスト。
    check("部分集合の個体数が length(multi_ids) と一致（列で切り出している）",
          sub_multi$secrdata$nind == length(multi_ids))
    check("部分集合の個体数が length(single_ids) と一致",
          sub_single$secrdata$nind == length(single_ids))
    check("部分集合の detect 行数が元と同じ（努力は削られていない）",
          nrow(sub_multi$secrdata$obs[[1]]$detect) == ntrap)
  } else {
    cat("skip\n")
  }

  cat("[7/7] wrapper_sgd_loglf_fast ... ")
  if (exists("wrapper_sgd_loglf_fast") && exists("create_subset_secrad")) {
    ll_wrap <- wrapper_sgd_loglf_fast(par         = par0,
                                      obj_multi   = sub_multi,
                                      obj_single  = sub_single,
                                      n_detected  = simdata$nind,
                                      sampling_rate = 1.0)
    cat(sprintf("%.6f\n", ll_wrap))
    check("wrapper_sgd_loglf_fast が長さ1の数値を返す",
          is.numeric(ll_wrap) && length(ll_wrap) == 1L)
    check("wrapper_sgd_loglf_fast が有限値を返す", is.finite(ll_wrap))
  } else {
    cat("skip（wrapper_sgd_loglf_fast が見つからない）\n")
    check("wrapper_sgd_loglf_fast が取り込めている", exists("wrapper_sgd_loglf_fast"))
  }

} else {
  cat("skip（", WRAPPERPATH, " が見つからない）\n", sep = "")
}

# --- 結果 -------------------------------------------------------------------

cat("\n", strrep("-", 66), "\n", sep = "")

nfail <- 0L
for (r in .results) {
  mark <- if (r$ok) "  OK  " else " FAIL "
  cat(mark, r$label, if (nzchar(r$msg)) paste0("  << ", r$msg) else "", "\n", sep = "")
  if (!r$ok) nfail <- nfail + 1L
}

cat(strrep("-", 66), "\n", sep = "")
cat(sprintf("%d 件中 %d 件成功  （所要 %.1f秒）\n",
            length(.results), length(.results) - nfail,
            as.numeric(difftime(Sys.time(), t_start, units = "secs"))))

if (nfail > 0L) {
  cat("\n★ 失敗があります。別PCへ push する前に解消してください。\n")
  quit(status = 1L)
} else {
  cat("\nすべて成功。別PCへ push して問題ありません。\n")
  quit(status = 0L)
}
