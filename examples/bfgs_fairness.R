# examples/bfgs_fairness.R ---------------------------------------------------
#
# **BFGS に最善を尽くさせたうえで、まだ Adam に届かないかを確かめる。**
#
#   Rscript examples/bfgs_fairness.R results/sgd_sim_XXXX.RData
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-17）
#
# 2026-09-17 の実行で、Adam 5条件すべてが BFGS 参照解より 2.16 対数尤度ぶん
# 高い点に着いた（results/log_Brate_b90.txt）。`verify_reference.R` で
# 「Adam の点は BFGS にとっても停留点」であることは確かめた。
#
# **しかしそれは「BFGS が遠方から届かなかった」ことを示すだけで、
# 「BFGS では届かない」を示していない。** 比較が公正だったかを疑う理由がある:
#
#   optim(method = "BFGS") は gr = NULL のとき数値勾配を自作する。
#   **その刻み幅の既定値は control$ndeps = 1e-3。**
#   我々の Adam は 1e-4 を使っている。**BFGS は10倍粗い勾配で戦っていた。**
#
# 平坦な尾根の上では、粗い差分の誤差が勾配の向きを壊す。20反復で
# convergence = 0 を返して停まったのは、線形探索が改善を見つけられなく
# なったためと読むのが自然で、**BFGS の限界ではなく設定の不備かもしれない。**
#
# **公正な比較とは、相手に最善を尽くさせること。**
# これをやらずに「Adam が BFGS に勝った」と書けば、査読で最初に潰される。
#
# 振るもの:
#   1. ndeps   … 数値勾配の刻み幅（1e-3 既定 / 1e-4 / 1e-5）
#   2. reltol  … 収束判定（既定 1e-8 / 1e-12）
#   3. 初期値  … 遠方の1点だけでなく、その周りに散らした複数点
#
# 読むだけで、保存ファイルは書き換えない。実データも要らない。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("使い方: Rscript examples/bfgs_fairness.R <results/....RData>")
RESULTFILE <- args[1]
if (!file.exists(RESULTFILE)) stop("見つかりません: ", RESULTFILE)

N_START    <- 3        # 多点出発で試す追加の初期値の数
START_SD   <- 0.5      # 初期値の散らし幅
SEED       <- 20260917
OPTIM_MAXIT <- 1000
SOURCEPATH <- "adcrsgd/secrad.r"
PAR_NAMES  <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")

cat("== 読み込み ==\n", RESULTFILE, "\n", sep = "")
e <- new.env()
load(RESULTFILE, envir = e)

suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))

## --- 保存された部品から推定オブジェクトを組み直す --------------------------
## （verify_reference.R と同じ手順。乱数を使わないので再現は厳密）
ntrap <- length(e$effort_loc)
simdata <- secrad_data$new(coords     = cbind(x = e$xcoord, y = e$ycoord),
                           area       = rep(e$CELL_AREA, e$ncell),
                           grid_cov   = e$grid_cov,
                           resolution = c(x = e$CELL_SIZE, y = e$CELL_SIZE))
simdata$add_obs(type = "poisson", effort = rep(e$EFFORT, ntrap),
                effort_loc = e$effort_loc, effort_occ = rep(e$N_OCCASION, ntrap),
                detect = e$detect)
obj <- secrad$new(secrdata = simdata)
obj$set_model(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

## --- 比較の基準: Adam が着いた最良点 ---------------------------------------
adam_ll  <- sapply(e$runs, function(r) r$ll_full)
adam_best <- names(which.max(adam_ll))
adam_par  <- e$runs[[adam_best]]$final_par[PAR_NAMES]
cat(sprintf("\nAdam の最良: %s  logL %.6f\n", adam_best, max(adam_ll)))
cat(sprintf("元の BFGS  : logL %.6f（差 %+.4f）\n",
            -e$secrad_res$value, -e$secrad_res$value - max(adam_ll)))

## --- BFGS を条件を変えて回す ------------------------------------------------
run_bfgs <- function(label, init, ndeps, reltol) {
  t0 <- Sys.time()
  ctrl <- list(maxit = OPTIM_MAXIT, ndeps = rep(ndeps, length(PAR_NAMES)),
               reltol = reltol)
  r <- tryCatch(optim(init, obj$loglf, method = "BFGS",
                      control = ctrl, loglfscale = -1, hessian = FALSE),
                error = function(err) NULL)
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  if (is.null(r)) {
    cat(sprintf("%-22s 失敗\n", label))
    return(NULL)
  }
  ll <- -r$value
  cat(sprintf("%-22s logL %12.6f  (Adam との差 %+8.4f)  conv %d  %5.1f分\n",
              label, ll, ll - max(adam_ll), r$convergence, mins))
  list(label = label, ll = ll, par = setNames(r$par, PAR_NAMES),
       convergence = r$convergence, mins = mins, ndeps = ndeps, reltol = reltol)
}

cat("\n== BFGS に条件を変えて挑ませる ==\n")
cat("（差が負なら BFGS が Adam に届いていない）\n\n")

res <- list()

## ① 既定の設定を再現する。元の実行と同じはず
res[["既定 ndeps=1e-3"]] <- run_bfgs("既定 ndeps=1e-3", e$INIT, 1e-3, 1e-8)

## ② 勾配の刻みを Adam と揃える。**これが本命の対抗仮説**
res[["ndeps=1e-4"]] <- run_bfgs("ndeps=1e-4", e$INIT, 1e-4, 1e-8)

## ③ さらに細かく
res[["ndeps=1e-5"]] <- run_bfgs("ndeps=1e-5", e$INIT, 1e-5, 1e-8)

## ④ 収束判定を締める
res[["ndeps=1e-4 reltol厳"]] <- run_bfgs("ndeps=1e-4 reltol厳", e$INIT, 1e-4, 1e-12)

## ⑤ 多点出発。1点からの失敗は「その点からは届かない」でしかない
set.seed(SEED)
for (k in seq_len(N_START)) {
  init_k <- e$INIT + rnorm(length(PAR_NAMES), 0, START_SD)
  names(init_k) <- PAR_NAMES
  res[[paste0("多点出発 #", k)]] <- run_bfgs(paste0("多点出発 #", k), init_k, 1e-4, 1e-8)
}

## --- 判定 -------------------------------------------------------------------
res <- Filter(Negate(is.null), res)
lls <- sapply(res, `[[`, "ll")
best <- names(which.max(lls))
gap  <- max(lls) - max(adam_ll)

cat("\n== 判定 ==\n")
cat(sprintf("BFGS の最良: %s  logL %.6f\n", best, max(lls)))
cat(sprintf("Adam の最良: %s  logL %.6f\n", adam_best, max(adam_ll)))
cat(sprintf("差: %+.6f\n\n", gap))

if (gap > 0.01) {
  cat("**条件を整えれば BFGS のほうが良い点に着く。**\n")
  cat("昨夜の『Adam が勝った』は設定の不備によるものだった。\n")
  cat("→ 主張を取り下げ、速度の議論に絞ること。\n")
} else if (gap > -0.01) {
  cat("**条件を整えれば BFGS も同じ点に着く。**\n")
  cat("『Adam でなければ届かない』とは言えない。穏当な主張に留めること。\n")
  cat("→ SGD の利点は速度と規模であって、到達性ではない。\n")
} else {
  cat("**条件を整えても BFGS は届かない。**\n")
  cat("どの ndeps・どの初期値でも Adam の点に及ばない。\n")
  cat("→ 到達性の主張が残る。ただし1データでの1例であることを忘れないこと。\n")
}

cat("\n-- 各条件の到達点 --\n")
tab <- do.call(rbind, lapply(res, function(r)
  data.frame(logL = round(r$ll, 6), `Adamとの差` = round(r$ll - max(adam_ll), 4),
             conv = r$convergence, 分 = round(r$mins, 1), check.names = FALSE)))
print(tab)

cat("\n-- 推定値の比較 --\n")
print(round(rbind(`Adam 最良` = adam_par,
                  `BFGS 最良` = res[[best]]$par[PAR_NAMES],
                  `元の BFGS` = e$ref_par[PAR_NAMES],
                  `真値`      = e$true_par[PAR_NAMES]), 5))
