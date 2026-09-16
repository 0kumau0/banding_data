# examples/verify_reference.R ------------------------------------------------
#
# **保存した実行の「BFGS 参照解」が本当に最大点かを確かめる。**
#
#   Rscript examples/verify_reference.R results/sgd_sim_XXXX.RData
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-17）
#
# この track の判定は一貫して「BFGS 解と一致するか」で行ってきた。
# 同じ尤度を別の方法で最大化しているのだから一致すべき、という理屈で、
# これ自体は正しい。**ただし BFGS が最大点に着いていれば、の話。**
#
# 2026-09-17 の実行（results/log_Brate_b90.txt）で、5条件すべての Adam が
# **BFGS より対数尤度の高い点**に着いた（-91.15 対 -93.31）。
# BFGS は conn_agri = 9.26（真値 0.6）へ走り去り、20反復で convergence = 0 を
# 返している。**参照解のほうが間違っていた。**
#
# このスクリプトは、保存されたデータから推定オブジェクトを組み直し、
#
#   1. 参照解と各条件の最終点で、全データの対数尤度を測り直す
#   2. **いちばん良い Adam の点から BFGS を出発させる**
#
# を行う。2 でそこに留まれば、「BFGS が遠方からでは届かなかった」ことの実証になる。
#
# 読むだけで、保存ファイルは書き換えない。実データも要らない。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("使い方: Rscript examples/verify_reference.R <results/....RData>")
RESULTFILE <- args[1]
if (!file.exists(RESULTFILE)) stop("見つかりません: ", RESULTFILE)

SOURCEPATH <- "adcrsgd/secrad.r"
PAR_NAMES  <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")
OPTIM_MAXIT <- 1000

cat("== 読み込み ==\n", RESULTFILE, "\n", sep = "")
e <- new.env()
load(RESULTFILE, envir = e)

cat("  検証台 ", e$TESTBED, " / ", e$SWEEP_NAME, "\n", sep = "")
if (nzchar(e$COND_SPEC)) cat("  条件: ", e$COND_SPEC, "\n", sep = "")

suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))

# --- 保存された部品から推定オブジェクトを組み直す --------------------------
# 保存しているのはデータ本体（detect・共変量・座標・検出器の位置）なので、
# 生成時と同じ手順で組み直せる。乱数は使わないので再現は厳密。
ntrap <- length(e$effort_loc)

simdata <- secrad_data$new(
  coords     = cbind(x = e$xcoord, y = e$ycoord),
  area       = rep(e$CELL_AREA, e$ncell),
  grid_cov   = e$grid_cov,
  resolution = c(x = e$CELL_SIZE, y = e$CELL_SIZE))

simdata$add_obs(type       = "poisson",
                effort     = rep(e$EFFORT, ntrap),
                effort_loc = e$effort_loc,
                effort_occ = rep(e$N_OCCASION, ntrap),
                detect     = e$detect)

obj <- secrad$new(secrdata = simdata)
obj$set_model(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

ll <- function(p) obj$loglf(setNames(as.numeric(p), PAR_NAMES), loglfscale = 1)

# --- 1. いまある点を測り直す ------------------------------------------------
cat("\n== 1. 全データでの対数尤度 ==\n")

pts <- list()
if (!is.null(e$ref_par)) pts[["BFGS（参照解）"]] <- e$ref_par[PAR_NAMES]
for (nm in names(e$runs)) pts[[nm]] <- e$runs[[nm]]$final_par[PAR_NAMES]

lls <- sapply(pts, ll)
ord <- order(lls, decreasing = TRUE)
tab <- data.frame(logL = round(lls, 4),
                  `参照解との差` = round(lls - lls[["BFGS（参照解）"]], 4),
                  check.names = FALSE)
print(tab[ord, , drop = FALSE])

best <- names(pts)[which.max(lls)]
cat("\n最良: ", best, "（logL ", sprintf("%.4f", max(lls)), "）\n", sep = "")

if (best == "BFGS（参照解）") {
  cat("**参照解が最良。判定の前提は保たれている。**\n")
} else {
  cat("**参照解より良い点がある。参照解は最大点ではない。**\n")
  cat("  差 ", sprintf("%.4f", max(lls) - lls[["BFGS（参照解）"]]),
      " 対数尤度単位ぶん、Adam のほうが高い。\n", sep = "")
}

# --- 2. 最良点から BFGS を出発させる ---------------------------------------
# ここに留まるなら、遠方から出発した BFGS が届かなかっただけ、と確定する。
cat("\n== 2. 最良点から BFGS を再出発 ==\n")
t0 <- Sys.time()
res2 <- optim(pts[[best]], obj$loglf, method = "BFGS",
              control = list(maxit = OPTIM_MAXIT, trace = 1, REPORT = 10),
              loglfscale = -1, hessian = FALSE)
cat(sprintf("  %.1f分 / convergence = %d / logL = %.6f\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")),
            res2$convergence, -res2$value))
print(round(setNames(res2$par, PAR_NAMES), 5))

moved <- max(abs(res2$par - pts[[best]]))
cat(sprintf("\n出発点からの最大移動: %.6f\n", moved))
cat(sprintf("対数尤度の改善: %.6f\n", -res2$value - max(lls)))

cat("\n== 判定 ==\n")
if (moved < 0.01) {
  cat("**その場に留まった。** Adam の着地点は BFGS にとっても停留点であり、\n")
  cat("遠方から出発した BFGS が届かなかっただけだと確定する。\n")
} else {
  cat("**さらに動いた。** Adam の着地点も最大点ではない。\n")
  cat("上の par が現時点で分かっている最良の点。\n")
}
if (!is.null(e$true_par)) {
  cat("\n参考（真値との比較。最適化の成否ではなく推定量の性能）:\n")
  print(round(rbind(`再出発後` = setNames(res2$par, PAR_NAMES),
                    `真値`     = e$true_par[PAR_NAMES]), 5))
}
