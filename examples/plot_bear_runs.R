# examples/plot_bear_runs.R --------------------------------------------------
#
# **クマ実データの Adam-SGD 実行を、参照解と標準誤差の物差しで描く。**
#
#   Rscript examples/plot_bear_runs.R <result.RData> [<result.RData> ...]
#
# 既定では b90（alpha x1）と b90a4（alpha x4）を並べる。
#
# 読むだけ。実データは要らない（trace と保存済みのヘッセ行列しか見ない）。
#
# ---------------------------------------------------------------------------
# なぜ標準誤差で割って描くのか（2026-09-28）
#
# 係数の生の差を見ても「近い」かどうか判断できない。conn_agri の 0.01 と
# g0_1 の 0.01 は意味が違う（標準誤差が 0.351 対 0.102）。
# **データが決められる精度より細かく最適化しても意味がない**ので、
# 標準誤差が唯一の実務的な物差しになる。
#
# **trace_ll は描かない。** あれは SGD の目的関数 sgd_loglik() で、
# 参照解の secrad_obj$loglf() とは別の関数（定数ぶんずれる）。
# 2026-09-28 に、この2つを引き算した数字で誤った結論を出しかけた。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
files <- if (length(args)) args else
  c("SGD_bear_20260909_far_b90_result.RData",
    "results/SGD_bear_20260909_far_b90a4_result.RData")
files <- files[file.exists(files)]
if (!length(files)) stop("result ファイルが見つかりません")

PAR_NAMES <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")
## SGD_bear_20260909.R と同じ定義（result に alpha は保存されていない）
ALPHA_BASE <- c(dens_0 = 0.02, conn_0 = 0.03, conn_agri = 0.02,
                conn_wtr = 0.02, g0_1 = 0.02)
INIT_FAR <- c(dens_0 = -1, conn_0 = -2, conn_agri = 0, conn_wtr = 0, g0_1 = -5)
OUT0 <- "reports/figures/bear-fig-traces-raw.png"   # 生の係数（従来の見せ方）
OUT1 <- "reports/figures/bear-fig-traces.png"       # 標準誤差の単位
OUT2 <- "reports/figures/bear-fig-approach.png"     # 歩幅の比
COLS <- c("#a8402c", "#1f6f80", "#557a4b", "#a8761f")

runs <- lapply(files, function(f) {
  e <- new.env(); load(f, envir = e)
  n <- e$iter_done
  se <- if (!is.null(e$secrad_res$hessian))
          tryCatch(sqrt(diag(solve(e$secrad_res$hessian))), error = function(x) NULL)
        else NULL
  ## result ファイルでは設定は大文字の変数名で保存されている
  ## （小文字の tag / alpha_mult / beta2 はチェックポイント側の名前）
  list(file = f, tag = e$RUN_TAG, mult = e$ALPHA_MULT, beta2 = e$BETA2, n = n,
       P = e$trace_par[seq_len(n), , drop = FALSE],
       S = e$trace_step[seq_len(n), , drop = FALSE],
       ref = setNames(as.numeric(e$ref_par)[seq_along(PAR_NAMES)], PAR_NAMES),
       se = if (!is.null(se)) setNames(se, PAR_NAMES) else NULL,
       alpha = ALPHA_BASE * e$ALPHA_MULT)
})
names(runs) <- sapply(runs, function(r) sprintf("%s (alpha x%g)", r$tag, r$mult))
SE <- Filter(Negate(is.null), lapply(runs, `[[`, "se"))[[1]]
REF <- runs[[1]]$ref
NMAX <- max(sapply(runs, `[[`, "n"))

cat("== 描画 ==\n")
first_below <- function(dm, thr) { i <- which(dm < thr); if (length(i)) i[1] else NA_integer_ }
for (k in seq_along(runs)) {
  r  <- runs[[k]]
  dm <- apply(abs(sweep(r$P, 2, REF, "-")), 1, function(v) max(v / SE))
  cat(sprintf("  %-22s beta2=%.3f / %d 反復 / 最終 %.4f SE\n",
              names(runs)[k], r$beta2, r$n, dm[r$n]))
  cat(sprintf("      1 SE を切る: %s 反復 / 0.1 SE: %s / 0.05 SE: %s\n",
              first_below(dm, 1), first_below(dm, 0.1), first_below(dm, 0.05)))
}

dir.create("reports/figures", showWarnings = FALSE, recursive = TRUE)

## --- 図0: 生の係数（従来の見せ方）-------------------------------------------
##
## **標準誤差の単位（図1）と併記する。** 両方に役割がある:
##   生の値 … 係数が実際にどの値を取ったか。解釈に使うのはこちら
##   SE 単位 … 到達したかどうかの判定。係数ごとにスケールが違うので、
##             生の値のままでは「近い」かを判断できない
## 6枚目は SGD の目的関数。**参照解の対数尤度とは別の関数なので基準線は引かない**
## （引き算に意味が無い。2026-09-28 にこれで誤った結論を出しかけた）。
png(OUT0, width = 1200, height = 780, res = 115)
op <- par(mfrow = c(2, 3), mar = c(4.2, 4.4, 3, 1))
for (j in seq_along(PAR_NAMES)) {
  nm <- PAR_NAMES[j]
  yr <- range(unlist(lapply(runs, function(r) r$P[, j])), REF[nm], INIT_FAR[nm])
  plot(NA, xlim = c(1, NMAX), ylim = yr, xlab = "Iteration",
       ylab = "Estimate", main = nm)
  abline(h = REF[nm], col = "red", lwd = 2, lty = 2)
  for (k in seq_along(runs))
    lines(seq_len(runs[[k]]$n), runs[[k]]$P[, j], col = COLS[k], lwd = 1.8)
  if (j == 1) legend("bottomright", legend = c(names(runs), "reference (BFGS)"),
                     col = c(COLS[seq_along(runs)], "red"),
                     lty = c(rep(1, length(runs)), 2),
                     lwd = 1.8, bty = "n", cex = 0.75)
}
LL <- lapply(runs, function(r) { e <- new.env(); load(r$file, envir = e)
                                 e$trace_ll[seq_len(r$n)] })
plot(NA, xlim = c(1, NMAX), ylim = range(unlist(LL), na.rm = TRUE),
     xlab = "Iteration", ylab = "SGD objective",
     main = "SGD objective (not the reference scale)")
for (k in seq_along(runs)) lines(seq_len(runs[[k]]$n), LL[[k]], col = COLS[k], lwd = 1.8)
mtext("sgd_loglik(), offset from loglf() by a constant", side = 3, line = 0.1,
      cex = 0.65, col = "grey30")
par(op); dev.off()
cat("図0: ", OUT0, "\n", sep = "")

## --- 図1: 係数の推移。参照解を 0 に、縦軸を標準誤差の単位にする -------------
## 生の値ではなく「参照解から標準誤差いくつ離れているか」を描く。
## 係数ごとにスケールが違うので、生の値だと比較できない。
png(OUT1, width = 1200, height = 780, res = 115)
op <- par(mfrow = c(2, 3), mar = c(4.2, 4.4, 3, 1))
for (j in seq_along(PAR_NAMES)) {
  nm <- PAR_NAMES[j]
  ys <- lapply(runs, function(r) (r$P[, j] - REF[nm]) / SE[nm])
  yr <- range(unlist(ys), 0, na.rm = TRUE)
  plot(NA, xlim = c(1, NMAX), ylim = yr, xlab = "Iteration",
       ylab = "(estimate - reference) / SE", main = nm)
  ## データが決められる精度の帯
  rect(-10, -1, NMAX * 2, 1, col = "#00000010", border = NA)
  abline(h = 0, col = "red", lwd = 2, lty = 2)
  for (k in seq_along(runs)) lines(seq_len(runs[[k]]$n), ys[[k]],
                                   col = COLS[k], lwd = 1.8)
  if (j == 1) legend("topleft", legend = names(runs), col = COLS[seq_along(runs)],
                     lwd = 1.8, bty = "n", cex = 0.8)
}
## 6枚目: 全係数の最大距離。これが到達の判定そのもの
plot(NA, xlim = c(1, NMAX), ylim = c(5e-4, 30), log = "y",
     xlab = "Iteration", ylab = "max |estimate - reference| / SE",
     main = "Distance from the reference")
for (h in c(1, 0.1, 0.01)) abline(h = h, col = "grey70", lty = 3)
text(NMAX, c(1, 0.1, 0.01), c("1 SE", "0.1 SE", "0.01 SE"),
     adj = c(1, -0.3), cex = 0.7, col = "grey40")
for (k in seq_along(runs)) {
  r <- runs[[k]]
  dm <- apply(abs(sweep(r$P, 2, REF, "-")), 1, function(v) max(v / SE))
  lines(seq_len(r$n), dm, col = COLS[k], lwd = 2)
}
par(op); dev.off()
cat("図1: ", OUT1, "\n", sep = "")

## --- 図2: 歩幅の比。失速と到達の区別 ----------------------------------------
## |step|/alpha は Adam の m/sqrt(v) にあたる。
## **小さいこと自体は失敗を意味しない**（到達すれば勾配の符号が振れて m -> 0）。
## 失速との区別は、係数がまだ遠いかどうかで付ける（図1）。
png(OUT2, width = 1100, height = 440, res = 115)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.4, 3, 1))
for (k in seq_along(runs)) {
  r <- runs[[k]]
  ratio <- abs(r$S) / matrix(r$alpha, r$n, length(PAR_NAMES), byrow = TRUE)
  plot(NA, xlim = c(1, r$n), ylim = c(1e-3, 1.5), log = "y",
       xlab = "Iteration", ylab = "|step| / alpha",
       main = sprintf("%s", names(runs)[k]))
  abline(h = 1, col = "grey70", lty = 3)
  for (j in seq_along(PAR_NAMES))
    lines(seq_len(r$n), pmax(ratio[, j], 1e-3), col = COLS[j], lwd = 1.2)
  if (k == 1) legend("bottomleft", legend = PAR_NAMES, col = COLS, lwd = 1.2,
                     bty = "n", cex = 0.7, ncol = 2)
}
par(op); dev.off()
cat("図2: ", OUT2, "\n", sep = "")
