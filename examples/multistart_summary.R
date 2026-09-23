# examples/multistart_summary.R ----------------------------------------------
#
# examples/multistart_study.R が残した CSV を要約する。
#
#   Rscript examples/multistart_summary.R [results/multistart_study.csv]
#
# 読むだけ。実データも要らない。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
CSV  <- if (length(args)) args[1] else "results/multistart_study.csv"
if (!file.exists(CSV)) stop("見つかりません: ", CSV)

d <- read.csv(CSV, stringsAsFactors = FALSE)
cat("== ", CSV, " ==\n", sep = "")
cat(sprintf("%d 件 / 合計 %.1f 時間\n\n", nrow(d), sum(d$mins) / 60))

## --- データの規模 -----------------------------------------------------------
cat("-- 水準ごとのデータの姿 --\n")
agg <- aggregate(cbind(n_detected, n_multi, n_single, det_per_ind, max_se) ~ level, d, mean)
agg <- agg[match(c("dense", "mid", "sparse"), agg$level), ]
agg$n <- as.integer(table(d$level)[agg$level])
print(format(agg[, c("level", "n", "n_detected", "n_multi", "n_single",
                     "det_per_ind", "max_se")], digits = 3), row.names = FALSE)

## --- 本題: 既定の初期値が最良を逃す頻度 -------------------------------------
cat("\n-- 単一の初期値（遠方の既定値）が最良の峰を逃した頻度 --\n")
d$miss <- !d$far_is_best
tab <- do.call(rbind, lapply(c("dense", "mid", "sparse"), function(lv) {
  s <- d[d$level == lv, ]
  data.frame(level = lv, n = nrow(s),
             `個体あたり検出` = mean(s$det_per_ind),
             `逃した件数` = sum(s$miss),
             `割合` = mean(s$miss),
             `峰が2つ以上` = mean(s$n_peak > 1),
             `遅れの最大` = max(s$far_gap),
             check.names = FALSE)
}))
print(format(tab, digits = 3), row.names = FALSE)

cat(sprintf("\n全体: %d / %d 件（%.1f%%）で既定の初期値が最良を逃した\n",
            sum(d$miss), nrow(d), 100 * mean(d$miss)))

## --- 逃した件の詳細 ---------------------------------------------------------
if (any(d$miss)) {
  cat("\n-- 逃した件 --\n")
  m <- d[d$miss, c("idx", "level", "n_detected", "n_multi", "det_per_ind",
                   "n_peak", "far_gap", "max_se")]
  print(format(m, digits = 4), row.names = FALSE)
  cat("\n遅れ（対数尤度）の分位点:\n")
  print(round(quantile(d$far_gap[d$miss], c(0, .25, .5, .75, 1)), 4))
}

## --- 峰の数の分布 -----------------------------------------------------------
cat("\n-- 見つかった峰の数（5点の BFGS から。logL を 0.01 で丸めて数える）--\n")
print(table(水準 = d$level, 峰の数 = d$n_peak))

## --- 面の平坦さとの関係 -----------------------------------------------------
cat("\n-- 最大標準誤差の分位点（面の平坦さ）--\n")
## tapply(..., quantile) はリストを返すので rbind でまとめる（round がリストで落ちる）
qtab <- function(x, g, probs) {
  l <- tapply(x, g, function(v) quantile(v, probs = probs, na.rm = TRUE))
  do.call(rbind, l[c("dense", "mid", "sparse")])
}
print(round(qtab(d$max_se, d$level, c(.5, .9, 1)), 3))
cat(sprintf("（標準誤差が計算できなかった件: %d）\n", sum(!is.finite(d$max_se))))
if (any(d$miss) && sum(!d$miss) > 0) {
  cat(sprintf("\n逃した件の最大SE 中央値 %.3f / 逃さなかった件 %.3f\n",
              median(d$max_se[d$miss], na.rm = TRUE),
              median(d$max_se[!d$miss], na.rm = TRUE)))
}

## --- Adam --------------------------------------------------------------------
a <- d[is.finite(d$adam_gap), ]
if (nrow(a)) {
  cat(sprintf("\n-- Adam（beta2=0.9 / alpha x1 / 400反復、%d 件で実施）--\n", nrow(a)))
  cat(sprintf("  BFGS 最良との差（正 = Adam が届いていない）:\n"))
  print(round(quantile(a$adam_gap, c(0, .25, .5, .75, .9, 1)), 4))
  cat(sprintf("  0.01 以内: %d/%d 件（%.0f%%）\n",
              sum(a$adam_gap < 0.01), nrow(a), 100 * mean(a$adam_gap < 0.01)))
  cat(sprintf("  **BFGS を上回った件**: %d\n", sum(a$adam_gap < -0.01)))
  cat("\n  水準ごとの中央値:\n")
  print(round(tapply(a$adam_gap, a$level, median), 4))
}

## --- 1件あたりの所要 ---------------------------------------------------------
cat("\n-- 1データセットあたりの所要（分）--\n")
print(round(qtab(d$mins, d$level, c(.5, .9, 1)), 1))

## --- 図 ---------------------------------------------------------------------
## 左: 既定初期値の遅れ（0 は下端に潰す）。右: 面の平坦さ。
## どちらも横軸は「1個体あたり検出数」＝データの薄さ。
PNG <- "reports/figures/ms-fig-miss.png"
if (dir.exists("reports/figures")) {
  cols <- c(dense = "#557a4b", mid = "#a8761f", sparse = "#a8402c")
  png(PNG, width = 1100, height = 470, res = 110)
  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

  FLOOR <- 1e-4
  y <- pmax(d$far_gap, FLOOR)
  plot(d$det_per_ind, y, log = "y", pch = 19, cex = 0.7,
       col = cols[d$level],
       xlab = "Detections per detected individual",
       ylab = "logL gap of the single far start",
       main = "How far the default start falls short")
  abline(h = 0.01, lty = 2, col = "grey40")
  text(max(d$det_per_ind), 0.012, "0.01 = agreement", adj = c(1, 0),
       cex = 0.75, col = "grey30")
  legend("topright", legend = names(cols), col = cols, pch = 19, bty = "n", cex = 0.85)
  mtext(sprintf("%d datasets; points on the floor line agree exactly", nrow(d)),
        side = 3, line = 0.1, cex = 0.72, col = "grey30")

  plot(d$det_per_ind, d$max_se, log = "y", pch = 19, cex = 0.7,
       col = ifelse(d$miss, "#a8402c", "grey70"),
       xlab = "Detections per detected individual",
       ylab = "Largest standard error at the best peak",
       main = "Flatness of the surface")
  legend("topright", legend = c("default start missed", "found the best"),
         col = c("#a8402c", "grey70"), pch = 19, bty = "n", cex = 0.85)
  par(op); dev.off()
  cat("\n図: ", PNG, "\n", sep = "")
}
