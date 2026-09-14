# examples/freeze_compare.R --------------------------------------------------
#
# 合成データの設定比較（examples/adam_settings.R）が、なぜクマ実データの
# 失速を予測できなかったのかを、両方のトレースを並べて確かめる。
#
#   Rscript examples/freeze_compare.R
#
# 読むだけ。実データ不要（クマ側も trace しか見ない）。
#
# ---------------------------------------------------------------------------
# 問い（2026-09-14）
#
# adam_settings.R は far と同じ初期値 (-1,-2,0,0,-5) で beta2 とリセットを
# 比べており、「beta2=0.99 + alpha x2.5 が最良」という結論を出していた。
# ところが週末の実データ実行（A2 / B4）は、どちらも 200 反復で参照解に届かず、
# |step|/alpha が 1/20 〜 1/10000 まで落ちて凍結した。
#
# **合成データで見えていた失速と、実データの凍結は、同じ現象の同じ強さだったのか。**
#
# Adam のステップは alpha * m_hat / sqrt(v_hat) なので、|step|/alpha が
# そのまま m/sqrt(v) にあたる。v は序盤の大きな勾配を beta2 の記憶時間
# （0.999 なら約1000反復）だけ覚えている。したがって凍結の強さを決めるのは
#
#     序盤の |g| / 終盤の |g|
#
# この比が大きいほど深く凍る。合成データでこの比が小さければ、
# 「同じ設定で同じ結論が出る」とは限らない。
# ---------------------------------------------------------------------------

ALPHA_SYN  <- c(dens_0 = 0.02, conn_0 = 0.03, conn_X1 = 0.03, conn_X2 = 0.03, g0_1 = 0.02)
ALPHA_BEAR <- c(dens_0 = 0.02, conn_0 = 0.03, conn_agri = 0.02, conn_wtr = 0.02, g0_1 = 0.02)

## adam_settings.R の CONDITIONS から alpha_mult を引くための対応表
SYN_MULT <- c(base = 1.0, b2_099 = 1.0, b2_090 = 1.0,
              reset100 = 1.0, alpha_hi = 2.5, b2_099_hi = 2.5)

hr <- function(ch = "-") cat(strrep(ch, 78), "\n")

## 共通の要約。G/S は 反復 x 係数 の行列、alpha は係数ごとのベクトル。
summarise <- function(label, G, S, err, alpha) {
  n <- nrow(G)
  gmax <- apply(abs(G), 1, max)
  ratio <- sweep(abs(S), 2, alpha, "/")
  rmax  <- apply(ratio, 1, max)
  q <- unique(round(seq(1, n, length.out = 5)))
  data.frame(
    label      = label,
    反復       = n,
    `初期 max|g|` = round(gmax[1], 1),
    `終盤 max|g|` = round(median(tail(gmax, 10)), 2),
    `勾配の減衰比` = round(gmax[1] / median(tail(gmax, 10)), 1),
    `step/alpha 序盤` = round(rmax[q[2]], 4),
    `step/alpha 終盤` = round(median(tail(rmax, 10)), 5),
    `最終誤差` = round(err[n], 4),
    check.names = FALSE)
}

rows <- list()

## --- 合成データ（adam_settings.R の各条件）---------------------------------
for (lab in names(SYN_MULT)) {
  f <- sprintf("results/adam_settings_trace_%s.csv", lab)
  if (!file.exists(f)) { cat("無し: ", f, "\n", sep = ""); next }
  d <- read.csv(f)
  nm <- names(ALPHA_SYN)
  G <- as.matrix(d[, paste0("grad_", nm)])
  S <- as.matrix(d[, paste0("step_", nm)])
  colnames(G) <- colnames(S) <- nm
  rows[[length(rows) + 1L]] <-
    summarise(paste0("合成/", lab), G, S, d$err, ALPHA_SYN * SYN_MULT[[lab]])
}

## --- クマ実データ（週末の far 実行）----------------------------------------
for (f in list.files(".", pattern = "^SGD_bear_.*_far_.*_result\\.RData$")) {
  e <- new.env(); load(f, envir = e)
  n  <- e$iter_done
  nm <- colnames(e$trace_par)
  G  <- e$trace_grad[seq_len(n), , drop = FALSE]
  S  <- e$trace_step[seq_len(n), , drop = FALSE]
  P  <- e$trace_par [seq_len(n), , drop = FALSE]
  err <- apply(P, 1, function(r) max(abs(r - e$ref_par[nm])))
  mult <- if (is.null(e$ALPHA_MULT)) 1 else e$ALPHA_MULT
  rows[[length(rows) + 1L]] <-
    summarise(paste0("クマ/", e$RUN_TAG), G, S, err, ALPHA_BEAR[nm] * mult)
}

if (!length(rows)) stop("比較できるトレースがありません。")
res <- do.call(rbind, rows)

hr("=")
cat("Adam の凍結の強さ — 合成データ と クマ実データ\n")
hr("=")
print(res, row.names = FALSE)

hr()
cat("## 読み方\n")
cat("  「勾配の減衰比」= 初期 max|g| / 終盤 max|g|。\n")
cat("  v は序盤の勾配を覚えているので、この比が大きいほど step が深く潰れる。\n")
cat("  「step/alpha 終盤」が 1 から遠いほど凍結している。\n")

syn  <- res[grepl("^合成/", res$label), ]
bear <- res[grepl("^クマ/", res$label), ]
if (nrow(syn) && nrow(bear)) {
  hr()
  cat(sprintf("  合成(base)   : 減衰比 %.1f / step比 %.5f\n",
              syn$`勾配の減衰比`[syn$label == "合成/base"],
              syn$`step/alpha 終盤`[syn$label == "合成/base"]))
  for (i in seq_len(nrow(bear)))
    cat(sprintf("  %-12s : 減衰比 %.1f / step比 %.5f\n",
                bear$label[i], bear$`勾配の減衰比`[i], bear$`step/alpha 終盤`[i]))
}

dir.create("results", showWarnings = FALSE)
write.csv(res, "results/freeze_compare.csv", row.names = FALSE)
cat("\n保存: results/freeze_compare.csv\n")
