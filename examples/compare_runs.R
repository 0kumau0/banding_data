# examples/compare_runs.R ----------------------------------------------------
#
# sgd_simulation.R が残した結果ファイルを並べて比べる。
#
#   Rscript examples/compare_runs.R                       # results/ の全部
#   Rscript examples/compare_runs.R results/a.RData results/b.RData
#
# **保存済みのオブジェクトを読むだけ。** secrad.r も実データも要らないので数秒で終わる。
# 実行中の別の解析を邪魔しない。
#
# ---------------------------------------------------------------------------
# 何のために使うか
#
# このシミュレーションは set.seed を固定しても再現しない（C++ 側が
# std::random_device で独自に乱数を初期化するため）。したがって
# **同じ設定で走らせた2つの実行は、別のデータに対する独立な試行**になる。
#
# それを並べると、
#
#   - 失速がデータ固有の偶然なのか、設定から来る系統的なものなのか
#   - BFGS 解のばらつき（＝推定量のばらつき）がどの程度か
#
# が分かる。別PCの結果を持ち帰ったときの読み方でもある。
# ---------------------------------------------------------------------------

args  <- commandArgs(trailingOnly = TRUE)
files <- if (length(args)) args else
  sort(list.files("results", pattern = "^sgd_simulation_.*\\.RData$", full.names = TRUE))

if (!length(files)) {
  cat("結果ファイルが見つかりません。作業ディレクトリ: ", getwd(), "\n", sep = "")
  quit(status = 0)
}

hr <- function(ch = "-") cat(strrep(ch, 78), "\n")
g0 <- function(x, d = NA) if (is.null(x)) d else x

rows <- list()

for (f in files) {
  hr("=")
  e <- new.env(); load(f, envir = e)
  cat(basename(f), "  (更新 ", format(file.mtime(f), "%Y-%m-%d %H:%M"), ")\n\n", sep = "")

  nm <- names(e$true_par)
  n  <- g0(e$iter_done, 0L)

  ## --- データの規模 ---------------------------------------------------------
  nmulti  <- length(g0(e$multi_ids,  integer(0)))
  nsingle <- length(g0(e$single_ids, integer(0)))
  cat(sprintf("データ   : 検出 %d（複数回 %d / 単回 %d = 1 : %.1f）/ ncell %s\n",
              g0(e$n_detected, NA), nmulti, nsingle,
              nsingle / max(1L, nmulti), g0(e$ncell, "?")))
  cat(sprintf("真の設定 : dens %s / g0 %s / conn %s\n",
              g0(e$TRUE_DENS, "?"), g0(e$TRUE_G0, "?"),
              paste(g0(e$TRUE_CONN, "?"), collapse = ",")))

  ## --- 推定値 ---------------------------------------------------------------
  tab <- rbind(`真値` = e$true_par[nm])
  if (!is.null(e$final_par)) tab <- rbind(tab, `Adam-SGD` = e$final_par[nm])
  if (!is.null(e$ref_par))   tab <- rbind(tab, `BFGS` = e$ref_par[nm])
  se <- NULL
  if (!is.null(e$secrad_res$hessian))
    se <- tryCatch(sqrt(diag(solve(e$secrad_res$hessian))), error = function(x) NULL)
  if (!is.null(se)) tab <- rbind(tab, `標準誤差` = se)
  cat("\n")
  print(round(tab, 4))

  ## --- 到達度 ---------------------------------------------------------------
  maxerr <- if (!is.null(e$ref_par) && !is.null(e$final_par))
              max(abs(e$final_par[nm] - e$ref_par[nm])) else NA_real_
  llgap  <- if (!is.null(e$secrad_res) && n >= 1L)
              (-e$secrad_res$value) - e$trace_ll[n] else NA_real_
  dse <- NA_real_
  if (!is.null(se) && n >= 1L) {
    rem <- as.vector(solve(e$secrad_res$hessian) %*% e$trace_grad[n, ])
    dse <- max(abs(rem / se))
  }

  ## --- 終盤の診断（trace から計算し直す）------------------------------------
  sign_rate <- travel <- rep(NA_real_, length(nm))
  if (n >= 5L) {
    w   <- min(20L, n)
    idx <- seq.int(n - w + 1L, n)
    sign_rate <- apply(e$trace_grad[idx, , drop = FALSE], 2,
                       function(g) max(mean(g > 0), mean(g < 0)))
    travel <- abs(e$trace_par[n, ] - e$trace_par[idx[1L], ]) / (g0(e$ALPHA, 1) * (w - 1L))
  }

  cat(sprintf("\n反復 %d / 最大誤差 vs BFGS %.4f / logL 差 %.3f / 最大 距離/SE %.3f\n",
              n, maxerr, llgap, dse))
  cat("終盤の符号一致率 :",
      paste(sprintf("%s %3.0f%%", nm, 100 * sign_rate), collapse = "  "), "\n")
  cat("終盤の正味移動/α :",
      paste(sprintf("%s %.2f", nm, travel), collapse = "  "), "\n")

  pushed <- any(sign_rate > 0.8, na.rm = TRUE)
  moving <- any(travel    > 0.3, na.rm = TRUE)
  verdict <- if (pushed && moving)        "進行中"
             else if (pushed && !moving)  "★失速"
             else if (!pushed && !moving) "振動（到達の可能性）"
             else                         "過渡的"
  cat("判定 :", verdict, "\n")

  rows[[length(rows) + 1L]] <- data.frame(
    file = basename(f), 反復 = n,
    検出 = g0(e$n_detected, NA), 複数回 = nmulti, 単回 = nsingle,
    `最大誤差` = maxerr, `logL差` = llgap, `距離/SE` = dse, 判定 = verdict,
    check.names = FALSE, stringsAsFactors = FALSE)
}

hr("=")
cat("まとめ\n")
hr("=")
res <- do.call(rbind, rows)
print(format(res, digits = 4), row.names = FALSE)

cat("\n＊ 同じ設定の実行が複数あるなら、それらは**別データに対する独立な試行**。\n")
cat("  判定が揃うなら、失速はデータ固有ではなく設定から来る系統的なもの。\n")
