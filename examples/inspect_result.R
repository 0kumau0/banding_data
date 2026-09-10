# examples/inspect_result.R --------------------------------------------------
#
# SGD_bear_*.R が残した結果ファイル・チェックポイントの中身を要約する。
# 別PCから持ち帰った .RData / .rds を、こちらで開いて状態を確認するための道具。
#
#   Rscript examples/inspect_result.R                      # 見つかったもの全部
#   Rscript examples/inspect_result.R <ファイル> [<ファイル> ...]
#
# 読むだけで、何も書き換えない。実データも要らない
# （結果ファイルにはパラメータの推移しか入っていない）。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

files <- if (length(args)) args else
  c(list.files(".", pattern = "^SGD_bear_.*_result\\.RData$"),
    list.files(".", pattern = "^SGD_bear_.*_checkpoint\\.rds$"),
    list.files(".", pattern = "^SGD_bear_.*_optim\\.rds$"))

if (!length(files)) {
  cat("SGD_bear_* の結果ファイルが見つかりません。\n",
      "作業ディレクトリ: ", getwd(), "\n", sep = "")
  quit(status = 0)
}

hr <- function() cat(strrep("-", 72), "\n")

fmt_par <- function(x, digits = 6) {
  if (is.null(x)) return("(なし)")
  paste(sprintf("%s=%.*f", names(x), digits, x), collapse = "  ")
}

## 参照解との差を表で出す。SGD が「どこまで来たか」を見るための中心的な指標。
cmp_table <- function(cur, ref, label = "現在値") {
  if (is.null(cur) || is.null(ref)) return(invisible(NULL))
  nm <- names(ref)
  cur <- cur[nm]
  m <- rbind(cur, ref, cur - ref)
  rownames(m) <- c(label, "参照解", "差")
  print(round(m, 6))
  cat(sprintf("最大絶対誤差: %.6f\n", max(abs(cur - ref))))
}

for (f in files) {
  hr()
  if (!file.exists(f)) { cat("見つかりません: ", f, "\n", sep = ""); next }
  cat(f, "  (", format(round(file.size(f) / 1024), big.mark = ","), " KB, 更新 ",
      format(file.mtime(f), "%Y-%m-%d %H:%M"), ")\n\n", sep = "")

  if (grepl("_optim\\.rds$", f)) {
    o <- readRDS(f)
    cat("BFGS 参照解\n")
    cat("  convergence :", o$convergence, if (o$convergence == 0L) "(収束)" else "(未収束)", "\n")
    cat("  logL        :", sprintf("%.6f", -o$value), "\n")
    cat("  評価回数    :", paste(names(o$counts), o$counts, collapse = " / "), "\n")
    cat("  par         :", fmt_par(o$par), "\n")
    if (!is.null(o$hessian)) {
      # loglfscale=-1 で最小化しているので、o$hessian は -logL のヘッセ行列
      # ＝観測情報行列。その逆行列の対角の平方根が標準誤差。
      se <- tryCatch(sqrt(diag(solve(o$hessian))), error = function(e) NULL)
      if (!is.null(se) && all(is.finite(se))) {
        cat("  標準誤差    :", fmt_par(setNames(se, names(o$par)), 5), "\n")
        z <- o$par / se
        cat("  95%信頼区間 :\n")
        ci <- cbind(推定値 = o$par, SE = se,
                    下限 = o$par - 1.96 * se, 上限 = o$par + 1.96 * se)
        print(round(ci, 5))
      } else {
        cat("  標準誤差    : ヘッセ行列が反転できません（要確認）\n")
      }
    } else {
      cat("  ヘッセ行列  : なし（--no-hessian で実行された）\n")
    }
    next
  }

  if (grepl("_checkpoint\\.rds$", f)) {
    ck <- readRDS(f)
    cat("チェックポイント\n")
    cat("  完了した反復:", ck$iter_done, "\n")
    cat("  init_mode   :", if (is.null(ck$init_mode)) "(記録なし)" else ck$init_mode, "\n")
    cat("  tag         :", if (is.null(ck$run_tag) || !nzchar(ck$run_tag)) "(なし)" else ck$run_tag, "\n")
    cat("  alpha_mult  :", if (is.null(ck$alpha_mult)) "(記録なし)" else ck$alpha_mult, "\n")
    cat("  beta2       :", if (is.null(ck$beta2)) "(記録なし)" else ck$beta2, "\n")
    cat("  sampling_rate:", ck$sampling_rate, "\n\n")
    cmp_table(ck$current_par, ck$ref_par)
    if (!is.null(ck$trace_par) && ck$iter_done >= 2L) {
      tp <- ck$trace_par[seq_len(ck$iter_done), , drop = FALSE]
      err <- apply(tp, 1, function(r) max(abs(r - ck$ref_par[colnames(tp)])))
      cat("\n参照解との最大絶対誤差の推移:\n")
      idx <- unique(round(seq(1, length(err), length.out = min(10, length(err)))))
      print(round(setNames(err[idx], paste0("iter", idx)), 5))
    }
    next
  }

  # 結果ファイル
  e <- new.env(); load(f, envir = e)
  cat("実行結果\n")
  cat("  init_mode   :", if (is.null(e$INIT_MODE)) "(記録なし)" else e$INIT_MODE, "\n")
  cat("  tag         :", if (is.null(e$RUN_TAG) || !nzchar(e$RUN_TAG)) "(なし)" else e$RUN_TAG, "\n")
  cat("  alpha_mult  :", if (is.null(e$ALPHA_MULT)) "(記録なし)" else e$ALPHA_MULT, "\n")
  cat("  sampling_rate:", e$SAMPLING_RATE, " / beta2:", e$BETA2, "\n")
  cat("  完了した反復:", e$iter_done, "\n")
  if (!is.null(e$time_adam_sgd) && is.finite(e$time_adam_sgd))
    cat("  Adam 所要   :", sprintf("%.1f 時間 (%.1f 分/反復)", e$time_adam_sgd / 3600,
                                   e$time_adam_sgd / 60 / max(1, e$iter_done)), "\n")
  cat("  検出個体    :", e$n_detected, "（複数回 ", length(e$multi_ids),
      " / 単回 ", length(e$single_ids), "）\n", sep = "")
  cat("\n")

  if (!is.null(e$final_par)) {
    cmp_table(e$final_par, e$ref_par, "Adam-SGD")
  } else {
    cat("Adam-SGD は実行されていません（--no-sgd）。参照解のみ:\n")
    cat("  ", fmt_par(e$ref_par), "\n", sep = "")
  }

  # 生スケールへの逆変換。論文や旧結果と比べるときに要る。
  if (!is.null(e$COV_MU) && !is.null(e$COV_SD) && !is.null(e$ref_par)) {
    to_raw <- function(q) {
      c(dens_0    = unname(q["dens_0"]),
        conn_0    = unname(q["conn_0"]
                           - q["conn_agri"] * e$COV_MU["agri"] / e$COV_SD["agri"]
                           - q["conn_wtr"]  * e$COV_MU["wtr"]  / e$COV_SD["wtr"]),
        conn_agri = unname(q["conn_agri"] / e$COV_SD["agri"]),
        conn_wtr  = unname(q["conn_wtr"]  / e$COV_SD["wtr"]),
        g0_1      = unname(q["g0_1"]))
    }
    cat("\n生スケール（agri も wtr も生の値に対する係数）:\n")
    print(round(to_raw(e$ref_par), 5))
    cat("標準化スケールでの効果の比 |agri/wtr| =",
        sprintf("%.2f", abs(e$ref_par["conn_agri"] / e$ref_par["conn_wtr"])), "\n")
  }
}
hr()
