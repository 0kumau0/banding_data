# examples/extract_bear_trace.R ----------------------------------------------
#
# 旧クマ実行（SGD_bera_20260818.R、2026-08〜09）のトレースを CSV に書き出す。
# レポートが results/*.csv から数値を読む方式なので、その供給元。
#
#   Rscript examples/extract_bear_trace.R
#
# 読むのは Claude\SGD_Adam_result_20260904.Rdata（推定値のトレースであって
# 生データではない）。無ければ何もせず終了する。
#
# ---------------------------------------------------------------------------
# 注意: この Rdata のトレースは行数が揃っていない
# ---------------------------------------------------------------------------
#
#   trace_par  531 行
#   trace_step 630 行
#   trace_grad 630 行
#   trace_ll   5 x 200（形が壊れている）
#
# 旧スクリプトの末尾で、複数回に分けた実行の結果を load() して rbind で
# 継ぎ足していたため。実際 diff(trace_par) と trace_step はどのオフセットでも
# 一致しない（最良でも最大差 0.029）。**par と step/grad は別系列として扱うこと。**
# 反復番号も通し番号としては信用できないので、記録の並び順として扱う。
#
# 2026-09-09 の診断（max_step は未使用、conn_wtr は最速で動いていた、
# 終盤に失速）はすべて trace_step / trace_grad の 630 行だけから導いており、
# この食い違いの影響を受けない。
# ---------------------------------------------------------------------------

RDATA <- "../SGD_Adam_result_20260904.Rdata"
PARS  <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")

# 旧実行の設定（SGD_bera_20260818.R より）
ALPHA    <- c(dens_0 = 0.02, conn_0 = 0.03, conn_agri = 0.03,
              conn_wtr = 0.03, g0_1 = 0.02)
MAX_STEP <- c(dens_0 = 0.05, conn_0 = 0.10, conn_agri = 0.05,
              conn_wtr = 0.05, g0_1 = 0.05)

if (!file.exists(RDATA)) {
  cat("見つかりません:", RDATA, "\n何もせず終了します。\n")
  quit(status = 0)
}

e <- new.env()
load(RDATA, envir = e)
dir.create("results", showWarnings = FALSE)

drop_na_rows <- function(m) {
  colnames(m) <- PARS
  m[!is.na(m[, 1]), , drop = FALSE]
}

par_m  <- drop_na_rows(e$trace_par)
step_m <- drop_na_rows(e$trace_step)
grad_m <- drop_na_rows(e$trace_grad)

cat(sprintf("par %d 行 / step %d 行 / grad %d 行\n",
            nrow(par_m), nrow(step_m), nrow(grad_m)))

# --- 1. パラメータの軌跡 ----------------------------------------------------
write.csv(data.frame(rec = seq_len(nrow(par_m)), par_m),
          "results/bear_run_20260904_par.csv", row.names = FALSE)

# --- 2. ステップと勾配（この2つは同じ長さで、互いに対応する） ---------------
ratio <- sweep(abs(step_m), 2, ALPHA, "/")
colnames(ratio) <- paste0("ratio_", PARS)
write.csv(data.frame(rec = seq_len(nrow(step_m)),
                     setNames(as.data.frame(step_m), paste0("step_", PARS)),
                     setNames(as.data.frame(grad_m), paste0("grad_", PARS)),
                     ratio),
          "results/bear_run_20260904_stepgrad.csv", row.names = FALSE)

# --- 3. 要約（レポートの主張の根拠） ----------------------------------------
opt <- e$secrad_res$par; names(opt) <- PARS
init <- c(dens_0 = -1, conn_0 = -2, conn_agri = 0, conn_wtr = -1, g0_1 = -5)
# 注: conn_agri / conn_wtr は旧スクリプトで明示指定されておらず generate_init 任せ。
# トレースの最初の値から読み取る。
init[c("conn_agri", "conn_wtr")] <- par_m[1, c("conn_agri", "conn_wtr")]

blocks <- list("1-50" = 1:50, "51-150" = 51:150, "151-300" = 151:300,
               "301-450" = 301:450, "451-" = 451:nrow(step_m))
ratio_by_block <- t(sapply(blocks, function(w) {
  w <- w[w <= nrow(step_m)]; apply(ratio[w, , drop = FALSE], 2, median)
}))
colnames(ratio_by_block) <- PARS

summ <- data.frame(
  par        = PARS,
  init       = as.numeric(init[PARS]),
  optim      = as.numeric(opt[PARS]),
  distance   = as.numeric(abs(opt[PARS] - init[PARS])),
  alpha      = as.numeric(ALPHA[PARS]),
  max_step   = as.numeric(MAX_STEP[PARS]),
  # max_step の上限に当たった回数。0 なら「効いていない」
  n_capped   = as.integer(colSums(abs(step_m) >= MAX_STEP[PARS] * 0.999)),
  # 終盤100反復での勾配の安定性
  grad_mean_tail = apply(grad_m[(nrow(grad_m) - 99):nrow(grad_m), , drop = FALSE], 2, mean),
  grad_sd_tail   = apply(grad_m[(nrow(grad_m) - 99):nrow(grad_m), , drop = FALSE], 2, sd),
  sign_agree_tail = apply(grad_m[(nrow(grad_m) - 99):nrow(grad_m), , drop = FALSE], 2,
                          function(x) max(mean(x > 0), mean(x < 0))),
  row.names = NULL
)
summ$snr_tail <- abs(summ$grad_mean_tail) / summ$grad_sd_tail

write.csv(summ, "results/bear_run_20260904_summary.csv", row.names = FALSE)
write.csv(data.frame(block = rownames(ratio_by_block), ratio_by_block),
          "results/bear_run_20260904_ratio.csv", row.names = FALSE)

cat("\n保存:\n  results/bear_run_20260904_par.csv\n",
    " results/bear_run_20260904_stepgrad.csv\n",
    " results/bear_run_20260904_summary.csv\n",
    " results/bear_run_20260904_ratio.csv\n", sep = "")

cat("\n--- 要約 ---\n")
print(summ, digits = 4)
cat("\n--- |step|/alpha の中央値 ---\n")
print(round(ratio_by_block, 4))
