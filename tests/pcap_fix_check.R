# tests/pcap_fix_check.R -----------------------------------------------------
#
# **`adcrsgd/secrad.r` に入れた minCoeff の巻き上げが、本家と同じ値を返すことを確かめる。**
#
#   Rscript tests/pcap_fix_check.R
#   Rscript tests/pcap_fix_check.R --rep 20 --seed 1
#
# 実データ不要。約1分（C++ のコンパイルが2回走る）。
#
# ---------------------------------------------------------------------------
# なぜこの形なのか（2026-09-29）
#
# 2026-09-29 に `adcrsgd/secrad.r` の `pcap_bin` / `pcap_poisson` /
# `pcap_poisson_debug` で、最内ループにあった `ind_cov.minCoeff()` を
# ループの外へ出した（ループ不変式の巻き上げ。CLAUDE.md の該当節）。
#
# **本家 kfukasawa37/adcrtest2 からの改変なので、「値が変わっていない」ことを
# 機械的に確かめられる状態にしておく必要がある。** 論文で
# 「数値結果は完全に一致」と書くなら、その根拠がコマンド1つで再現できること。
#
# ## やり方
#
# `examples/pcap_kernels.cpp` には**本家の実装そのままの写し**（`pcap_orig`）が
# 入れてある。これは測定用に置いたものだが、**改変前の基準**としても使える。
#
#   secrad.r の pcap_poisson（改変後）  vs  pcap_kernels.cpp の pcap_orig（本家）
#
# 同じ入力を両方に通して差を見る。**期待は最大絶対差 0**（丸め誤差すら出ない。
# 計算の順序も演算も変えていないため）。
#
# ## 何を振るか
#
# `ind_cov` の最小値が 1 以外のときにこそ差が出うる（`cov_min` の値が効く）ので、
# **`ind_cov` を 1 始まりでない場合も含めて振る**。ここを 1 固定で試すと
# 「どちらも同じ」になるのが当たり前で、検証になっていない。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--rep", "--seed")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

NREP <- as.integer(.opt("--rep", "12"))
SEED <- as.integer(.opt("--seed", "20260929"))
KERNELS <- "examples/pcap_kernels.cpp"

cat("== pcap の改変が値を変えていないかの検証 ==\n")
cat("  改変後: adcrsgd/secrad.r の pcap_poisson / pcap_bin\n")
cat("  基 準 : ", KERNELS, " の pcap_orig（本家の写し）\n", sep = "")

suppressMessages({ library(Rcpp); library(RcppEigen) })
for (p in c(KERNELS, "adcrsgd/secrad.r"))
  if (!file.exists(p)) stop(p, " が見つかりません。リポジトリルートで実行してください: ", getwd())

cat("\n本家の写しをコンパイル中...\n")
sourceCpp(KERNELS, rebuild = TRUE)
ref_poisson <- pcap_orig                       # 改変前の pcap_poisson と同じもの

cat("改変後の secrad.r をコンパイル中（30秒ほど）...\n")
suppressMessages(suppressWarnings(source("adcrsgd/secrad.r", encoding = "UTF-8")))
## source した側の pcap_poisson / pcap_bin で上書きされている
new_poisson <- pcap_poisson
new_bin     <- pcap_bin

ok <- TRUE
report <- function(label, d, detail) {
  good <- isTRUE(d == 0)
  if (!good) ok <<- FALSE
  cat(sprintf("  %-34s 最大絶対差 %-10.3g %s\n", label, d,
              if (good) "← 完全一致" else "← ★不一致"))
  if (!good) cat("      条件: ", detail, "\n", sep = "")
}

set.seed(SEED)
cat("\n", NREP, " 通りの条件で比較する（ind_cov の最小値も振る）:\n", sep = "")
for (r in seq_len(NREP)) {
  nmu     <- sample(5:40, 1)
  neffort <- sample(2:12, 1)
  nind    <- sample(3:30, 1)
  ## ★ ここが要点。ind_cov の最小値が 1 でない場合を必ず混ぜる。
  ##   1 固定だと cov_min の値が効かず、検証にならない
  ngrp    <- sample(1:3, 1)
  offset  <- sample(c(1L, 1L, 2L, 5L, 10L), 1)      # 最小値をずらす
  ind_cov <- as.integer(sample(seq_len(ngrp), nind, replace = TRUE) + offset - 1L)
  ind_cov[1] <- offset                               # 最小値を確実に offset にする
  nocc    <- max(1L, sample(1:3, 1))

  a <- list(detect        = matrix(rpois(neffort * nind, 0.4), neffort, nind),
            effort_occ    = as.integer(sample(seq_len(nocc), neffort, replace = TRUE)),
            loglambda_mat = matrix(rnorm(nmu * neffort * ngrp, -2, 1), nmu, neffort * ngrp),
            srv           = matrix(1L, nind, nocc),
            ind_cov       = ind_cov)

  r_ref <- ref_poisson(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)
  r_new <- new_poisson(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)
  detail <- sprintf("nmu %d / neffort %d / nind %d / ind_cov %d〜%d",
                    nmu, neffort, nind, min(ind_cov), max(ind_cov))
  report(sprintf("[%2d] pcap_poisson  (min=%2d)", r, min(ind_cov)),
         max(abs(r_ref - r_new)), detail)
}

## --- pcap_bin も同じ巻き上げを入れたので確認する ---------------------------
## こちらは本家の写しを用意していないので、**cov_min が効かない条件
## （ind_cov がすべて 1）での自己整合**しか見られない。
## 実際に使っているのは pcap_poisson なので、ここは「壊していない」確認まで。
cat("\npcap_bin（type=\"binom\" 用。現在の解析では未使用）:\n")
set.seed(SEED + 1)
nmu <- 20L; neffort <- 6L; nind <- 12L
b <- list(detect     = matrix(rbinom(neffort * nind, 3, 0.2), neffort, nind),
          effort     = rep(3L, neffort),
          effort_occ = rep(1L, neffort),
          lp_mat     = matrix(-abs(rnorm(nmu * neffort)), nmu, neffort),
          srv        = matrix(1L, nind, 1L),
          ind_cov    = rep(1L, nind))
v <- tryCatch(new_bin(b$detect, b$effort, b$effort_occ, b$lp_mat, b$srv, b$ind_cov),
              error = function(e) e)
if (inherits(v, "error")) {
  cat("  ★ 呼び出しに失敗: ", conditionMessage(v), "\n", sep = ""); ok <- FALSE
} else {
  cat(sprintf("  戻り値 %d × %d / 有限値 %s\n", nrow(v), ncol(v),
              if (all(is.finite(v))) "OK" else "★NG"))
  if (!all(is.finite(v))) ok <- FALSE
}

cat("\n------------------------------------------------------------------\n")
if (ok) {
  cat("**すべて完全一致。** 改変は値を変えていない。\n")
  cat("論文に「数値結果は完全に一致（最大絶対差 0）」と書く根拠はこのコマンド。\n")
} else {
  cat("★ 一致しない条件がある。改変を見直すこと。\n")
}
quit(status = if (ok) 0L else 1L)
