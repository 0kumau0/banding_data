# examples/pcap_bench.R ------------------------------------------------------
#
# `pcap_poisson` の2つの実装上の問題を、修正版と並べて実測する。
#
#   Rscript examples/pcap_bench.R
#   Rscript examples/pcap_bench.R --nind 2400,4800,9600,19200 --nmu 400
#
# **adcrsgd/secrad.r は触らない。** 同じ関数の3版を別に定義して比べるだけ。
# 実データ不要。書き出すのは results/pcap_bench.csv だけ。
#
# ---------------------------------------------------------------------------
# 背景（2026-09-14）
#
# examples/profile_loglf.R のプロファイルで、loglf 1回の時間の 96% が
# pcap_poisson に集中し、個体数4倍で13倍（指数 1.85）に伸びることが分かった。
# アルゴリズム本体は nmu x nind x neffort で線形のはずなので、これはおかしい。
#
# secrad.r:313-323 の現行実装:
#
#   #pragma omp parallel for private(i,j,k,colnum,temp)
#   for(i=0;i<nmu;i++){
#     for(j=0;j<nind;j++){
#       for(k=0;k<neffort;k++){
#         colnum = k+neffort*(ind_cov(j)-ind_cov.minCoeff());   // (1)
#         temp = dpoisson_c(...);
#         #pragma omp atomic                                     // (2)
#         res(i,j) += temp;
#       }
#     }
#   }
#
# (1) ind_cov.minCoeff() は**ループ不変**（ind_cov は const）なのに最内ループに
#     ある。minCoeff() は長さ nind の縮約なので、
#     総コストが nmu x nind x neffort x O(nind) = **O(nind^2)** になる。
#
# (2) 並列化しているのは i のループで、各スレッドは自分の i の行にしか
#     書き込まない。**排他は不要。** nind=19200 では 1.92 億回の atomic が走り、
#     さらに MatrixXd は列優先なので異なる i・同じ j のスレッドが
#     同じキャッシュラインに書き込む（false sharing）。
#
# どちらも**計算結果を変えない**。変わるのは速度だけ。
# それを確かめるのがこのスクリプト。
#
# 比較する3版:
#   orig … 現行そのまま
#   fix1 … (1) だけ直す（minCoeff をループ外に）
#   fix2 … (1) + (2)（内側 k をローカルに足し込み、最後に1回だけ書く）
#
# **C++ の本体は examples/pcap_kernels.cpp にある**（2026-09-29 に切り出した）。
# examples/pcap_threads.R が同じカーネルを使う。片方だけ直して食い違うと、
# 2つの測定が比較できなくなるため。
# ---------------------------------------------------------------------------

OUT_CSV <- "results/pcap_bench.csv"
KERNELS <- "examples/pcap_kernels.cpp"

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--nind", "--nmu", "--neffort", "--rep")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

NIND_SET <- as.integer(strsplit(.opt("--nind", "2400,4800,9600,19200"), ",")[[1]])
NMU      <- as.integer(.opt("--nmu", "400"))
NEFFORT  <- as.integer(.opt("--neffort", "25"))
NREP     <- as.integer(.opt("--rep", "3"))

suppressMessages({ library(Rcpp); library(RcppEigen) })
if (!file.exists(KERNELS))
  stop(KERNELS, " が見つかりません。リポジトリルートで実行してください: ", getwd())

cat("== C++ のコンパイル ==\n")
sourceCpp(KERNELS, rebuild = TRUE)
cat("  OpenMP スレッド数: ", pcap_threads_used(),
    "（論理コア ", pcap_procs(), "）\n", sep = "")

mk_input <- function(nind) {
  set.seed(20260914)
  list(detect        = matrix(rpois(NEFFORT * nind, 0.3), NEFFORT, nind),
       effort_occ    = rep(1L, NEFFORT),
       loglambda_mat = matrix(rnorm(NMU * NEFFORT, -3, 1), NMU, NEFFORT),
       srv           = matrix(1L, nind, 1L),
       ind_cov       = rep(1L, nind))
}

timeit <- function(f, a, rep = NREP) {
  f(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)   # ウォームアップ
  median(replicate(rep, system.time(
    f(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov))[["elapsed"]]))
}

rows <- list()
for (n in NIND_SET) {
  cat(sprintf("\n---- nind = %d （nmu=%d, neffort=%d）----\n", n, NMU, NEFFORT))
  a <- mk_input(n)

  t_orig <- timeit(pcap_orig, a)
  t_fix1 <- timeit(pcap_fix1, a)
  t_fix2 <- timeit(pcap_fix2, a)

  r0 <- pcap_orig(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)
  r1 <- pcap_fix1(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)
  r2 <- pcap_fix2(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)
  d1 <- max(abs(r0 - r1)); d2 <- max(abs(r0 - r2))

  cat(sprintf("  orig %8.3f 秒\n  fix1 %8.3f 秒  (%.1f 倍速)\n  fix2 %8.3f 秒  (%.1f 倍速)\n",
              t_orig, t_fix1, t_orig / t_fix1, t_fix2, t_orig / t_fix2))
  cat(sprintf("  結果の最大絶対差: fix1 %.3g / fix2 %.3g %s\n",
              d1, d2, if (max(d1, d2) == 0) "← 完全一致" else "← ⚠ 一致しない"))

  rows[[length(rows) + 1L]] <- data.frame(
    nmu = NMU, neffort = NEFFORT, nind = n,
    orig = t_orig, fix1 = t_fix1, fix2 = t_fix2,
    speedup1 = t_orig / t_fix1, speedup2 = t_orig / t_fix2,
    maxdiff1 = d1, maxdiff2 = d2)
}

res <- do.call(rbind, rows)
cat("\n==================== まとめ ====================\n")
print(format(res, digits = 3), row.names = FALSE)

if (nrow(res) >= 2) {
  a <- res[1, ]; b <- res[nrow(res), ]
  r <- b$nind / a$nind
  cat(sprintf("\n## nind %d → %d（%.0f 倍）のときの伸び\n", a$nind, b$nind, r))
  cat(sprintf("  orig : %.3f → %.3f 秒（%.1f 倍、指数 %.2f）\n",
              a$orig, b$orig, b$orig / a$orig, log(b$orig / a$orig) / log(r)))
  cat(sprintf("  fix1 : %.3f → %.3f 秒（%.1f 倍、指数 %.2f）\n",
              a$fix1, b$fix1, b$fix1 / a$fix1, log(b$fix1 / a$fix1) / log(r)))
  cat(sprintf("  fix2 : %.3f → %.3f 秒（%.1f 倍、指数 %.2f）\n",
              a$fix2, b$fix2, b$fix2 / a$fix2, log(b$fix2 / a$fix2) / log(r)))
  cat("\n  指数 2.0 = nind の2乗 / 1.0 = 線形。\n")
  cat("  fix で指数が 1 に落ちるなら、2乗は minCoeff のループ内評価が原因と確定する。\n")
}

dir.create("results", showWarnings = FALSE)
write.csv(res, OUT_CSV, row.names = FALSE)
cat("\n保存: ", OUT_CSV, "\n", sep = "")
