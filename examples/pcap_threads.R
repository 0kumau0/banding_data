# examples/pcap_threads.R ----------------------------------------------------
#
# **`pcap_poisson` がスレッド数にどれだけ伸びるかを測る。**
#
#   Rscript examples/pcap_threads.R
#   Rscript examples/pcap_threads.R --nind 4800 --threads 1,2,4,8,16,32 --rep 3
#
# 実データ不要。書き出すのは results/pcap_threads.csv だけ。
# **adcrsgd/secrad.r は触らない**（カーネルは examples/pcap_kernels.cpp）。
#
# ---------------------------------------------------------------------------
# なぜこれが要るのか（2026-09-29）
#
# 「計算機を大きくしたら足りるか」という問いに答えるための数字。
# **コアを4倍にして本当に4倍になるのか、2倍で頭打ちか**が分からないと、
# 計算機センターの検討ができない。
#
# `examples/thread_scaling.R` で OpenMP 効率を実測しているが、
# **測ったのは `advdiff.eigen`**。いまのボトルネックは `pcap` で
# （`loglf` の時間の96%）、そちらのスレッド伸び率は測っていなかった。
#
# ## 予想（測る前に書いておく）
#
# ループは `i`（= `nmu`）で並列化されていて、`nmu` は数千〜1万。
# 各スレッドは自分の行にしか書かないので、**本来なら素直に伸びる形**。
#
# ところが現行実装には `#pragma omp atomic` があり、`MatrixXd` は列優先なので
# **異なる `i`・同じ `j` のスレッドが同じキャッシュラインに書く**（false sharing）。
#
#   → **`orig` はスレッドを増やすほど効率が落ちる**はず。
#   → `fix2`（atomic を外した版）は素直に伸びるはず。
#
# これが当たれば、**「`#pragma omp atomic` を外すことが、大きい計算機を
# 意味のあるものにする前提条件」**と言える。外れたら、その主張は取り下げる。
#
# ## 答え合わせ（2026-09-29、編集機で実測）— **予想は外れ。主張は取り下げた**
#
# `nmu` 400 / `nind` 2400 / `neffort` 25、20スレッドまで:
#
#   | スレッド | orig eff | fix1 eff | fix2 eff |
#   |---|---|---|---|
#   | 2  | 0.74 | 0.76 | 0.85 |
#   | 4  | 0.64 | 0.82 | 0.84 |
#   | 8  | 0.31 | 0.36 | 0.35 |
#   | 20 | 0.25 | 0.26 | 0.29 |
#
# **`orig` と `fix2` の差はわずか**（20スレッドで 0.25 対 0.29）。
# `orig/fix2` の時間比も 1スレッドで 2.31、20スレッドで 2.64 とほぼ横ばいで、
# **atomic の競合はスケーリングをほとんど支配していない。**
#
# **崩れの正体は測定機のほう**: 編集機は Core i7-1370P（性能コア6＋効率コア8
# / 20スレッド、28W級モバイル）。4スレッドまでは性能コアだけを使うので
# efficiency 0.64〜0.84 と正常。8スレッドから効率コアと SMT に載り、
# 静的分割が均等な塊を配るので**いちばん遅いコアを全員が待つ**。
# → **この数字は均質なコアのマシンには転用できない。**
#
# ## もう1つ分かったこと（results/pcap_bench.csv から。こちらのほうが重要）
#
# **`fix1` だけで `fix2` の 91〜96% の速度が得られる**
# （speedup1/speedup2 = 1.00 / 0.98 / 0.93 / 0.91、nind 2400→19200）。
# **atomic を外して増えるのは 4〜9% だけ。**
#
# → **本質的な修正は `minCoeff()` をループの外へ出す1行**。
#   「本家との差分を小さく保ちたい」という制約（CLAUDE.md、2026-09-14）とは
#   相性がよい。
#
# ## 読み方
#
#   speedup   = t(1スレッド) / t(nスレッド)
#   efficiency = speedup / n     … 1.0 が理想。0.5 なら「コア半分が無駄」
#
# 物理コアを超えると（SMT / ハイパースレッディング）効率は必ず落ちるので、
# **物理コア数までの傾き**を見ること。
# ---------------------------------------------------------------------------

OUT_CSV <- "results/pcap_threads.csv"
KERNELS <- "examples/pcap_kernels.cpp"

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--nind", "--nmu", "--neffort", "--rep", "--threads", "--kernels")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

NIND    <- as.integer(.opt("--nind",    "2400"))
NMU     <- as.integer(.opt("--nmu",     "400"))
NEFFORT <- as.integer(.opt("--neffort", "25"))
NREP    <- as.integer(.opt("--rep",     "3"))
KERNS   <- strsplit(.opt("--kernels", "orig,fix1,fix2"), ",")[[1]]
stopifnot(NIND > 0, NMU > 0, NEFFORT > 0, NREP >= 1,
          all(KERNS %in% c("orig", "fix1", "fix2")))

suppressMessages({ library(Rcpp); library(RcppEigen) })
if (!file.exists(KERNELS))
  stop(KERNELS, " が見つかりません。リポジトリルートで実行してください: ", getwd())

cat("== C++ のコンパイル ==\n")
sourceCpp(KERNELS, rebuild = TRUE)

NPROC <- pcap_procs()
NPHYS <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)

## ★ **CPU が均質かを先に確かめる。** 性能コアと効率コアが混在した CPU
## （Intel の P/E コア構成など）では、`#pragma omp parallel for` の既定の
## 静的分割が全スレッドに均等な塊を配るため、**いちばん遅いコアを全員が待つ**。
## その結果 efficiency が崩れるが、それは**アルゴリズムではなく CPU の性質**で、
## 均質なサーバや計算機センターのノードには転用できない。
##
## 2026-09-29 に編集機（Core i7-1370P。性能コア6＋効率コア8 / 20スレッド、
## 28W級モバイル）で測ったところ、4スレッドまで efficiency 0.64〜0.84 なのに
## **8スレッドで 0.31 に崩れ、所要時間が4スレッドより長くなった**。
## この数字を「pcap は並列化が効かない」と読んではいけない。
if (!is.na(NPHYS) && NPROC > NPHYS)
  cat(sprintf("\n⚠ 論理 %d > 物理 %d（SMT あり）。物理コアを超える範囲の efficiency は\n",
              NPROC, NPHYS),
      "  計算能力ではなく命令の詰め込み具合を見ているので、額面どおりに読まないこと\n",
      sep = "")
cat("\n⚠ **性能コアと効率コアが混在した CPU では efficiency が崩れる。**\n",
    "  それは CPU の性質であってアルゴリズムの性質ではない。\n",
    "  計算機センターの検討に使うなら、**均質なコアのマシンで測り直すこと**\n",
    "  （このプロジェクトなら解析機の32スレッド。ただし他の計算と同時に走らせない）\n",
    sep = "")
## 既定は 1,2,4,8,... を論理コア数まで。最後に論理コア数そのものを足す
THREADS <- if (!is.null(.opt("--threads"))) {
  as.integer(strsplit(.opt("--threads"), ",")[[1]])
} else {
  t <- 2^(0:ceiling(log2(max(NPROC, 2))))
  sort(unique(c(t[t <= NPROC], NPROC)))
}
stopifnot(all(THREADS >= 1L))

cat(sprintf("  論理コア %d / 測るスレッド数: %s\n", NPROC,
            paste(THREADS, collapse = ", ")))
cat(sprintf("  規模: nmu %d × nind %d × neffort %d / 各 %d 回の中央値\n",
            NMU, NIND, NEFFORT, NREP))
cat("  カーネル: ", paste(KERNS, collapse = ", "), "\n", sep = "")

set.seed(20260929)
a <- list(detect        = matrix(rpois(NEFFORT * NIND, 0.3), NEFFORT, NIND),
          effort_occ    = rep(1L, NEFFORT),
          loglambda_mat = matrix(rnorm(NMU * NEFFORT, -3, 1), NMU, NEFFORT),
          srv           = matrix(1L, NIND, 1L),
          ind_cov       = rep(1L, NIND))

FUN <- list(orig = pcap_orig, fix1 = pcap_fix1, fix2 = pcap_fix2)

timeit <- function(f) {
  f(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov)   # ウォームアップ
  median(replicate(NREP, system.time(
    f(a$detect, a$effort_occ, a$loglambda_mat, a$srv, a$ind_cov))[["elapsed"]]))
}

rows <- list()
ref  <- setNames(rep(NA_real_, length(KERNS)), KERNS)   # 1スレッドの時間
for (nt in THREADS) {
  pcap_set_threads(nt)
  used <- pcap_threads_used()
  ## **要求した数と実際に使われた数を突き合わせる。** 環境変数や
  ## ランタイムの都合で要求どおりにならないことがあり、それを知らずに
  ## 「伸びなかった」と読むと結論を誤る
  cat(sprintf("\n---- 要求 %d スレッド（実際 %d）----\n", nt, used))
  if (used != nt) cat("  ⚠ 要求と実際が違う。以下の値はこの実際の数で解釈すること\n")

  for (kn in KERNS) {
    t <- timeit(FUN[[kn]])
    if (is.na(ref[kn])) ref[kn] <- t
    sp <- ref[kn] / t
    cat(sprintf("  %-5s %8.3f 秒   speedup %5.2f   efficiency %5.2f\n",
                kn, t, sp, sp / used))
    rows[[length(rows) + 1L]] <- data.frame(
      kernel = kn, threads_req = nt, threads_used = used,
      nmu = NMU, neffort = NEFFORT, nind = NIND,
      time = t, speedup = sp, efficiency = sp / used)
  }
}
pcap_set_threads(NPROC)

res <- do.call(rbind, rows)
cat("\n==================== まとめ ====================\n")
print(format(res, digits = 3), row.names = FALSE)

# --- 結論 -------------------------------------------------------------------
cat("\n## 読み方\n")
cat("  efficiency = speedup / スレッド数。1.0 が理想、0.5 で「コア半分が無駄」。\n")
cat("  物理コアを超えると必ず落ちるので、**物理コアまでの傾き**を見ること。\n")

top <- max(res$threads_used)
for (kn in KERNS) {
  r <- res[res$kernel == kn & res$threads_used == top, ]
  if (nrow(r)) cat(sprintf("\n  %-5s: %d スレッドで %.1f 倍（efficiency %.2f）",
                           kn, top, r$speedup[1], r$efficiency[1]))
}
cat("\n")

if (all(c("orig", "fix2") %in% KERNS)) {
  ro <- res[res$kernel == "orig" & res$threads_used == top, ]
  rf <- res[res$kernel == "fix2" & res$threads_used == top, ]
  if (nrow(ro) && nrow(rf)) {
    cat("\n## 予想の答え合わせ（この節の主張は測定に従って書き換えること）\n")
    cat(sprintf("  orig efficiency %.2f / fix2 efficiency %.2f\n",
                ro$efficiency[1], rf$efficiency[1]))
    if (rf$efficiency[1] > ro$efficiency[1] + 0.1) {
      cat("  → **予想どおり。orig は atomic と false sharing でスレッドを活かせない。**\n")
      cat("     コア数を増やす投資は、pcap の修正とセットでないと目減りする。\n")
    } else if (ro$efficiency[1] > rf$efficiency[1] + 0.1) {
      cat("  → ⚠ 予想と逆。orig のほうが効率がよい。主張を取り下げること。\n")
    } else {
      cat("  → 差は小さい。**「atomic がスケーリングを阻害する」とは言えない。**\n")
      cat("     この規模では atomic の競合が起きていない可能性がある",
          "（nmu が小さい / 実行時間が短い）。\n")
      cat("     --nmu や --nind を上げて確かめること。\n")
    }
  }
}

dir.create("results", showWarnings = FALSE)
write.csv(res, OUT_CSV, row.names = FALSE)
cat("\n保存: ", OUT_CSV, "\n", sep = "")
