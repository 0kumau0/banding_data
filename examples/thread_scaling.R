# examples/thread_scaling.R --------------------------------------------------
#
# advdiff.eigen の OpenMP 並列化効率を測る。
#
#   Rscript examples/thread_scaling.R
#
# 実データ不要。結果は results/thread_scaling.csv に残す。
#
# ---------------------------------------------------------------------------
# 何のための測定か
# ---------------------------------------------------------------------------
#
# advdiff_core の中心はこのループ:
#
#   #pragma omp parallel for
#   for (i = 0; i < nmu; i++) {          // nmu = セル数（活動中心の候補）
#       ... 疎行列を組み立て ...
#       eigenvec = eigenapprox_solve(advdiff, b);
#       res.row(i) = log(eigenvec);
#   }
#
# nmu 回の独立した疎行列ソルブなので、原理的には完全に並列化できる。
# 実際にコア数に比例して速くなるなら、**計算機センターの多コアノードへ
# 移すことが直接そのまま効く**。飽和しているなら、コアを増やしても無駄。
#
# ---------------------------------------------------------------------------
# スレッド数の変え方（重要）
# ---------------------------------------------------------------------------
#
# **omp_set_num_threads() を別の cppFunction から呼んでも効かない。**
# Windows + Rtools では sourceCpp で作る DLL ごとに libgomp が静的リンクされ、
# secrad.r の DLL とは別の OpenMP ランタイムになるため。
# 2026-09-10 にこれで測定を失敗した（1スレッドと8スレッドで時間が同じだった）。
#
# 確実なのは環境変数 OMP_NUM_THREADS を **R の起動前**に設定すること。
# そこでこのスクリプトは、スレッド数ごとに自分自身を子プロセスとして起動する。
#
#   親: Rscript examples/thread_scaling.R
#   子: OMP_NUM_THREADS=n Rscript examples/thread_scaling.R --worker n <ncell> <out>
#
# 子は1条件だけ測って CSV に1行追記し、終了する。
# 子ごとに secrad.r のコンパイル（約25秒）が入るが、確実さを優先する。
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
NCELLS     <- c(1600, 2500)     # 40x40 と 50x50。問題規模で効率が変わるかを見る
REPS       <- 2
OUT        <- "results/thread_scaling.csv"

args <- commandArgs(trailingOnly = TRUE)
IS_WORKER <- length(args) >= 1L && args[1] == "--worker"

make_grid <- function(ncell) {
  nx <- round(sqrt(ncell)); nc <- nx * nx
  xs <- rep(1:nx, each = nx); ys <- rep(1:nx, nx)
  X <- as.numeric(scale(sin(xs / 2) + cos(ys / 3)))
  d <- secrad_data$new(coords = cbind(x = xs, y = ys), area = rep(1, nc),
                       grid_cov = data.frame(X = X), resolution = c(x = 1, y = 1))
  list(d = d, nc = nc,
       logc = c(model.matrix(~X, data = d$grid_cov) %*% c(-1.0, 0.3)))
}

time_advdiff <- function(g) {
  t0 <- Sys.time()
  invisible(advdiff.eigen(mucoords = g$d$coords, gridcoords = g$d$coords,
                          logc = g$logc, loga = rep(0, g$nc),
                          neighmat = g$d$neighmat, resolution = g$d$resolution,
                          logprob = TRUE))
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}


# ===========================================================================
# 子プロセス: 1条件だけ測って追記する
# ===========================================================================
if (IS_WORKER) {
  th     <- as.integer(args[2])
  ncellt <- as.integer(args[3])
  outf   <- args[4]

  suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
  g <- make_grid(ncellt)
  invisible(time_advdiff(g))                       # 1回捨てる（暖機）
  secs <- median(replicate(REPS, time_advdiff(g)))

  # 実際に OpenMP が何スレッド使うつもりかも記録しておく（設定が効いたかの確認）
  line <- data.frame(ncell = g$nc, threads = th, sec = secs,
                     omp_env = Sys.getenv("OMP_NUM_THREADS", ""))
  write.table(line, outf, sep = ",", row.names = FALSE,
              col.names = !file.exists(outf), append = file.exists(outf))
  cat(sprintf("  ncell=%d  %3d スレッド : %8.2f 秒\n", g$nc, th, secs))
  quit(status = 0)
}


# ===========================================================================
# 親プロセス: 条件を並べて子を順に起動する
# ===========================================================================
stopifnot(file.exists(SOURCEPATH))

nproc <- parallel::detectCores(logical = TRUE)
THREADS <- unique(sort(c(1, 2, 4, 8, 16, nproc)))
THREADS <- THREADS[THREADS <= nproc]

cat("== advdiff.eigen の並列化効率を測定 ==\n")
cat("論理プロセッサ:", nproc, " / スレッド数:", paste(THREADS, collapse = ", "), "\n")
cat("条件ごとに子プロセスを起動する（それぞれ secrad.r のコンパイルに約25秒）\n\n")

dir.create("results", showWarnings = FALSE)
if (file.exists(OUT)) file.remove(OUT)

rscript <- file.path(R.home("bin"), "Rscript")
self    <- normalizePath("examples/thread_scaling.R", winslash = "/")

for (ncell_target in NCELLS) {
  cat(sprintf("--- ncell = %d ---\n", ncell_target))
  for (th in THREADS) {
    # system2 の env= は Windows では効かない（環境変数の指定がコマンド行に
    # 前置きされ、cmd がプログラム名と解釈して失敗する。2026-09-10 に status 5 で
    # 全滅した）。親で Sys.setenv してから起動し、子に継承させる。
    Sys.setenv(OMP_NUM_THREADS = as.character(th))
    status <- system2(rscript, c(shQuote(self), "--worker", th, ncell_target, shQuote(OUT)))
    if (!identical(status, 0L))
      cat(sprintf("  ncell=%d %3d スレッド : 失敗 (status %s)\n", ncell_target, th, status))
  }
  cat("\n")
}

if (!file.exists(OUT)) stop("測定結果がありません。子プロセスがすべて失敗しています。")
res <- read.csv(OUT)

# 1スレッドを基準に、速度向上と並列化効率を出す。
# 効率 = 速度向上 / スレッド数。1.0 なら理想、0.5 なら半分しか使えていない。
res <- do.call(rbind, lapply(split(res, res$ncell), function(d) {
  d <- d[order(d$threads), ]
  base <- d$sec[d$threads == 1]
  d$speedup <- if (length(base)) base / d$sec else NA_real_
  d$efficiency <- d$speedup / d$threads
  d
}))
rownames(res) <- NULL
write.csv(res, OUT, row.names = FALSE)

cat("== 結果 ==\n")
print(transform(res, sec = round(sec, 2), speedup = round(speedup, 2),
                efficiency = round(efficiency, 3)))
cat("\n保存:", OUT, "\n\n")

# --- 外挿 -------------------------------------------------------------------
cat("== 多コアノードへ移した場合の見積もり ==\n")
for (nc in unique(res$ncell)) {
  d <- res[res$ncell == nc & is.finite(res$speedup), ]
  if (!nrow(d)) next
  top <- d[which.max(d$threads), ]
  # Amdahl: speedup(n) = 1/(s + (1-s)/n) を最大スレッド点で解いて直列部分 s を推定
  s <- (1 / top$speedup - 1 / top$threads) / (1 - 1 / top$threads)
  s <- max(0, min(1, s))
  cat(sprintf("\nncell = %d（%d スレッドで %.2f 倍、効率 %.2f）\n",
              nc, top$threads, top$speedup, top$efficiency))
  cat(sprintf("  Amdahl の直列部分の推定: %.1f%%\n", 100 * s))
  for (n in c(32, 64, 128, 256)) {
    sp <- 1 / (s + (1 - s) / n)
    cat(sprintf("  %3d コア: 1スレッド比 %5.1f 倍 … 現在の %d スレッド比 %.1f 倍\n",
                n, sp, top$threads, sp / top$speedup))
  }
}
cat("\n注意: Amdahl の外挿はメモリ帯域の飽和を考慮していない。実際にはこれより悪くなる。\n")
cat("      上限の目安として読むこと。\n")
