# examples/profile_loglf.R ---------------------------------------------------
#
# `loglf` 1回の時間がどこで使われているかを特定する。
#
#   Rscript examples/profile_loglf.R
#   Rscript examples/profile_loglf.R --nx 20 --nind 4800,19200
#
# 実データ不要。書き出すのは results/profile_loglf.txt だけ。
#
# ---------------------------------------------------------------------------
# 何を調べているか（2026-09-14）
#
# examples/nind_scaling.R の実測で、ncell を固定したまま nind を増やすと
# loglf 1回のコストが **ほぼ nind の2乗**で伸びることが分かった。
#
#   nind    300 ->    1,200 ->   4,800 ->  19,200 ->  76,800
#   秒     0.87 ->     1.63 ->    7.20 ->    89.4 -> 1,258.5
#   局所的な指数        0.45      1.07      1.82      1.91
#
# 個体ごとの計算は本来 nind に線形のはずで、実際キャッシュが当たった側
# （warm）は傾き 1.01 と線形だった。2乗になるのは初回経路だけ。
# **モデルの構造からは出てこない形なので、実装由来の疑いが濃い。**
#
# 候補:
#   仮説1 R のガベージコレクション。ヒープが大きいほど1回の GC が重く、
#          確保回数が nind に比例すれば総 GC コストは nind^2 になる
#   仮説2 個体ループ内でのオブジェクトの成長（rbind / c() での逐次結合）
#   仮説3 モデル本来のコスト（＝直せない）
#
# 測り方:
#   - gc.time() の差分で **GC に費やした時間を直接測る**。仮説1はこれで決着する
#   - Rprof で自己時間の内訳を取り、**nind を変えたときに割合が増える関数**を探す。
#     2乗の犯人は「nind を増やすと割合が上がる」はず
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
OUT_TXT    <- "results/profile_loglf.txt"

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--nx", "--nind", "--ntrue", "--nt", "--g0", "--interval")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

NX       <- as.integer(.opt("--nx", "20"))
NIND_SET <- as.integer(strsplit(.opt("--nind", "4800,19200"), ",")[[1]])
NTRUE    <- as.integer(.opt("--ntrue", "1600"))
NT       <- as.integer(.opt("--nt", "200"))
G0PAR    <- as.numeric(.opt("--g0", "-4"))
INTERVAL <- as.numeric(.opt("--interval", "0.02"))

SEED <- 20260914
TRAP_AT <- c(2, 6, 9, 13, 17)
STEPSPERTIME <- 1; STEPAD <- 100; TIMEBURNIN <- 20
CPAR <- c(-1.0, 0.3); ADPAR <- -0.5

ncell <- NX * NX
cat(sprintf("== loglf のプロファイル ==\n  ncell = %d / nind = %s\n",
            ncell, paste(NIND_SET, collapse = ", ")))

cat("== secrad.r の読み込み ==\n")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))

xcoord <- rep(1:NX, each = NX); ycoord <- rep(1:NX, NX)
trapx <- rep(TRAP_AT, length(TRAP_AT)); trapy <- rep(TRAP_AT, each = length(TRAP_AT))
effort_loc <- match(paste(trapx, trapy), paste(xcoord, ycoord))
effort_loc <- effort_loc[!is.na(effort_loc)]
ntrap <- length(effort_loc)
X <- as.numeric(scale(sin(xcoord / 2) + cos(ycoord / 2)))
MODEL <- list(envmodel = list(D ~ 1, C ~ X, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

cat("== 合成データを1回生成 ==\n")
set.seed(SEED)
simdata <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord), area = rep(1, ncell),
                           grid_cov = data.frame(X = X), resolution = c(x = 1, y = 1))
simdata$add_obs(type = "poisson", effort = rep(NT, ntrap),
                effort_loc = effort_loc, effort_occ = rep(1, ntrap))
simdata$set_truemodel(envmodel = list(D ~ 1, C ~ X, A ~ 1),
                      indmodel = c(A = FALSE, g0 = FALSE), occmodel = c(A = FALSE, g0 = FALSE),
                      dpar = log(NTRUE / ncell), cpar = CPAR, adpar = ADPAR, g0par = G0PAR)
simdata$set_sim(NT, STEPSPERTIME, STEPAD, TIMEBURNIN)
simdata$generate_ind()
simdata$simulate(verbose = FALSE)
simdata$update_detect()
base_detect <- as.matrix(simdata$obs[[1]]$detect)
cat(sprintf("  検出 %d 個体を複製して nind を作る\n", ncol(base_detect)))

make_obj <- function(n) {
  idx <- rep_len(seq_len(ncol(base_detect)), n)
  d <- base_detect[, idx, drop = FALSE]
  sub <- secrad_data$new(coords = simdata$coords, area = simdata$area,
                         grid_cov = simdata$grid_cov, resolution = simdata$resolution)
  o <- simdata$obs[[1]]
  sub$add_obs(type = o$type, effort = o$effort, effort_loc = o$effort_loc,
              effort_occ = o$effort_occ, detect = d)
  sub$ind_cov <- rep(1, n)
  obj <- secrad$new(secrdata = sub)
  obj$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
                occmodel = MODEL$occmodel)
  obj
}

## ウォームアップ（初回呼び出しの余分を測定から外す）
{ w <- make_obj(min(NIND_SET)); invisible(w$loglf(generate_init(w), loglfscale = 1))
  rm(w); gc(full = TRUE) }

sink_lines <- character()
say <- function(...) { s <- paste0(...); cat(s, "\n"); sink_lines <<- c(sink_lines, s) }

profiles <- list()

for (n in NIND_SET) {
  say("")
  say(strrep("=", 70))
  say(sprintf("nind = %d", n))
  say(strrep("=", 70))

  obj <- make_obj(n)
  par <- generate_init(obj)

  gc(reset = TRUE, full = TRUE)
  gc.time(TRUE)                       # GC 時間の計測を有効化
  gc0 <- gc.time()

  prof <- tempfile(fileext = ".out")
  Rprof(prof, interval = INTERVAL, memory.profiling = TRUE, gc.profiling = TRUE)
  t_all <- system.time(ll <- obj$loglf(par, loglfscale = 1))[["elapsed"]]
  Rprof(NULL)

  gc1 <- gc.time()
  gc_elapsed <- (gc1 - gc0)[3]
  peak <- sum(gc(full = TRUE)[, 6])

  say(sprintf("  総時間      : %.2f 秒", t_all))
  say(sprintf("  うち GC     : %.2f 秒  (%.1f%%)   ← 仮説1の直接の検定",
              gc_elapsed, 100 * gc_elapsed / t_all))
  say(sprintf("  R ヒープ峰  : %.0f MB / logL = %.1f", peak, ll))

  sm <- tryCatch(summaryRprof(prof, memory = "both"), error = function(e) NULL)
  if (is.null(sm)) { say("  summaryRprof に失敗"); rm(obj); gc(full = TRUE); next }

  self <- sm$by.self
  say("")
  say("  自己時間の上位10（self.time = その関数自身で使った時間）")
  top <- head(self[order(-self$self.time), c("self.time", "self.pct", "total.time", "mem.total")], 10)
  for (i in seq_len(nrow(top))) {
    say(sprintf("    %-28s self %7.2fs (%5.1f%%)  total %7.2fs  mem %8.1f MB",
                rownames(top)[i], top$self.time[i], top$self.pct[i],
                top$total.time[i], top$mem.total[i]))
  }

  say("")
  say(sprintf("  サンプリング合計: %.2f 秒 / GC 実行中のサンプル割合: %s",
              sm$sampling.time,
              if (!is.null(sm$by.self$self.pct)) "by.self の gc 行を参照" else "不明"))

  profiles[[as.character(n)]] <- list(self = self, total = t_all, gc = gc_elapsed, peak = peak)
  rm(obj); gc(full = TRUE)
}

## --- nind を変えたときに「割合」が増えた関数を探す --------------------------
if (length(profiles) >= 2) {
  ns <- names(profiles)
  a <- profiles[[ns[1]]]; b <- profiles[[ns[length(ns)]]]
  say("")
  say(strrep("=", 70))
  say(sprintf("nind %s → %s で自己時間の割合が増えた関数（2乗の犯人はここ）", ns[1], ns[length(ns)]))
  say(strrep("=", 70))

  fa <- setNames(a$self$self.pct, rownames(a$self))
  fb <- setNames(b$self$self.pct, rownames(b$self))
  ta <- setNames(a$self$self.time, rownames(a$self))
  tb <- setNames(b$self$self.time, rownames(b$self))
  keys <- union(names(fa), names(fb))
  g <- function(v, k) ifelse(is.na(v[k]), 0, v[k])

  cmp <- data.frame(
    fun      = keys,
    pct_a    = round(g(fa, keys), 1),
    pct_b    = round(g(fb, keys), 1),
    sec_a    = round(g(ta, keys), 2),
    sec_b    = round(g(tb, keys), 2),
    row.names = NULL)
  cmp$倍率 <- ifelse(cmp$sec_a > 0, round(cmp$sec_b / cmp$sec_a, 1), NA)
  cmp$割合差 <- cmp$pct_b - cmp$pct_a
  cmp <- cmp[order(-cmp$sec_b), ]

  nind_ratio <- as.numeric(ns[length(ns)]) / as.numeric(ns[1])
  say(sprintf("  nind は %.0f 倍。総時間は %.1f 倍（%.2f → %.2f 秒）",
              nind_ratio, b$total / a$total, a$total, b$total))
  say(sprintf("  線形なら倍率 %.0f、2乗なら %.0f が目安", nind_ratio, nind_ratio^2))
  say("")
  say(sprintf("    %-28s %7s %7s %7s %7s %6s", "関数", "秒(小)", "秒(大)", "%(小)", "%(大)", "倍率"))
  for (i in seq_len(min(15, nrow(cmp)))) {
    say(sprintf("    %-28s %7.2f %7.2f %7.1f %7.1f %6s",
                cmp$fun[i], cmp$sec_a[i], cmp$sec_b[i], cmp$pct_a[i], cmp$pct_b[i],
                ifelse(is.na(cmp$倍率[i]), "-", format(cmp$倍率[i]))))
  }
  say("")
  say(sprintf("  GC: %.2f → %.2f 秒（%.1f 倍 / 総時間に占める割合 %.1f%% → %.1f%%）",
              a$gc, b$gc, if (a$gc > 0) b$gc / a$gc else NA,
              100 * a$gc / a$total, 100 * b$gc / b$total))
  say(sprintf("  ヒープ峰: %.0f → %.0f MB（%.1f 倍）", a$peak, b$peak, b$peak / a$peak))
}

dir.create("results", showWarnings = FALSE)
writeLines(sink_lines, OUT_TXT)
cat("\n保存: ", OUT_TXT, "\n", sep = "")
