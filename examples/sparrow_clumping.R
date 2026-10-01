# examples/sparrow_clumping.R ------------------------------------------------
#
# **捕獲の塊性を調べ、塊性に影響されない指標でトレンドが生き残るかを確かめる。**
#
#   Rscript examples/sparrow_clumping.R --spc B804,A312,9904
#   Rscript examples/sparrow_clumping.R --spc B804 --cap 5,10,30
#
# 生データを**読むだけ**。書き出すのは --out の CSV と図。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-10-01）
#
# `examples/sparrow_trend_check.R` でスズメの CPUE が強い減少を示した:
#
#   継続42地点・ほぼ一定の努力で  3.214 → 0.623（**5.2分の1**）
#   対照種は横ばい（シジュウカラ 0.89倍）か増加（ウグイス 1.9倍）
#   アシ原でもそれ以外でも減少（7.6分の1 / 3.4分の1）
#
# **3つの対照をすべて生き延びた。** ただし年次を見ると
#
#   1973年 5.785 / 1981-1986 に 2.0〜2.2 の高原 / 2009-2018 は 0.32〜0.69
#
# **スズメは冬にヨシ原で大群のねぐらを取る。** 一晩で数百〜数千羽という
# 捕獲が起こりうるので、
#
#   - **CPUE が少数の大量捕獲に支配されている可能性がある。**
#     初期の高い値がそれなら、「減少」は大量捕獲が起きなくなっただけかもしれない
#   - **ポアソン分布の仮定が壊れる。** ADCR の観測モデルは `pcap_poisson` で、
#     λ ≈ 1 のポアソンから「1回で2,000羽」は出てこない。
#     **ADCR を使えるかどうかに直結する**
#
# ---------------------------------------------------------------------------
# ★ 決定的な検査 — 塊性に影響されない指標
#
#   **出現率 = 1羽以上捕れた調査日の数 ÷ 全調査日**
#
# これは**1回の捕獲数に一切影響されない**。大量捕獲が1回あっても1日は1日。
#
#   - **出現率でも同じように減っていれば、減少は本物。**
#     大量捕獲の有無では説明できない
#   - **出現率は横ばいなのに CPUE だけ減っていれば、**
#     「捕れる日数は変わらず、1回あたりの数が減った」＝
#     **ねぐらの規模や捕獲方法の変化**を疑う必要がある
#
# 中間として「1回あたりの捕獲数に上限をかけた CPUE」も出す。
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--dbf", "--place", "--splist", "--spc", "--breaks", "--cap",
            "--min-periods", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

SPC_SEL <- strsplit(.opt("--spc", "B804,A312,9904"), ",")[[1]]
BREAKS  <- as.integer(strsplit(.opt("--breaks", "1972,1982,1992,2002,2012"), ",")[[1]])
CAPS    <- as.numeric(strsplit(.opt("--cap", "10,30"), ",")[[1]])
OUT     <- .opt("--out", "results/sparrow_clumping")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
RT     <- if (exists("DATA_ROOT")) DATA_ROOT else "../.."
DBF    <- .opt("--dbf",    file.path(RT, "6118LANDBIRD.DBF"))
PLACE  <- .opt("--place",  file.path(RT, "PLACE.DBF"))
SPLIST <- .opt("--splist", file.path(RT, "Splist.DBF"))
if (!file.exists(DBF)) stop("見つかりません: ", DBF)

## ⚠ Sys.setlocale は呼ばない（UTF-8 ファイルではパースが壊れる。CLAUDE.md）
suppressMessages({ library(dplyr); library(foreign) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 76))

say("=========================================================================")
say("  捕獲の塊性と、塊性に影響されないトレンド")
say("=========================================================================")

# ===========================================================================
# §1 読み込み
# ===========================================================================
d <- read.dbf(DBF, as.is = TRUE)
d$YEAR <- as.integer(substr(as.character(d$DAY), 1, 4))
d$SPC  <- as.character(d$SPC)
d$PCODE <- as.character(d$PCODE)
say(sprintf("  %s 行 / 年 %d〜%d", format(nrow(d), big.mark = ","),
            min(d$YEAR, na.rm = TRUE), max(d$YEAR, na.rm = TRUE)))

fix_enc <- function(x) { y <- suppressWarnings(iconv(x, "CP932", "UTF-8"))
                         ifelse(is.na(y) | !nzchar(y), x, y) }
SPMAP <- NULL
if (file.exists(SPLIST)) {
  sl <- read.dbf(SPLIST, as.is = TRUE)
  if (all(c("SPC", "SPNAMK") %in% names(sl))) {
    sl$SPC <- as.character(sl$SPC); sl$SPNAMK <- fix_enc(as.character(sl$SPNAMK))
    SPMAP <- setNames(sl$SPNAMK, sl$SPC)[!duplicated(sl$SPC)]
  }
}
LABEL <- function(s) if (is.null(SPMAP) || is.na(SPMAP[s])) s else
  sprintf("%s(%s)", s, SPMAP[s])
miss <- setdiff(SPC_SEL, unique(d$SPC))
if (length(miss)) stop("無い SPC: ", paste(miss, collapse = ", "))
say("  対象: ", paste(vapply(SPC_SEL, LABEL, ""), collapse = " / "))

## 期間
bk <- c(BREAKS, max(d$YEAR, na.rm = TRUE) + 1L)
d$.period <- cut(d$YEAR, breaks = bk, right = FALSE,
                 labels = sprintf("%d-%d", bk[-length(bk)], bk[-1] - 1L))
d <- d[!is.na(d$.period), , drop = FALSE]
say("  期間: ", paste(levels(d$.period), collapse = " / "))

## 調査日 = (ステーション, 日) の組。**これが努力の単位**
d$.ev <- paste(d$PCODE, d$DAY, sep = "")
eff_all <- unique(d[, c(".ev", "YEAR", ".period", "PCODE")])
say(sprintf("  調査日（ステーション × 日）: %s", format(nrow(eff_all), big.mark = ",")))

# ===========================================================================
# §2 捕獲の塊性
# ===========================================================================
say("")
hr(); say("§2 1回の調査日あたり何羽捕れているか"); hr()
say("  **スズメは冬にヨシ原で大群のねぐらを取る。** 1回の捕獲が極端に大きいと")
say("  CPUE が少数の事象に支配され、ポアソン分布の仮定も壊れる。")
say("")
say(sprintf("  %-14s %10s %8s %8s %8s %10s %12s %10s",
            "種", "捕獲事象", "中央", "平均", "最大", "分散/平均",
            "上位10件の割合", "上位1%の割合"))
clump <- list()
for (s in SPC_SEL) {
  x <- d[d$SPC == s, , drop = FALSE]
  ev <- as.integer(table(x$.ev))                 # 事象ごとの捕獲数
  tot <- sum(ev)
  o <- sort(ev, decreasing = TRUE)
  n1 <- max(1L, ceiling(length(o) * 0.01))
  say(sprintf("  %-14s %10s %8.0f %8.2f %8s %10.1f %12s %10s",
              LABEL(s), format(length(ev), big.mark = ","),
              stats::median(ev), mean(ev), format(max(ev), big.mark = ","),
              stats::var(ev) / mean(ev),
              sprintf("%.1f%%", 100 * sum(utils::head(o, 10)) / tot),
              sprintf("%.1f%%", 100 * sum(utils::head(o, n1)) / tot)))
  clump[[s]] <- data.frame(spc = s, n_event = length(ev), total = tot,
                           med = stats::median(ev), mean = mean(ev), max = max(ev),
                           vmr = stats::var(ev) / mean(ev),
                           top10_share = sum(utils::head(o, 10)) / tot,
                           top1pct_share = sum(utils::head(o, n1)) / tot)
}
say("")
say("  ★ **分散/平均** が 1 ならポアソン。大きいほど塊性が強い")
say("    **上位10件の割合** が大きければ、数回の大量捕獲が全体を決めている")

## 大量捕獲が時代とともに変わったか
say("")
say("  ── 大きな捕獲事象の時代変化（上位1%の閾値を超えた事象の数） ──")
for (s in SPC_SEL) {
  x <- d[d$SPC == s, , drop = FALSE]
  ev <- table(x$.ev)
  thr <- stats::quantile(as.integer(ev), 0.99, names = FALSE)
  evdf <- data.frame(ev = names(ev), n = as.integer(ev), stringsAsFactors = FALSE)
  pd <- unique(x[, c(".ev", ".period")]); names(pd)[1] <- "ev"
  evdf <- merge(evdf, pd, by = "ev")
  say(sprintf("    %-14s 閾値 %3.0f 羽以上:", LABEL(s), thr))
  for (p in levels(d$.period)) {
    z <- evdf[evdf$.period == p, , drop = FALSE]
    big <- z[z$n >= thr, , drop = FALSE]
    say(sprintf("      %-10s 事象 %6s / うち大 %4s（%4.1f%%）/ 大が占める羽数 %5.1f%%",
                p, format(nrow(z), big.mark = ","), format(nrow(big), big.mark = ","),
                if (nrow(z)) 100 * nrow(big) / nrow(z) else 0,
                if (sum(z$n)) 100 * sum(big$n) / sum(z$n) else 0))
  }
}

# ===========================================================================
# §3 ★ 塊性に影響されない指標でのトレンド
# ===========================================================================
say("")
hr(); say("§3 出現率でもトレンドは残るか（本命の検査）"); hr()
say("  **出現率 = 1羽以上捕れた調査日の数 ÷ 全調査日**")
say("  1回の捕獲数に一切影響されない。大量捕獲が1回あっても1日は1日。")

## 継続地点（全期間で稼働）も同時に見る
np <- length(levels(d$.period))
MINP <- as.integer(.opt("--min-periods", as.character(np)))
n_pd <- table(unique(d[, c("PCODE", ".period")])$PCODE)
full <- names(n_pd)[n_pd >= MINP]
say(sprintf("  継続地点（%d 期間以上）: %s 箇所", MINP, format(length(full), big.mark = ",")))

trend_table <- function(sub, tag) {
  eff <- unique(sub[, c(".ev", ".period")])
  say("")
  say("  ── ", tag, " ──")
  hdr <- sprintf("    %-10s %10s", "期間", "調査日")
  for (s in SPC_SEL) hdr <- paste0(hdr, sprintf(" %22s", LABEL(s)))
  say(hdr)
  say(sprintf("    %-10s %10s%s", "", "",
              paste(rep(sprintf(" %22s", "CPUE / 出現率 / 上限付"), length(SPC_SEL)),
                    collapse = "")))
  out <- list()
  for (p in levels(d$.period)) {
    ne <- sum(eff$.period == p)
    line <- sprintf("    %-10s %10s", p, format(ne, big.mark = ","))
    for (s in SPC_SEL) {
      x <- sub[sub$SPC == s & sub$.period == p, , drop = FALSE]
      cnt <- table(x$.ev)
      cpue <- if (ne) sum(cnt) / ne else NA_real_
      occ  <- if (ne) length(cnt) / ne else NA_real_
      capv <- if (ne) sum(pmin(as.integer(cnt), CAPS[1])) / ne else NA_real_
      line <- paste0(line, sprintf(" %6.3f /%6.3f /%6.3f", cpue, occ, capv))
      out[[length(out) + 1L]] <- data.frame(scope = tag, period = p, spc = s,
        n_day = ne, n_record = sum(cnt), n_event = length(cnt),
        cpue = cpue, occ = occ, cpue_cap = capv)
    }
    say(line)
  }
  do.call(rbind, out)
}

res1 <- trend_table(d, "全地点")
res2 <- if (length(full)) trend_table(d[d$PCODE %in% full, , drop = FALSE],
                                      sprintf("継続%d地点", length(full))) else NULL

say("")
say(sprintf("  ＊ 「CPUE / 出現率 / 上限%.0f羽のCPUE」", CAPS[1]))
say("")
say("  ★ 読み方")
say("    **出現率でも同じように減っていれば、減少は本物。**")
say("      大量捕獲の有無では説明できない")
say("    **出現率は横ばいで CPUE だけ減っていれば、**")
say("      「捕れる日数は変わらず1回あたりが減った」＝")
say("      ねぐらの規模や捕獲方法の変化を疑う必要がある")

# ===========================================================================
# §4 年ごと ＋ 図
# ===========================================================================
say("")
hr(); say("§4 年ごと（図）"); hr()
yrs <- sort(unique(d$YEAR))
mk <- function(sub) {
  eff <- unique(sub[, c(".ev", "YEAR")])
  sapply(SPC_SEL, function(s) {
    vapply(yrs, function(y) {
      ne <- sum(eff$YEAR == y); if (!ne) return(c(NA, NA, NA))
      cnt <- table(sub$.ev[sub$SPC == s & sub$YEAR == y])
      c(sum(cnt) / ne, length(cnt) / ne, sum(pmin(as.integer(cnt), CAPS[1])) / ne)
    }, numeric(3))
  }, simplify = "array")
}
A <- mk(d)      # [3, 年, 種]

FIG <- "reports/figures/sparrow-fig-clumping.png"
dir.create(dirname(FIG), showWarnings = FALSE, recursive = TRUE)
png(FIG, width = 1250, height = 420 * length(SPC_SEL), res = 112)
op <- par(mfrow = c(length(SPC_SEL), 1), mar = c(3.5, 4.5, 2.5, 1))
for (j in seq_along(SPC_SEL)) {
  m <- t(A[, , j])                       # 年 × 3
  ## 初期値で基準化して**形**を比べる（水準は意味が違うので）
  base <- apply(m, 2, function(v) { v <- v[is.finite(v) & v > 0]
                                    if (length(v)) v[1] else NA })
  mm <- sweep(m, 2, base, "/")
  matplot(yrs, mm, type = "n", log = "y", las = 1, xlab = "",
          ylab = "初期値を1とした比（対数）",
          main = sprintf("%s — CPUE / 出現率 / 上限%.0f羽のCPUE",
                         LABEL(SPC_SEL[j]), CAPS[1]))
  abline(h = 1, col = "grey80")
  cl <- c("#e31a1c", "#1f78b4", "#33a02c")
  for (k in 1:3) { lines(yrs, mm[, k], col = cl[k], lwd = 2)
                   points(yrs, mm[, k], col = cl[k], pch = 16, cex = 0.4) }
  legend("bottomleft", legend = c("CPUE", "**出現率**", sprintf("上限%.0f羽", CAPS[1])),
         col = cl, lwd = 2, bty = "n", cex = 0.85)
}
par(op); dev.off()
say("  図: ", FIG)
say("")
say("  ★ **3本が重なっていれば、塊性はトレンドを歪めていない。**")
say("    CPUE だけが下に離れていれば、大量捕獲の減少が効いている")

# ===========================================================================
# §5 保存
# ===========================================================================
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(do.call(rbind, clump), paste0(OUT, "_events.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(rbind(res1, res2), paste0(OUT, "_trend.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
ann <- do.call(rbind, lapply(seq_along(SPC_SEL), function(j)
  data.frame(spc = SPC_SEL[j], year = yrs,
             cpue = A[1, , j], occ = A[2, , j], cpue_cap = A[3, , j])))
write.csv(ann, paste0(OUT, "_annual.csv"), row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, "_events.csv / _trend.csv / _annual.csv")
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
say("")
say("  ★ 判断")
say("    分散/平均 が大きく、上位10件の割合も大きければ **塊性は強い**")
say("      → ADCR の `pcap_poisson` の仮定は成り立たない。")
say("        負の二項分布の GLM のほうが正直かもしれない")
say("    それでも **出現率でトレンドが残れば、減少の主張は保たれる**")
