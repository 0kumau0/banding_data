# examples/sparrow_season.R --------------------------------------------------
#
# **網かけの季節が変わっていないかを確かめる。**
#
#   Rscript examples/sparrow_season.R
#   Rscript examples/sparrow_season.R --spc B804 --all-sites
#
# 生データを**読むだけ**。書き出すのは --out の CSV と図。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-10-01）
#
# スズメの減少は**アシ原に限定**されていた（継続70地点、層別の努力で割った出現率）:
#
#   アシ原    0.601 → 0.315   **1.9分の1**
#   その他    0.105 → 0.153   **1.46倍に増加**
#
# しかも**同じアシ原で対照種は増えている**:
#
#   スズメ        0.601 → 0.315   0.52倍
#   シジュウカラ  0.108 → 0.240   2.2倍
#   ウグイス      0.297 → 0.443   1.49倍
#
# **「アシ原での網かけが減った」では説明できない**（努力で割ってあり、
# 対照種は増えている）。
#
# ## 残る説明 — **季節の移動**
#
#   - **スズメがアシ原で捕れるのは冬のねぐら**（11〜2月）
#   - ウグイス・シジュウカラがアシ原で捕れるのは渡り期・越冬期で**季節が違う**
#
# **網かけの季節が冬から秋（渡り期）へ移っていれば:**
#
#   スズメ（冬のねぐら）の捕獲は減る
#   渡り鳥の捕獲は増える
#   **努力量（日数）で割っても補正されない**
#
# **観測されたパターンと完全に一致する。** これを確かめる。
#
# ---------------------------------------------------------------------------
# ★ 決定的な検査
#
#   1. **月ごとの努力量の時代変化**（アシ原 / その他）
#      → 冬の網かけが減っていれば、それだけで説明がつく
#   2. **月ごとのスズメの出現率の時代変化**
#      → **同じ月の中でも減っていれば、季節の移動では説明できない**
#
# 2 が本命。**月を揃えて比べるのが、交絡を断つ唯一の方法。**
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--dbf", "--place", "--splist", "--spc", "--breaks",
            "--min-periods", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--all-sites", "--reed-only"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--all-sites", "--reed-only"),
                                               collapse = " "))

SPC_SEL  <- strsplit(.opt("--spc", "B804,A312,9904"), ",")[[1]]
BREAKS   <- as.integer(strsplit(.opt("--breaks", "1972,1982,1992,2002,2012"), ",")[[1]])
ALLSITE  <- "--all-sites" %in% .args
REEDONLY <- "--reed-only" %in% .args
OUT      <- .opt("--out", "results/sparrow_season")

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
hr  <- function() say(strrep("-", 78))

say("=========================================================================")
say("  網かけの季節は変わったか")
say("=========================================================================")

# ===========================================================================
# §1 読み込み
# ===========================================================================
d <- read.dbf(DBF, as.is = TRUE)
dy <- as.character(d$DAY)
d$YEAR  <- as.integer(substr(dy, 1, 4))
d$MONTH <- as.integer(substr(dy, 5, 6))
d$SPC   <- as.character(d$SPC)
d$PCODE <- as.character(d$PCODE)
d <- d[!is.na(d$MONTH) & d$MONTH >= 1 & d$MONTH <= 12, , drop = FALSE]

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

bk <- c(BREAKS, max(d$YEAR, na.rm = TRUE) + 1L)
d$.period <- cut(d$YEAR, breaks = bk, right = FALSE,
                 labels = sprintf("%d-%d", bk[-length(bk)], bk[-1] - 1L))
d <- d[!is.na(d$.period), , drop = FALSE]
d$.ev <- paste(d$PCODE, d$DAY, sep = "")

np <- length(levels(d$.period))
MINP <- as.integer(.opt("--min-periods", as.character(np)))
n_pd <- table(unique(d[, c("PCODE", ".period")])$PCODE)
full <- names(n_pd)[n_pd >= MINP]
if (!ALLSITE && length(full)) {
  d <- d[d$PCODE %in% full, , drop = FALSE]
  say(sprintf("  **継続%s地点に限定**", format(length(full), big.mark = ",")))
}

## アシ原かどうか
if (file.exists(PLACE)) {
  pl <- read.dbf(PLACE, as.is = TRUE); pl$PCODE <- as.character(pl$PCODE)
  if ("HABITAT" %in% names(pl)) {
    hb <- pl %>% dplyr::select(PCODE, HABITAT) %>%
      mutate(HABITAT = fix_enc(as.character(HABITAT))) %>% distinct(PCODE, .keep_all = TRUE)
    d <- d %>% left_join(hb, by = "PCODE")
    d$.reed <- grepl("アシ|ｱｼ|ヨシ|ﾖｼ",
                     ifelse(is.na(d$HABITAT), "", d$HABITAT))
  } else d$.reed <- FALSE
} else d$.reed <- FALSE
if (REEDONLY) { d <- d[d$.reed, , drop = FALSE]; say("  **アシ原の地点だけに限定**") }
say(sprintf("  %s 行 / 期間 %s", format(nrow(d), big.mark = ","),
            paste(levels(d$.period), collapse = " / ")))

eff <- unique(d[, c(".ev", ".period", "MONTH", ".reed")])

# ===========================================================================
# §2 月ごとの努力量の時代変化
# ===========================================================================
say("")
hr(); say("§2 月ごとの努力量（調査日数）の構成比"); hr()
say("  **冬（11〜2月）の割合が下がっていれば、それだけでスズメの減少を説明できる**")
for (rd in if (any(d$.reed)) c(TRUE, FALSE) else FALSE) {
  say("")
  say("  ── ", if (rd) "アシ原" else "その他", " ──")
  say(sprintf("    %-10s %8s %s", "期間", "調査日",
              paste(sprintf("%5d", 1:12), collapse = "")))
  for (p in levels(d$.period)) {
    z <- eff[eff$.period == p & eff$.reed == rd, , drop = FALSE]
    if (!nrow(z)) next
    tb <- table(factor(z$MONTH, levels = 1:12))
    say(sprintf("    %-10s %8s %s", p, format(nrow(z), big.mark = ","),
                paste(sprintf("%4.0f%%", 100 * as.integer(tb) / nrow(z)), collapse = "")))
  }
  ## 冬の割合
  say(sprintf("    %-10s %8s  冬(11-2月)の割合:", "", ""))
  for (p in levels(d$.period)) {
    z <- eff[eff$.period == p & eff$.reed == rd, , drop = FALSE]
    if (!nrow(z)) next
    w <- mean(z$MONTH %in% c(11, 12, 1, 2))
    say(sprintf("      %-10s %5.1f%%", p, 100 * w))
  }
}

# ===========================================================================
# §3 ★ 月を揃えた出現率 — 本命
# ===========================================================================
say("")
hr(); say("§3 月を揃えたときのスズメの出現率（アシ原）"); hr()
say("  **同じ月の中でも減っていれば、季節の移動では説明できない。**")
say("  月をまたいだ比較は交絡しているので、ここが判定の核心。")

rows <- list()
for (s in SPC_SEL) {
  for (rd in if (any(d$.reed)) c(TRUE, FALSE) else FALSE) {
    say("")
    say(sprintf("  【%s】 %s", LABEL(s), if (rd) "アシ原" else "その他"))
    say(sprintf("    %-10s %s", "期間",
                paste(sprintf("%6d", 1:12), collapse = "")))
    for (p in levels(d$.period)) {
      line <- sprintf("    %-10s", p)
      for (m in 1:12) {
        ne <- sum(eff$.period == p & eff$.reed == rd & eff$MONTH == m)
        if (ne < 20) { line <- paste0(line, sprintf("%6s", "·")); next }
        nv <- length(unique(d$.ev[d$SPC == s & d$.period == p &
                                  d$.reed == rd & d$MONTH == m]))
        line <- paste0(line, sprintf("%6.3f", nv / ne))
        rows[[length(rows) + 1L]] <- data.frame(spc = s, reed = rd, period = p,
          month = m, n_day = ne, n_event = nv, occ = nv / ne)
      }
      say(line)
    }
  }
}
say("")
say("  ＊ 「·」は調査日が20未満で信頼できない月")

# ===========================================================================
# §4 冬だけに絞った比較
# ===========================================================================
say("")
hr(); say("§4 冬（11〜2月）だけに絞った比較"); hr()
say("  季節の移動の影響を断つ最も簡単な方法。")
say(sprintf("    %-10s %10s %s", "期間", "冬の調査日",
            paste(sprintf("%16s", vapply(SPC_SEL, LABEL, "")), collapse = "")))
win <- d[d$MONTH %in% c(11, 12, 1, 2), , drop = FALSE]
effw <- unique(win[, c(".ev", ".period", ".reed")])
for (rd in if (any(d$.reed)) c(TRUE, FALSE) else FALSE) {
  say("")
  say("  ── ", if (rd) "アシ原" else "その他", " ──")
  for (p in levels(d$.period)) {
    ne <- sum(effw$.period == p & effw$.reed == rd)
    if (!ne) next
    line <- sprintf("    %-10s %10s", p, format(ne, big.mark = ","))
    for (s in SPC_SEL) {
      nv <- length(unique(win$.ev[win$SPC == s & win$.period == p & win$.reed == rd]))
      cn <- sum(win$SPC == s & win$.period == p & win$.reed == rd)
      line <- paste0(line, sprintf("  %6.3f /%6.2f", nv / ne, cn / ne))
    }
    say(line)
  }
}
say("")
say("  ＊ 「出現率 / CPUE」")
say("")
say("  ★ **冬のアシ原に絞ってもスズメだけが減っていれば、")
say("    季節の移動では説明できない。減少は本物。**")

# ===========================================================================
# §5 図
# ===========================================================================
FIG <- "reports/figures/sparrow-fig-season.png"
dir.create(dirname(FIG), showWarnings = FALSE, recursive = TRUE)
png(FIG, width = 1250, height = 800, res = 112)
op <- par(mfrow = c(2, 1), mar = c(3.8, 4.5, 2.5, 1))
cols <- grDevices::hcl.colors(np, "Zissou 1")
## 上: 月ごとの努力の構成（アシ原）
plot(1:12, rep(NA, 12), ylim = c(0, 0.45), xlab = "", las = 1, xaxt = "n",
     ylab = "調査日の割合", main = "月ごとの努力の構成（アシ原）")
axis(1, at = 1:12)
for (i in seq_len(np)) {
  p <- levels(d$.period)[i]
  z <- eff[eff$.period == p & eff$.reed, , drop = FALSE]
  if (!nrow(z)) next
  tb <- table(factor(z$MONTH, levels = 1:12))
  lines(1:12, as.integer(tb) / nrow(z), col = cols[i], lwd = 2.2)
}
legend("topright", legend = levels(d$.period), col = cols, lwd = 2.2, bty = "n", cex = 0.8)
## 下: 冬のアシ原での出現率
par(mar = c(4.5, 4.5, 2.5, 1))
mw <- sapply(SPC_SEL, function(s) vapply(levels(d$.period), function(p) {
  ne <- sum(effw$.period == p & effw$.reed)
  if (!ne) return(NA_real_)
  length(unique(win$.ev[win$SPC == s & win$.period == p & win$.reed])) / ne
}, numeric(1)))
matplot(seq_len(np), mw, type = "b", pch = 16, lwd = 2.2, las = 1, xaxt = "n",
        col = c("#e31a1c", "#1f78b4", "#33a02c"), xlab = "", ylab = "出現率",
        main = "冬（11〜2月）のアシ原での出現率")
axis(1, at = seq_len(np), labels = levels(d$.period), cex.axis = 0.8)
legend("topright", legend = vapply(SPC_SEL, LABEL, ""),
       col = c("#e31a1c", "#1f78b4", "#33a02c"), lwd = 2.2, bty = "n", cex = 0.8)
par(op); dev.off()
say("  図: ", FIG)

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(do.call(rbind, rows), paste0(OUT, ".csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
say("  保存: ", OUT, ".csv")
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
