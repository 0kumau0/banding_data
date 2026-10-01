# examples/sparrow_eventsize.R -----------------------------------------------
#
# **捕獲数の分布全体が左にずれたのか、大きい側だけが消えたのかを分ける。**
#
#   Rscript examples/sparrow_eventsize.R
#   Rscript examples/sparrow_eventsize.R --spc B804 --min-periods 5
#
# 生データを**読むだけ**。書き出すのは --out の CSV と図。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-10-01）
#
# スズメの減少シグナルは、指標によって大きさが食い違う（継続70地点）:
#
#   CPUE            2.161 → 0.538   **4.0分の1**
#   上限10羽のCPUE  1.142 → 0.478   **2.4分の1**
#   出現率          0.264 → 0.187   **1.4分の1（29%減）**
#
# **「捕れる日数」はあまり減らず、「捕れた日の羽数」が大きく減った。**
#
# 考えられるのは2つで、**これを分けたい**:
#
#   (a) **群れの規模が小さくなった**（個体数の実質的な減少）
#       → 捕獲数の**分布全体が左にずれる**。中央値も下がる
#   (b) **捕獲方法が変わった**（網の数・時間、群れが入ったときの対応）
#       → **大きい側だけが消える**。中央値は動かない
#
# 対照種（シジュウカラ・ウグイス）に同じ変化が無ければ、方法の変化では
# 説明しにくくなる。**アシ原かどうかでも分ける**（ねぐらは主にアシ原）。
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--dbf", "--place", "--splist", "--spc", "--breaks",
            "--min-periods", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--all-sites"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--all-sites"), collapse = " "))

SPC_SEL <- strsplit(.opt("--spc", "B804,A312,9904"), ",")[[1]]
BREAKS  <- as.integer(strsplit(.opt("--breaks", "1972,1982,1992,2002,2012"), ",")[[1]])
ALLSITE <- "--all-sites" %in% .args
OUT     <- .opt("--out", "results/sparrow_eventsize")

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
say("  捕獲数の分布 — 全体が左にずれたのか、裾だけ消えたのか")
say("=========================================================================")

# ===========================================================================
# §1 読み込み
# ===========================================================================
d <- read.dbf(DBF, as.is = TRUE)
d$YEAR  <- as.integer(substr(as.character(d$DAY), 1, 4))
d$SPC   <- as.character(d$SPC)
d$PCODE <- as.character(d$PCODE)
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
say(sprintf("  %s 行 / 対象: %s", format(nrow(d), big.mark = ","),
            paste(vapply(SPC_SEL, LABEL, ""), collapse = " / ")))
say("  期間: ", paste(levels(d$.period), collapse = " / "))

## 継続地点に絞る（既定）。**調査地の移動を断ったうえで分布を見る**
np <- length(levels(d$.period))
MINP <- as.integer(.opt("--min-periods", as.character(np)))
n_pd <- table(unique(d[, c("PCODE", ".period")])$PCODE)
full <- names(n_pd)[n_pd >= MINP]
if (!ALLSITE && length(full)) {
  d <- d[d$PCODE %in% full, , drop = FALSE]
  say(sprintf("  **継続%s地点に限定**（--all-sites で全地点）",
              format(length(full), big.mark = ",")))
} else say("  全地点を使う")

## アシ原かどうか（ねぐらは主にアシ原）
if (file.exists(PLACE)) {
  pl <- read.dbf(PLACE, as.is = TRUE)
  pl$PCODE <- as.character(pl$PCODE)
  if ("HABITAT" %in% names(pl)) {
    hb <- pl %>% dplyr::select(PCODE, HABITAT) %>%
      mutate(HABITAT = fix_enc(as.character(HABITAT))) %>%
      distinct(PCODE, .keep_all = TRUE)
    d <- d %>% left_join(hb, by = "PCODE")
    d$.reed <- grepl("アシ|ｱｼ|ヨシ|ﾖｼ",
                     ifelse(is.na(d$HABITAT), "", d$HABITAT))
    say(sprintf("  アシ原の地点の記録: %.1f%%", 100 * mean(d$.reed)))
  } else d$.reed <- NA
} else d$.reed <- NA

# ===========================================================================
# §2 分位点の推移 ← **これが判定の核心**
# ===========================================================================
QS <- c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
say("")
hr(); say("§2 1調査日あたり捕獲数の分位点"); hr()
say("  **中央値も下がっていれば分布全体が左にずれた**（個体数の減少と整合）")
say("  **中央値は動かず上位分位点だけ下がれば裾だけ消えた**（方法の変化を疑う）")

ev_of <- function(x) as.integer(table(x$.ev))
rows <- list()
for (s in SPC_SEL) {
  say("")
  say("  【", LABEL(s), "】")
  say(sprintf("    %-10s %8s %8s %8s %8s %8s %8s %8s %8s",
              "期間", "事象数", "25%", "50%", "75%", "90%", "95%", "99%", "最大"))
  for (p in levels(d$.period)) {
    ev <- ev_of(d[d$SPC == s & d$.period == p, , drop = FALSE])
    if (!length(ev)) next
    q <- stats::quantile(ev, QS, names = FALSE)
    say(sprintf("    %-10s %8s %8.0f %8.0f %8.0f %8.0f %8.0f %8.0f %8s",
                p, format(length(ev), big.mark = ","), q[1], q[2], q[3], q[4], q[5], q[6],
                format(max(ev), big.mark = ",")))
    rows[[length(rows) + 1L]] <- data.frame(spc = s, period = p, n_event = length(ev),
      q25 = q[1], q50 = q[2], q75 = q[3], q90 = q[4], q95 = q[5], q99 = q[6],
      max = max(ev), mean = mean(ev))
  }
  ## ★ 判定: 中央値と上位分位点の変化を比べる
  r <- do.call(rbind, rows); r <- r[r$spc == s, , drop = FALSE]
  if (nrow(r) >= 2) {
    f <- r[1, ]; l <- r[nrow(r), ]
    say(sprintf("    → 中央値 %.0f→%.0f（%.2f倍）/ 95%% %.0f→%.0f（%.2f倍）/ 99%% %.0f→%.0f（%.2f倍）",
                f$q50, l$q50, l$q50 / f$q50, f$q95, l$q95, l$q95 / f$q95,
                f$q99, l$q99, l$q99 / f$q99))
  }
}

# ===========================================================================
# §3 サイズ階級の構成比
# ===========================================================================
say("")
hr(); say("§3 サイズ階級の構成比（事象のうち何%がその大きさか）"); hr()
BRK <- c(0, 1, 3, 10, 30, 100, Inf)
LAB <- c("1", "2-3", "4-10", "11-30", "31-100", "101+")
for (s in SPC_SEL) {
  say("")
  say("  【", LABEL(s), "】")
  say(sprintf("    %-10s %8s %s", "期間", "事象数",
              paste(sprintf("%8s", LAB), collapse = "")))
  for (p in levels(d$.period)) {
    ev <- ev_of(d[d$SPC == s & d$.period == p, , drop = FALSE])
    if (!length(ev)) next
    tb <- table(cut(ev, BRK, labels = LAB))
    say(sprintf("    %-10s %8s %s", p, format(length(ev), big.mark = ","),
                paste(sprintf("%7.1f%%", 100 * as.integer(tb) / length(ev)),
                      collapse = "")))
  }
}

# ===========================================================================
# §4 アシ原かどうかで分ける
# ===========================================================================
if (any(!is.na(d$.reed)) && any(d$.reed, na.rm = TRUE)) {
  say("")
  hr(); say("§4 アシ原かどうかで分けた中央値と95%点"); hr()
  say("  ねぐらは主にアシ原。**アシ原でだけ裾が消えていればねぐらの事情**")
  for (s in SPC_SEL) {
    say("")
    say("  【", LABEL(s), "】")
    say(sprintf("    %-10s %10s %8s %8s %10s %8s %8s", "期間",
                "アシ原 事象", "中央", "95%", "その他 事象", "中央", "95%"))
    for (p in levels(d$.period)) {
      out <- c()
      for (rd in c(TRUE, FALSE)) {
        ev <- ev_of(d[d$SPC == s & d$.period == p & !is.na(d$.reed) & d$.reed == rd, ,
                      drop = FALSE])
        out <- c(out, if (length(ev))
          sprintf("%10s %8.0f %8.0f", format(length(ev), big.mark = ","),
                  stats::median(ev), stats::quantile(ev, 0.95, names = FALSE))
          else sprintf("%10s %8s %8s", "—", "—", "—"))
      }
      say(sprintf("    %-10s %s", p, paste(out, collapse = " ")))
    }
  }
}

# ===========================================================================
# §5 図
# ===========================================================================
FIG <- "reports/figures/sparrow-fig-eventsize.png"
dir.create(dirname(FIG), showWarnings = FALSE, recursive = TRUE)
png(FIG, width = 1250, height = 420 * length(SPC_SEL), res = 112)
op <- par(mfrow = c(length(SPC_SEL), 1), mar = c(3.8, 4.5, 2.5, 1))
res <- do.call(rbind, rows)
cols <- grDevices::hcl.colors(length(levels(d$.period)), "Zissou 1")
for (s in SPC_SEL) {
  r <- res[res$spc == s, , drop = FALSE]
  if (!nrow(r)) next
  m <- as.matrix(r[, c("q25", "q50", "q75", "q90", "q95", "q99")])
  matplot(seq_along(QS), t(m), type = "n", log = "y", xaxt = "n", las = 1,
          xlab = "", ylab = "1調査日あたり捕獲数（対数）",
          main = sprintf("%s — 分位点の推移（線が平行に下がれば分布全体の移動）", LABEL(s)))
  axis(1, at = seq_along(QS), labels = sprintf("%.0f%%", 100 * QS))
  for (i in seq_len(nrow(r))) {
    lines(seq_along(QS), m[i, ], col = cols[i], lwd = 2.2)
    points(seq_along(QS), m[i, ], col = cols[i], pch = 16, cex = 0.7)
  }
  legend("topleft", legend = r$period, col = cols[seq_len(nrow(r))],
         lwd = 2.2, bty = "n", cex = 0.8)
}
par(op); dev.off()
say("")
say("  図: ", FIG)
say("")
say("  ★ **線が平行に下がっていれば分布全体が左へ移動**（個体数の減少と整合）")
say("    **左端が重なったまま右端だけ下がれば裾だけ消えた**（方法の変化を疑う）")

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(res, paste0(OUT, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
say("  保存: ", OUT, ".csv")
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
