# examples/plot_region_box.R -------------------------------------------------
#
# **`band_region_scan.R` が選んだ解析範囲が日本のどこにあるかを図にする。**
#
#   Rscript examples/plot_region_box.R                      # つながり11本の最良
#   Rscript examples/plot_region_box.R --npair 14
#   Rscript examples/plot_region_box.R --width 800 --res 20
#
# 生データを**読むだけ**。書き出すのは reports/figures/region-fig-*.png と
# --out の CSV だけ。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-29）
#
# `examples/band_region_scan.R` の走査で、
#
#   W 800km / r 20km : ncell 1,600 / nind 23,507 / neffort 1,291
#                      つながり **11本** / SGD 300反復 **32.7日**
#
# という折り合い点が見つかった（全国10kmの 14本・1,332日 に対し40倍速い）。
#
# **だが、その箱が日本のどこを覆っているのかを見ていない。**
#
#   - 北海道か、本州か、どこか
#   - 密度推定の対象がその範囲に限定されるので、**何の個体数を推定するのか**が決まる
#   - 端が海ばかりなら、実質的な面積はもっと小さい
#
# ## もう1つ確かめること: **位置に対する頑健性**
#
# 11本取れる箱が**1つしか無い**なら、その解析は脆い。
# **多くの置き場所で11本取れる**なら、位置の選択は本質ではない。
# 候補をまとめて描いて判断する。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--csv", "--place", "--scan", "--species", "--sp-col",
            "--npair", "--width", "--res", "--land", "--prefix", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

SPECIES <- .opt("--species", "ｼｼﾞｭｳｶﾗ")  # ｼｼﾞｭｳｶﾗ
SP_COL  <- .opt("--sp-col", "SPNAMK")
NPAIR   <- as.integer(.opt("--npair", "11"))
W_FIX   <- .opt("--width", NULL)
R_FIX   <- .opt("--res", NULL)
SCAN    <- .opt("--scan", "results/band_region_scan.csv")
PREFIX  <- .opt("--prefix", "region-fig")
OUT     <- .opt("--out", "results/band_region_box.csv")
FIGDIR  <- "reports/figures"

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
CSV   <- .opt("--csv",   if (exists("BAND_CSV")) BAND_CSV else NA_character_)
PLACE <- .opt("--place", file.path(if (exists("DATA_ROOT")) DATA_ROOT else "../..",
                                   "PLACE.DBF"))
for (p in c(CSV, PLACE, SCAN))
  if (is.na(p) || !file.exists(p)) stop("見つかりません: ", p)

LAND_CAND <- c(.opt("--land", ""), if (exists("LAND_SHP")) LAND_SHP else NULL,
               "../../griddata/QGIS/poly_20251210.shp",
               "../../griddata/Japan_merge2.shp")
LAND <- LAND_CAND[nzchar(LAND_CAND) & file.exists(LAND_CAND)][1]

## ⚠ Sys.setlocale は呼ばない（UTF-8 ファイルではパースが壊れる。CLAUDE.md）
suppressMessages({ library(sf); library(dplyr); library(readr); library(foreign) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 74))

say("=========================================================================")
say("  解析範囲の位置を確認する: ", SPECIES)
say("=========================================================================")

# ===========================================================================
# §1 走査の結果から箱を選ぶ
# ===========================================================================
scan <- read.csv(SCAN, fileEncoding = "UTF-8")
say(sprintf("  走査の結果: %s 構成（%s）", format(nrow(scan), big.mark = ","), SCAN))

cand <- scan[scan$n_pair >= NPAIR, , drop = FALSE]
if (!is.null(W_FIX)) cand <- cand[cand$W  == as.numeric(W_FIX), , drop = FALSE]
if (!is.null(R_FIX)) cand <- cand[cand$res == as.numeric(R_FIX), , drop = FALSE]
if (!nrow(cand)) stop("条件に合う構成がありません: つながり >= ", NPAIR)

best <- cand[which.min(cand$sgd_day), , drop = FALSE]
say("")
say("  ★ 選ばれた構成")
say(sprintf("    幅 %.0f km / 解像度 %.1f km / ncell %s",
            best$W, best$res, format(best$ncell, big.mark = ",")))
say(sprintf("    nind %s / neffort %s / **つながり %d 本**",
            format(best$nind, big.mark = ","), format(best$n_eff, big.mark = ","),
            best$n_pair))
say(sprintf("    SGD 300反復 %.1f 日（多点出発3点で %.0f 日）",
            best$sgd_day, best$sgd_day * 3))

## ★ 位置に対する頑健性。**同じ W・res で 11本以上取れる箱がいくつあるか**
same <- scan[scan$W == best$W & scan$res == best$res & scan$n_pair >= NPAIR, , drop = FALSE]
say("")
say(sprintf("  同じ幅・解像度で つながり %d 本以上になる箱: **%s 通り**",
            NPAIR, format(nrow(same), big.mark = ",")))
## ★ **個数ではなく「広がり」で判定する**（2026-10-01 に直した）。
## 6通りあっても中心が 20km × 10km に固まっていれば、箱の幅 800km に対して
## 2.5% / 1.25% で**実質的に1点**。個数だけを見ると誤った安心をする
sx <- diff(range(same$cx)); sy <- diff(range(same$cy))
say(sprintf("    中心の広がり: x %.0f km / y %.0f km（箱の幅 %.0f km に対して %.1f%% / %.1f%%）",
            sx, sy, best$W, 100 * sx / best$W, 100 * sy / best$W))
if (max(sx, sy) < 0.25 * best$W) {
  say("    → **実質的に1点。位置に対して脆い。**")
  say("      「11本取れる箱を探して当てた」形になるので、")
  say("      **なぜその範囲かを事後的にしか説明できない**")
} else {
  say("    → 置き場所に幅がある。位置の選択は本質的でない")
}

# ===========================================================================
# §2 データ（band_region_scan.R と同じ経路）
# ===========================================================================
band <- suppressWarnings(read_csv(CSV,
  col_types = cols(PCODE = col_character(), RING = "character",
                   GUID = "character", SPC = "character"),
  locale = locale(encoding = "shift_jis"), progress = FALSE))
place <- read.dbf(PLACE, as.is = TRUE)
band  <- band  %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))
place <- place %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE)) %>%
  filter(PCODE != 380036)
source("functions.R", encoding = "UTF-8")
place$Lat <- sapply(place$LAT, convert_to_decimal)
place$Lon <- sapply(place$LONG, convert_to_decimal)
place <- place[is.finite(place$Lat) & is.finite(place$Lon), , drop = FALSE]

pts <- place %>% dplyr::select(PCODE, Lat, Lon) %>% distinct(PCODE, .keep_all = TRUE) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% st_transform(3100)
xy <- st_coordinates(pts) / 1000
stxy <- data.frame(PCODE = pts$PCODE, x = xy[, 1], y = xy[, 2], stringsAsFactors = FALSE)

d <- band %>% filter(.data[[SP_COL]] == SPECIES) %>%
  dplyr::select(PCODE, GUID, RING, YEAR) %>% inner_join(stxy, by = "PCODE")
d$ind <- paste(d$GUID, d$RING, sep = "")

u  <- unique(d[, c("ind", "PCODE", "x", "y")])
nn <- table(u$ind)
um <- u[u$ind %in% names(nn)[nn >= 2L], , drop = FALSE]
eff <- band %>% dplyr::select(PCODE, YEAR) %>% distinct() %>% inner_join(stxy, by = "PCODE")

# ===========================================================================
# §3 箱の中身と、緯度経度での範囲
# ===========================================================================
h <- best$W / 2; x0 <- best$cx; y0 <- best$cy
inbox <- function(X, Y) abs(X - x0) <= h & abs(Y - y0) <= h

say("")
hr(); say("§3 箱の中身"); hr()

## 投影座標 → 緯度経度（人が読める形に）
corner <- st_sfc(st_point(c((x0 - h) * 1000, (y0 - h) * 1000)),
                 st_point(c((x0 + h) * 1000, (y0 + h) * 1000)), crs = 3100)
ll <- st_coordinates(st_transform(corner, 4326))
say(sprintf("  中心 (投影): x %.0f km / y %.0f km", x0, y0))
say(sprintf("  **範囲: 経度 %.2f〜%.2f / 緯度 %.2f〜%.2f**",
            ll[1, 1], ll[2, 1], ll[1, 2], ll[2, 2]))

e_in  <- eff[inbox(eff$x, eff$y), , drop = FALSE]
d_in  <- d[inbox(d$x, d$y), , drop = FALSE]
um_in <- um[inbox(um$x, um$y), , drop = FALSE]
say(sprintf("  ステーション（全種）: %s / 全国 %s",
            format(length(unique(e_in$PCODE)), big.mark = ","),
            format(length(unique(eff$PCODE)), big.mark = ",")))
say(sprintf("  %s の記録 %s / 個体 %s",
            SPECIES, format(nrow(d_in), big.mark = ","),
            format(length(unique(d_in$ind)), big.mark = ",")))

## 箱の中で、その解像度で「別のセル」になる移動
r <- best$res
cid <- function(X, Y) paste(floor(X / r), floor(Y / r), sep = "_")
um_in$cell <- cid(um_in$x, um_in$y)
segs <- do.call(rbind, lapply(split(um_in, um_in$ind), function(z) {
  if (length(unique(z$cell)) < 2L) return(NULL)
  cb <- utils::combn(seq_len(nrow(z)), 2)
  k <- which(z$cell[cb[1, ]] != z$cell[cb[2, ]])
  if (!length(k)) return(NULL)
  data.frame(x1 = z$x[cb[1, k]], y1 = z$y[cb[1, k]],
             x2 = z$x[cb[2, k]], y2 = z$y[cb[2, k]])
}))
say(sprintf("  この解像度で見える移動の線分: %d 本",
            if (is.null(segs)) 0L else nrow(segs)))

# ===========================================================================
# §4 作図
# ===========================================================================
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)
land <- if (!is.na(LAND) && length(LAND))
  st_make_valid(st_union(st_transform(st_read(LAND, quiet = TRUE), 3100))) else NULL
if (is.null(land)) say("  ⚠ 陸のポリゴンが無いので海岸線は描かない")

box_sfc <- function(cx, cy, w) {
  hh <- w / 2
  st_polygon(list(rbind(c(cx - hh, cy - hh), c(cx + hh, cy - hh),
                        c(cx + hh, cy + hh), c(cx - hh, cy + hh),
                        c(cx - hh, cy - hh)) * 1000))
}

## --- 図1: 日本全体の中での位置 ---------------------------------------------
f1 <- file.path(FIGDIR, paste0(PREFIX, "-location.png"))
png(f1, width = 1000, height = 1050, res = 112)
par(mar = c(1, 1, 3, 1))
plot(st_sfc(box_sfc(x0, y0, best$W), crs = 3100), border = NA, col = NA,
     xlim = range(c(eff$x, x0 + c(-h, h))) * 1000,
     ylim = range(c(eff$y, y0 + c(-h, h))) * 1000,
     main = sprintf("%s：解析範囲 %.0fkm 四方 / %.0fkm メッシュ（つながり %d 本）",
                    SPECIES, best$W, best$res, best$n_pair))
if (!is.null(land)) plot(land, add = TRUE, border = "grey55", lwd = 0.5, col = "grey97")
## 11本以上取れる他の置き場所（薄く）— 位置に対する頑健性が見える
if (nrow(same) > 1)
  for (i in seq_len(min(nrow(same), 400)))
    plot(st_sfc(box_sfc(same$cx[i], same$cy[i], best$W), crs = 3100),
         add = TRUE, border = "#1f78b433", col = NA, lwd = 0.6)
points(eff$x * 1000, eff$y * 1000, pch = 16, cex = 0.18, col = "grey60")
points(e_in$x * 1000, e_in$y * 1000, pch = 16, cex = 0.28, col = "#33a02c")
plot(st_sfc(box_sfc(x0, y0, best$W), crs = 3100), add = TRUE,
     border = "#e31a1c", col = NA, lwd = 3)
if (!is.null(segs))
  segments(segs$x1 * 1000, segs$y1 * 1000, segs$x2 * 1000, segs$y2 * 1000,
           col = "#e31a1c", lwd = 1.8)
legend("topleft", bty = "n", cex = 0.82,
       legend = c("標識ステーション（全国）", "箱の中のステーション",
                  sprintf("観測される移動 %d 本", if (is.null(segs)) 0L else nrow(segs)),
                  sprintf("つながり%d本以上になる他の置き場所（%s通り）",
                          NPAIR, format(nrow(same), big.mark = ","))),
       pch = c(16, 16, NA, NA), col = c("grey60", "#33a02c", "#e31a1c", "#1f78b4"),
       lwd = c(NA, NA, 1.8, 0.6))
dev.off(); say("  図1: ", f1)

## --- 図2: 箱の中の拡大 ------------------------------------------------------
f2 <- file.path(FIGDIR, paste0(PREFIX, "-zoom.png"))
png(f2, width = 1000, height = 1000, res = 112)
par(mar = c(1, 1, 3, 1))
plot(st_sfc(box_sfc(x0, y0, best$W), crs = 3100), border = "#e31a1c", col = NA, lwd = 2,
     main = sprintf("箱の中（%.0fkm 四方 / ncell %s / 個体 %s）",
                    best$W, format(best$ncell, big.mark = ","),
                    format(length(unique(d_in$ind)), big.mark = ",")))
if (!is.null(land)) plot(land, add = TRUE, border = "grey55", lwd = 0.6, col = "grey97")
## メッシュの線（間引いて描く。1,600本は多いので10セルおき）
gx <- seq(x0 - h, x0 + h, by = r); gy <- seq(y0 - h, y0 + h, by = r)
st <- max(1, round(length(gx) / 20))
abline(v = gx[seq(1, length(gx), by = st)] * 1000, col = "grey90", lwd = 0.4)
abline(h = gy[seq(1, length(gy), by = st)] * 1000, col = "grey90", lwd = 0.4)
points(e_in$x * 1000, e_in$y * 1000, pch = 16, cex = 0.5, col = "#33a02c")
if (!is.null(segs)) {
  segments(segs$x1 * 1000, segs$y1 * 1000, segs$x2 * 1000, segs$y2 * 1000,
           col = "#e31a1c", lwd = 2.2)
  points(c(segs$x1, segs$x2) * 1000, c(segs$y1, segs$y2) * 1000,
         pch = 16, cex = 0.7, col = "#e31a1c")
}
plot(st_sfc(box_sfc(x0, y0, best$W), crs = 3100), add = TRUE,
     border = "#e31a1c", col = NA, lwd = 2)
legend("topleft", bty = "n", cex = 0.85,
       legend = c(sprintf("ステーション %s", format(length(unique(e_in$PCODE)), big.mark = ",")),
                  sprintf("移動 %d 本", if (is.null(segs)) 0L else nrow(segs)),
                  sprintf("グリッド線は %.0f セルおき", st)),
       pch = c(16, 16, NA), col = c("#33a02c", "#e31a1c", NA), lwd = c(NA, 2.2, NA))
dev.off(); say("  図2: ", f2)

# ===========================================================================
# §5 保存
# ===========================================================================
out <- data.frame(
  species = SPECIES, W = best$W, res = best$res, cx = x0, cy = y0,
  lon_min = ll[1, 1], lon_max = ll[2, 1], lat_min = ll[1, 2], lat_max = ll[2, 2],
  ncell = best$ncell, n_eff = best$n_eff, nind = best$nind, n_pair = best$n_pair,
  loglf_sec = best$loglf_sec, sgd_day = best$sgd_day,
  n_station_in = length(unique(e_in$PCODE)),
  n_ind_in = length(unique(d_in$ind)),
  n_alt_boxes = nrow(same))
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT, row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT)
say("")
say("  ⚠ **密度推定の対象はこの箱の中に限定される。**")
say("    「日本全国の個体数」ではなく「この範囲の個体数」になる。")
