# examples/plot_band_kanto.R -------------------------------------------------
#
# **関東に切り出したシジュウカラのデータセットを図にする。**
#
#   Rscript examples/plot_band_kanto.R
#   Rscript examples/plot_band_kanto.R --species ヤマガラ --prefix yamagara-fig
#
# 生データを**読むだけ**。書き出すのは reports/figures/<prefix>-*.png と
# --out の CSV だけ。
#
# ---------------------------------------------------------------------------
# 作る図（reports/20260929_kanto_dataset.md で使う）
#
#   1. location … 日本全体のどこを切り出したか
#   2. effort   … 調査地（検出努力のあるセル）の位置と努力量 ＋ 年ごとの推移
#   3. capture  … 個体が捕獲された位置（セル単位の個体数）
#   4. moves    … 観測された移動（2セル以上で捕獲された個体）
#
# ---------------------------------------------------------------------------
# ★ 前提（間違えると全部ずれる）
#
#   - `effort$meshcode` は **`mesh2_convex7.gpkg` の行番号**。JIS コードではない
#   - 部分メッシュ（関東）との対応付けは **JIS メッシュコード列**で行う。
#     関東の gpkg は独自に 1..1,013 で番号が振られていて、行番号は対応しない
#   - 検出行列は **行 = 努力 / 列 = 個体**
#   - 疎行列には**全要素が 0 のダミー個体が1列**入っている。
#     `m@x > 0` で落とさないと捕獲0回の個体を数えてしまう（2026-09-29）
#
# 詳細は examples/band_subset.R と docs/20260929_作業記録.md
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--species", "--mesh-all", "--mesh-sub", "--rds", "--land",
            "--prefix", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

SPECIES <- .opt("--species", "シジュウカラ")
PREFIX  <- .opt("--prefix", "kanto-fig")
OUT_CSV <- .opt("--out", "results/band_kanto_cells.csv")
FIGDIR  <- "reports/figures"

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")

MESH_ALL <- .opt("--mesh-all", BAND_MESH)
MESH_SUB <- .opt("--mesh-sub", file.path(GRID_ROOT, "mesh2_convex_kanto.gpkg"))
RDS      <- .opt("--rds", BAND_RDS)
for (p in c(MESH_ALL, MESH_SUB, RDS))
  if (!file.exists(p)) stop("見つかりません: ", p)

## 陸のポリゴン。**ネットワークドライブ（S:）にあるので見えない環境がある。**
## 無ければ海岸線を描かないだけで、図自体は作る
LAND_CAND <- c(.opt("--land", ""), if (exists("LAND_SHP")) LAND_SHP else NULL,
               "../../griddata/QGIS/poly_20251210.shp",
               "../../griddata/Japan_merge2.shp")
LAND <- LAND_CAND[nzchar(LAND_CAND) & file.exists(LAND_CAND)][1]

suppressMessages({ library(sf); library(Matrix) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }

# ===========================================================================
# §1 データセットを組み立てる（examples/band_subset.R と同じ手順）
# ===========================================================================
say("== ", SPECIES, " / 関東の図を作る ==")

mesh_all <- st_read(MESH_ALL, quiet = TRUE)
mesh_sub <- st_read(MESH_SUB, quiet = TRUE)
d        <- readRDS(RDS)
effort   <- as.data.frame(d$effort)

i_sp <- which(d$splist == SPECIES)
if (!length(i_sp) || i_sp > length(d$detect_list))
  stop("種が見つかりません: ", SPECIES)
detect <- d$detect_list[[i_sp]]
if (!inherits(detect, "dgCMatrix")) detect <- methods::as(detect, "dgCMatrix")

## 関東に入る convex7 のセル（＝解析に使うセル）。突合は JIS コード
keep_cell <- which(as.character(mesh_all$meshcode) %in% as.character(mesh_sub$meshcode))
cells <- mesh_all[keep_cell, ]                       # 943 セル
ncell <- nrow(cells)

## 努力を関東内に絞る
eff_cell <- as.integer(as.character(effort$meshcode))  # ★ 行番号
keep_eff <- which(eff_cell %in% keep_cell)
cell_new <- match(eff_cell[keep_eff], keep_cell)       # 1..ncell に振り直す

## 個体を絞る（関東内で1回以上捕まった個体）
det_sub  <- detect[keep_eff, , drop = FALSE]
cnt_sub  <- Matrix::colSums(det_sub)
keep_ind <- which(cnt_sub > 0)
det_sub  <- det_sub[, keep_ind, drop = FALSE]

say(sprintf("  セル %s / 努力 %s 行 / 個体 %s / 検出 %s",
            format(ncell, big.mark = ","), format(length(keep_eff), big.mark = ","),
            format(length(keep_ind), big.mark = ","),
            format(sum(cnt_sub[keep_ind]), big.mark = ",")))

# ===========================================================================
# §2 セルごとに集計する
# ===========================================================================
## 努力（行数と量）
n_eff_row <- tabulate(cell_new, nbins = ncell)
eff_amt   <- as.numeric(tapply(effort$effort[keep_eff], factor(cell_new, levels = seq_len(ncell)),
                               sum, default = 0))
eff_amt[is.na(eff_amt)] <- 0

## 捕獲（回数と個体数）。**値 0 の格納要素を落とす**
z <- data.frame(ind = rep.int(seq_len(ncol(det_sub)), diff(det_sub@p)),
                row = det_sub@i + 1L, n = det_sub@x)
z <- z[z$n > 0, , drop = FALSE]
z$cell <- cell_new[z$row]

n_cap <- as.numeric(tapply(z$n, factor(z$cell, levels = seq_len(ncell)), sum, default = 0))
n_cap[is.na(n_cap)] <- 0
n_ind_cell <- tabulate(unique(data.frame(z$ind, z$cell))[, 2], nbins = ncell)

say(sprintf("  検出努力のあるセル %d / 捕獲のあったセル %d",
            sum(n_eff_row > 0), sum(n_cap > 0)))

## 共変量。**透過性モデル（C ~ agri + wtr）の中身そのもの**なので、
## 解析範囲の性質としてここで押さえておく。
## 値は面積（m²）。1セル = 100 km² = 1e8 m² なので、割合に直して見る
cov_frac <- data.frame(agri = cells$cultivated / 1e8, wtr = cells$openwater / 1e8)
say(sprintf("  共変量（943セル、割合に換算）: 農地 平均 %.3f（0〜%.3f）/ 水域 平均 %.3f（0〜%.3f）",
            mean(cov_frac$agri), max(cov_frac$agri),
            mean(cov_frac$wtr), max(cov_frac$wtr)))
say(sprintf("    水域が半分を超えるセル %d / %d（%.1f%%）… 実質的に海",
            sum(cov_frac$wtr > 0.5), ncell, 100 * mean(cov_frac$wtr > 0.5)))
say(sprintf("    そのうち検出努力があるもの %d",
            sum(cov_frac$wtr > 0.5 & n_eff_row > 0)))

## 移動（2セル以上で捕獲された個体）。セルの組と距離
xy <- st_coordinates(st_centroid(st_geometry(cells)))[, 1:2, drop = FALSE] / 1000  # km
per_ind <- split(z$cell, z$ind)
moves <- do.call(rbind, lapply(names(per_ind), function(id) {
  u <- unique(per_ind[[id]])
  if (length(u) < 2L) return(NULL)
  dm <- as.matrix(dist(xy[u, , drop = FALSE]))
  w <- which(dm == max(dm), arr.ind = TRUE)[1, ]
  data.frame(ind = as.integer(id), a = u[w[1]], b = u[w[2]], km = max(dm))
}))
n_moved <- if (is.null(moves)) 0L else nrow(moves)
say(sprintf("  移動が観測された個体 **%d**%s", n_moved,
            if (n_moved) sprintf("（%.1f〜%.1f km）", min(moves$km), max(moves$km)) else ""))

# ===========================================================================
# §3 作図の部品
# ===========================================================================
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)
fig <- function(name) file.path(FIGDIR, paste0(PREFIX, "-", name, ".png"))

land <- NULL
if (!is.na(LAND) && length(LAND)) {
  say("  陸のポリゴン: ", LAND)
  land <- st_make_valid(st_union(st_transform(st_read(LAND, quiet = TRUE),
                                              st_crs(mesh_all))))
} else {
  say("  ⚠ 陸のポリゴンが見つからないので海岸線は描かない（--land で指定可）")
}

## 値を色に割り当てる。0 は塗らない（NA 扱い）
ramp <- function(v, n = 6, pal = "YlOrRd") {
  cols <- rev(hcl.colors(n, pal))
  pos <- v[v > 0]
  if (!length(pos)) return(list(col = rep(NA, length(v)), brk = NULL, cols = cols))
  brk <- unique(round(exp(seq(log(min(pos)), log(max(pos)), length.out = n + 1))))
  if (length(brk) < 2) brk <- c(min(pos), max(pos) + 1)
  idx <- cut(v, breaks = c(0, brk[-1]), labels = FALSE, include.lowest = TRUE)
  idx[v <= 0] <- NA
  list(col = cols[pmin(idx, length(cols))], brk = brk, cols = cols)
}

legend_bins <- function(r, title) {
  if (is.null(r$brk)) return(invisible())
  k <- length(r$brk) - 1
  labs <- sprintf("%s–%s", format(r$brk[seq_len(k)], big.mark = ","),
                  format(r$brk[seq_len(k) + 1], big.mark = ","))
  legend("bottomright", legend = labs, fill = r$cols[seq_len(k)],
         title = title, bty = "n", cex = 0.8, border = NA)
}

# ===========================================================================
# 図1 location — 日本全体のどこを切り出したか
# ===========================================================================
png(fig("location"), width = 1000, height = 1000, res = 110)
par(mar = c(2, 2, 3, 1))
plot(st_geometry(mesh_all), border = "grey88", lwd = 0.2, col = NA,
     main = paste0(SPECIES, "：解析範囲の位置"))
if (!is.null(land)) plot(land, add = TRUE, border = "grey45", lwd = 0.5, col = NA)
plot(st_geometry(cells), add = TRUE, col = "#d95f0255", border = "#d95f02", lwd = 0.3)
bb <- st_bbox(cells)
rect(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], border = "#d95f02", lwd = 2)
legend("topleft", bty = "n", cex = 0.9,
       legend = c(sprintf("全国メッシュ %s セル（convex7）", format(nrow(mesh_all), big.mark = ",")),
                  sprintf("解析範囲 %s セル（関東）", format(ncell, big.mark = ",")),
                  if (!is.null(land)) "海岸線" else NULL),
       fill = c("white", "#d95f0255", if (!is.null(land)) NA else NULL),
       border = c("grey88", "#d95f02", if (!is.null(land)) NA else NULL))
dev.off(); say("  図1: ", fig("location"))

# ===========================================================================
# 図2 effort — 調査地の位置と努力量 ＋ 年ごとの推移
# ===========================================================================
png(fig("effort"), width = 1300, height = 750, res = 110)
layout(matrix(c(1, 2), 1, 2), widths = c(1.9, 1))
par(mar = c(2, 2, 3, 1))
r <- ramp(eff_amt)
plot(st_geometry(cells), col = r$col, border = "grey80", lwd = 0.25,
     main = sprintf("%s：調査地の位置と努力量（%d / %d セル）",
                    SPECIES, sum(n_eff_row > 0), ncell))
if (!is.null(land)) plot(land, add = TRUE, border = "grey40", lwd = 0.6, col = NA)
legend_bins(r, "努力の合計")

par(mar = c(4.5, 4.5, 3, 1))
yr <- sort(unique(effort$YEAR[keep_eff]))
n_by_yr <- sapply(yr, function(y) sum(effort$YEAR[keep_eff] == y))
c_by_yr <- sapply(yr, function(y) length(unique(cell_new[effort$YEAR[keep_eff] == y])))
bp <- barplot(n_by_yr, names.arg = yr, las = 2, col = "#7fcdbb",
              border = NA, ylab = "検出努力の行数", main = "年ごとの調査")
lines(bp, c_by_yr, type = "b", pch = 16, col = "#d95f02", lwd = 2)
legend("topleft", bty = "n", cex = 0.85,
       legend = c("努力の行数", "調査されたセル数"),
       fill = c("#7fcdbb", NA), border = c(NA, NA),
       col = c(NA, "#d95f02"), pch = c(NA, 16), lty = c(NA, 1))
dev.off(); say("  図2: ", fig("effort"))

# ===========================================================================
# 図3 capture — 個体が捕獲された位置
# ===========================================================================
png(fig("capture"), width = 1000, height = 900, res = 110)
par(mar = c(2, 2, 3, 1))
r <- ramp(n_ind_cell, pal = "Purples")
plot(st_geometry(cells), col = r$col, border = "grey80", lwd = 0.25,
     main = sprintf("%s：捕獲された個体の位置（%s 個体 / %d セル）",
                    SPECIES, format(length(keep_ind), big.mark = ","),
                    sum(n_ind_cell > 0)))
if (!is.null(land)) plot(land, add = TRUE, border = "grey40", lwd = 0.6, col = NA)
## 努力があったのに捕獲が無かったセルを区別して示す
none <- which(n_eff_row > 0 & n_ind_cell == 0)
if (length(none))
  plot(st_geometry(cells[none, ]), add = TRUE, col = NA, border = "#1f78b4", lwd = 1.2)
legend_bins(r, "個体数")
legend("topleft", bty = "n", cex = 0.85,
       legend = c(sprintf("捕獲あり %d セル", sum(n_ind_cell > 0)),
                  sprintf("努力はあったが捕獲なし %d セル", length(none))),
       fill = c(r$cols[length(r$cols)], NA), border = c(NA, "#1f78b4"))
dev.off(); say("  図3: ", fig("capture"))

# ===========================================================================
# 図4 moves — 観測された移動
# ===========================================================================
png(fig("moves"), width = 1000, height = 900, res = 110)
par(mar = c(2, 2, 3, 1))
plot(st_geometry(cells), col = "grey96", border = "grey85", lwd = 0.25,
     main = sprintf("%s：観測された移動（%d 個体）", SPECIES, n_moved))
if (!is.null(land)) plot(land, add = TRUE, border = "grey40", lwd = 0.6, col = NA)
## 捕獲のあったセルを背景に
on <- which(n_ind_cell > 0)
if (length(on))
  plot(st_geometry(cells[on, ]), add = TRUE, col = "#cab2d6", border = NA)
if (n_moved) {
  for (i in seq_len(n_moved)) {
    a <- moves$a[i]; b <- moves$b[i]
    arrows(xy[a, 1] * 1000, xy[a, 2] * 1000, xy[b, 1] * 1000, xy[b, 2] * 1000,
           length = 0.12, lwd = 2.4, col = "#e31a1c", code = 3)
    text((xy[a, 1] + xy[b, 1]) / 2 * 1000, (xy[a, 2] + xy[b, 2]) / 2 * 1000,
         sprintf("%.0f km", moves$km[i]), pos = 3, cex = 0.8, col = "#e31a1c")
  }
}
legend("topleft", bty = "n", cex = 0.85,
       legend = c(sprintf("捕獲のあったセル %d", length(on)),
                  sprintf("移動 %d 件（両端が捕獲地）", n_moved)),
       fill = c("#cab2d6", NA), border = c(NA, NA),
       col = c(NA, "#e31a1c"), lwd = c(NA, 2.4))
dev.off(); say("  図4: ", fig("moves"))

# ===========================================================================
# §4 セルごとの表を保存
# ===========================================================================
out <- data.frame(
  cell_sub  = seq_len(ncell),
  cell_all  = keep_cell,                       # convex7 の行番号
  meshcode  = as.character(cells$meshcode),    # JIS 2次メッシュ
  x_km = xy[, 1], y_km = xy[, 2],
  cultivated = cells$cultivated, openwater = cells$openwater,
  n_effort_row = n_eff_row, effort_sum = eff_amt,
  n_capture = n_cap, n_individual = n_ind_cell)
dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT_CSV, row.names = FALSE, fileEncoding = "UTF-8")
say("  保存: ", OUT_CSV, "（", ncell, " セル）")

say("")
say("  ★ 図に使った数値")
say(sprintf("    セル %d / 努力の行 %d / 調査されたセル %d / 捕獲のあったセル %d",
            ncell, length(keep_eff), sum(n_eff_row > 0), sum(n_ind_cell > 0)))
say(sprintf("    個体 %s / 検出 %s / 移動 %d",
            format(length(keep_ind), big.mark = ","),
            format(sum(cnt_sub[keep_ind]), big.mark = ","), n_moved))
if (n_moved) {
  say("    移動の明細:")
  for (i in seq_len(n_moved))
    say(sprintf("      個体 #%-6d  セル %4d(JIS %s) ←→ %4d(JIS %s)  %.1f km",
                keep_ind[moves$ind[i]], moves$a[i], cells$meshcode[moves$a[i]],
                moves$b[i], cells$meshcode[moves$b[i]], moves$km[i]))
}
