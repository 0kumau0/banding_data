# examples/plot_moves_national.R ---------------------------------------------
#
# **全国スケールで「観測された移動」を種ごとに図にする。**
#
#   Rscript examples/plot_moves_national.R
#   Rscript examples/plot_moves_national.R --species シジュウカラ,ヤマガラ --slug shijukara,yamagara
#
# 生データを**読むだけ**。書き出すのは reports/figures/<prefix>-*.png と
# --out の CSV だけ。
#
# ---------------------------------------------------------------------------
# 何のための図か（2026-09-29）
#
# ADCR が推定する透過性（`conn_0` / `conn_agri` / `conn_wtr`）の情報源は
# **「別のメッシュで再捕獲された個体」だけ**。再捕獲が何回あっても、
# 同じメッシュ内なら移動は観測されない。
#
# シジュウカラは全国17件・関東5件しかなかった（`reports/20260929_kanto_dataset.md`）。
# **他の種ではどうか、そして移動がどこで起きているかを見る。**
#
# ---------------------------------------------------------------------------
# ★ 前提
#
#   - `effort$meshcode` は `mesh2_convex7.gpkg` の**行番号**。JIS コードではない
#   - 検出行列は **行 = 努力 / 列 = 個体**
#   - 疎行列に**全要素 0 のダミー個体が1列**入っている。`m@x > 0` で落とすこと。
#     落とさないと捕獲0回の個体が全国を移動したことになり、
#     全30種の最大移動距離が 3020.3 km になる（2026-09-29 に特定）
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--species", "--slug", "--mesh", "--rds", "--land", "--prefix", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--panel"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

SPP    <- strsplit(.opt("--species", "ノジコ,ウグイス,シジュウカラ,クロツグミ"), ",")[[1]]
SLUG   <- strsplit(.opt("--slug", "nojiko,uguisu,shijukara,kurotsugumi"), ",")[[1]]
if (length(SLUG) != length(SPP)) SLUG <- paste0("sp", seq_along(SPP))
PREFIX <- .opt("--prefix", "moves-fig")
OUT    <- .opt("--out", "results/moves_national.csv")
FIGDIR <- "reports/figures"

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
MESH <- .opt("--mesh", BAND_MESH)
RDS  <- .opt("--rds",  BAND_RDS)
for (p in c(MESH, RDS)) if (!file.exists(p)) stop("見つかりません: ", p)

LAND_CAND <- c(.opt("--land", ""), if (exists("LAND_SHP")) LAND_SHP else NULL,
               "../../griddata/QGIS/poly_20251210.shp",
               "../../griddata/Japan_merge2.shp")
LAND <- LAND_CAND[nzchar(LAND_CAND) & file.exists(LAND_CAND)][1]

suppressMessages({ library(sf); library(Matrix) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }

# ===========================================================================
# §1 入力
# ===========================================================================
say("== 全国スケールの移動図 ==")
mesh <- st_read(MESH, quiet = TRUE)
d    <- readRDS(RDS)
effort <- as.data.frame(d$effort)
eff_cell <- as.integer(as.character(effort$meshcode))   # ★ 行番号
stopifnot(!anyNA(eff_cell), max(eff_cell) <= nrow(mesh))

xy   <- st_coordinates(st_centroid(st_geometry(mesh)))[, 1:2, drop = FALSE]  # m
xykm <- xy / 1000
jis  <- if ("meshcode" %in% names(mesh)) as.character(mesh$meshcode) else rep(NA, nrow(mesh))

land <- NULL
if (!is.na(LAND) && length(LAND)) {
  land <- st_make_valid(st_union(st_transform(st_read(LAND, quiet = TRUE), st_crs(mesh))))
  say("  陸のポリゴン: ", LAND)
} else say("  ⚠ 陸のポリゴンが無いので海岸線は描かない")

say("  メッシュ ", format(nrow(mesh), big.mark = ","), " セル / 努力 ",
    format(nrow(effort), big.mark = ","), " 行")

# ===========================================================================
# §2 種ごとに移動を取り出す
# ===========================================================================
## 1種ぶん。**値 0 の格納要素を落とす**のが要点
extract <- function(sp) {
  i <- which(d$splist == sp)
  if (!length(i) || i > length(d$detect_list)) return(NULL)
  m <- d$detect_list[[i]]
  if (!inherits(m, "dgCMatrix")) m <- methods::as(m, "dgCMatrix")
  z <- data.frame(ind = rep.int(seq_len(ncol(m)), diff(m@p)),
                  row = m@i + 1L, n = m@x)
  z <- z[z$n > 0, , drop = FALSE]
  z$cell <- eff_cell[z$row]

  cnt <- Matrix::colSums(m)
  per <- split(z$cell, z$ind)
  mv  <- do.call(rbind, lapply(names(per), function(id) {
    u <- unique(per[[id]])
    if (length(u) < 2L) return(NULL)
    dm <- as.matrix(dist(xykm[u, , drop = FALSE]))
    w  <- which(dm == max(dm), arr.ind = TRUE)[1, ]
    data.frame(ind = as.integer(id), a = u[w[1]], b = u[w[2]], km = max(dm))
  }))
  list(sp = sp, n_ind = sum(cnt > 0), n_multi = sum(cnt > 1),
       n_rec = sum(cnt), cells_used = sort(unique(z$cell)), moves = mv)
}

dat <- lapply(SPP, extract)
names(dat) <- SPP
if (any(sapply(dat, is.null))) stop("見つからない種: ",
                                    paste(SPP[sapply(dat, is.null)], collapse = ", "))

# ===========================================================================
# §3 作図
# ===========================================================================
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

## 1枚ぶんの地図。`cex_main` で 2x2 パネルと単独figで字の大きさを変える
draw_one <- function(x, cex_main = 1) {
  mv <- x$moves
  n_mv <- if (is.null(mv)) 0L else nrow(mv)
  ## 独立な「つながり」の数（同じセルの組で複数個体が動いていても1本と数える）
  n_pair <- if (!n_mv) 0L else
    length(unique(apply(cbind(pmin(mv$a, mv$b), pmax(mv$a, mv$b)), 1, paste, collapse = "-")))

  plot(st_geometry(mesh), border = "grey92", lwd = 0.15, col = NA,
       main = sprintf("%s  移動 %d 件 / つながり %d 本", x$sp, n_mv, n_pair),
       cex.main = cex_main)
  if (!is.null(land)) plot(land, add = TRUE, border = "grey55", lwd = 0.4, col = NA)
  ## 捕獲のあったセル
  plot(st_geometry(mesh[x$cells_used, ]), add = TRUE, col = "#cab2d6", border = NA)
  ## 移動。長いものほど濃く
  if (n_mv) {
    o <- order(mv$km)                       # 短いものを先に描く
    for (i in o) {
      col <- if (mv$km[i] >= 500) "#e31a1c" else "#1f78b4"
      arrows(xy[mv$a[i], 1], xy[mv$a[i], 2], xy[mv$b[i], 1], xy[mv$b[i], 2],
             length = 0.06, lwd = 1.6, col = paste0(col, "cc"), code = 3)
    }
  }
  invisible(list(n_mv = n_mv, n_pair = n_pair))
}

## --- まとめ図（--panel を付けたときだけ）------------------------------------
## **既定では作らない。** 4枚を並べると1枚あたりが小さくなり、
## 矢印がどのメッシュを結んでいるのかが読めなくなる（2026-09-29 の指示）。
## レポートでは種ごとの単独図を使う。
## つながりの本数の計算はここで済ませるので、パネルを描かないときも
## 同じ関数を「描画せずに」通す必要がある → 一時ファイルに捨てる
if ("--panel" %in% .args) {
  f4 <- file.path(FIGDIR, paste0(PREFIX, "-4sp.png"))
  png(f4, width = 1500, height = 1500, res = 120)
  op <- par(mfrow = c(2, 2), mar = c(1, 1, 2.5, 1))
  info <- lapply(dat, draw_one, cex_main = 1.1)
  par(op); dev.off()
  say("  図: ", f4)
} else {
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 600, height = 600)
  info <- lapply(dat, draw_one)
  dev.off(); unlink(tmp)
}

## --- 種ごとの単独図 ---------------------------------------------------------
for (k in seq_along(SPP)) {
  f <- file.path(FIGDIR, paste0(PREFIX, "-", SLUG[k], ".png"))
  png(f, width = 1000, height = 1100, res = 115)
  par(mar = c(1, 1, 3, 1))
  draw_one(dat[[k]])
  legend("topleft", bty = "n", cex = 0.85,
         legend = c(sprintf("捕獲のあったセル %s",
                            format(length(dat[[k]]$cells_used), big.mark = ",")),
                    "移動 500 km 未満", "移動 500 km 以上"),
         fill = c("#cab2d6", NA, NA), border = NA,
         col = c(NA, "#1f78b4", "#e31a1c"), lwd = c(NA, 1.6, 1.6))
  dev.off()
  say("  図: ", f)
}

# ===========================================================================
# §4 表
# ===========================================================================
tab <- do.call(rbind, lapply(seq_along(SPP), function(k) {
  x <- dat[[k]]; mv <- x$moves
  n_mv <- if (is.null(mv)) 0L else nrow(mv)
  n_pair <- info[[k]]$n_pair
  data.frame(
    species = x$sp,
    n_ind = x$n_ind, n_multi = x$n_multi, n_record = x$n_rec,
    det_per_ind = x$n_rec / x$n_ind,
    n_cell_used = length(x$cells_used),
    n_moved = n_mv, n_pair = n_pair,
    moved_per_multi = n_mv / max(1L, x$n_multi),
    km_med = if (n_mv) median(mv$km) else NA_real_,
    km_max = if (n_mv) max(mv$km)    else NA_real_,
    n_over500 = if (n_mv) sum(mv$km >= 500) else 0L)
}))

say("")
say("  ★ 全国スケールの比較")
print(format(tab, digits = 3), row.names = FALSE)
say("")
say("   n_moved  = 2つ以上の異なるメッシュで捕獲された個体")
say("   n_pair   = **独立なセルの組**（同じ組で複数個体が動いても1本）")
say("   n_over500 = 500 km 以上の移動。**渡りの可能性**")

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(tab, OUT, row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT)

## 移動の明細（距離順）も出しておく
say("")
for (k in seq_along(SPP)) {
  mv <- dat[[k]]$moves
  if (is.null(mv) || !nrow(mv)) { say("  ", SPP[k], ": 移動なし"); next }
  mv <- mv[order(-mv$km), , drop = FALSE]
  say(sprintf("  %s（%d件。上位5件）:", SPP[k], nrow(mv)))
  for (i in head(seq_len(nrow(mv)), 5))
    say(sprintf("      JIS %-8s ←→ %-8s  %7.1f km", jis[mv$a[i]], jis[mv$b[i]], mv$km[i]))
}
