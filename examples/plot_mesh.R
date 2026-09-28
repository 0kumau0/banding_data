# examples/plot_mesh.R -------------------------------------------------------
#
# **メッシュがどこに置かれているかを地図で確認し、海の割合を数える。**
#
#   Rscript examples/plot_mesh.R
#   Rscript examples/plot_mesh.R --mesh ../../griddata/mesh2_convex3.gpkg \
#                                --land ../../griddata/QGIS/poly_20251210.shp
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-28）
#
# make_mesh_20251127.R は**放鳥ステーションの凸包**にメッシュを切っている。
# コード自身のコメントが「凸包（海側を含む外郭の簡易近似）」。
# **クマのメッシュ（meshutm_0.5km_buff_land）は陸に切ってあるが、足環側は切っていない。**
#
# ncell = 26,459。loglf のコストは約 O(ncell^2.5) なので、
# **陸に切れるならそれが Step 4 で最も効く手**になる（reports/20260928_band_scale.md）。
#
# 「8割が海」は 26459 × 100km² = 265万km² と日本の陸地 37.8万km² からの**推定**。
# このスクリプトは**実測に置き換える**:
#   - メッシュの総面積
#   - 陸のポリゴンと交わるセルの数（--land を渡したとき）
#   - 検出努力があるセルの数（--rds を渡したとき。既定で config.R の BAND_RDS）
#
# 読むだけ。メッシュも陸のポリゴンも書き換えない。
#
# ⚠ このスクリプトは**活動範囲の外にある生データ（griddata）を読む**。
#    編集機の Claude は実行しないこと。ユーザが自分で走らせる前提。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.bad <- setdiff(grep("^--", .args, value = TRUE),
                c("--mesh", "--land", "--rds", "--out", "--no-effort", "--join"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

if (!requireNamespace("sf", quietly = TRUE)) stop("sf パッケージが要ります")

OUT  <- .opt("--out",  "reports/figures/band-fig-mesh.png")
USE_EFFORT <- !("--no-effort" %in% .args)

## マシンごとに違うパスは config.R に集約してある（BAND_MESH / LAND_SHP / BAND_RDS）。
## 引数で渡されたものが最優先。
if (file.exists("config.R")) source("config.R", encoding = "UTF-8")
MESH <- .opt("--mesh", if (exists("BAND_MESH")) BAND_MESH else
                       "../../griddata/mesh2_convex3.gpkg")

## 陸のポリゴン。config.R の LAND_SHP を最優先にし、無ければ候補を順に試す。
## **LAND_SHP はネットワークドライブ（S:）にあるので、見えない環境がある。**
LAND_CANDIDATES <- c(.opt("--land", ""),
                     if (exists("LAND_SHP")) LAND_SHP else NULL,
                     "../../griddata/QGIS/poly_20251210.shp",
                     "../../griddata/Japan_merge2.shp")
LAND <- LAND_CANDIDATES[nzchar(LAND_CANDIDATES) & file.exists(LAND_CANDIDATES)][1]

if (!file.exists(MESH))
  stop("メッシュが見つかりません: ", MESH, "\n--mesh で指定してください。")

cat("== メッシュの確認 ==\n")
cat("  メッシュ: ", MESH, "\n", sep = "")
mesh <- sf::st_read(MESH, quiet = TRUE)
cat(sprintf("  セル数: **%d**\n", nrow(mesh)))
cat("  CRS   : ", sf::st_crs(mesh)$input, "\n", sep = "")
bb <- sf::st_bbox(mesh)
cat(sprintf("  範囲  : 経度 %.2f 〜 %.2f / 緯度 %.2f 〜 %.2f\n",
            bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))

## --- 面積 --------------------------------------------------------------------
ar <- tryCatch(sum(as.numeric(sf::st_area(mesh))) / 1e6, error = function(e) NA_real_)
if (is.finite(ar)) {
  cat(sprintf("  総面積: **%.0f km²**（日本の陸地は約 378,000 km²）\n", ar))
  cat(sprintf("  → 陸地の %.1f 倍の範囲をメッシュが覆っている\n", ar / 378000))
}

## --- 検出努力のあるセル ------------------------------------------------------
eff_codes <- NULL
if (USE_EFFORT) {                      # config.R は冒頭で読み込み済み
  rds <- .opt("--rds", if (exists("BAND_RDS")) BAND_RDS else "")
  if (nzchar(rds) && file.exists(rds)) {
    d <- readRDS(rds)
    if (!is.null(d$effort) && "meshcode" %in% names(d$effort)) {
      eff_codes <- unique(as.character(d$effort$meshcode))
      cat(sprintf("\n  検出努力のあるメッシュ: **%d**（全体の %.1f%%）\n",
                  length(eff_codes), 100 * length(eff_codes) / nrow(mesh)))
    }
  }
}
## --- ★ effort の meshcode は「行番号」であって JIS メッシュコードではない ---
##
## make_data&plot.R:99-109 が出自:
##
##   mesh <- st_read("mesh2_convex7.gpkg") %>% mutate(meshcode2 = row_number())
##   band_place <- st_join(band_place, mesh, join = st_within, left = FALSE)
##   band_data  <- ... rename(meshcode = meshcode2)
##   effort     <- make.effort2(band_data)
##
## **捕獲地点を空間結合して、その地点が入るメッシュの「行番号」を持たせている。**
## gpkg の meshcode 列（make_mesh_20251127.R:63 が入れた jpgrid のコード）は
## band_data 側では捨てられている（griddata 側では oldmeshcode2 に退避。:181）。
##
## したがって:
##   - 突合は**行番号**で行う（--join row。既定）
##   - **同じファイルを、同じ並び順で読まないと意味が変わる**
##   - ファイルが違えば行番号も違う。**mesh2_convex7.gpkg を使うこと**
JOIN <- .opt("--join", "row")
mc <- intersect(c("meshcode", "MESHCODE", "mesh"), names(mesh))[1]
has_eff <- rep(FALSE, nrow(mesh))

if (!is.null(eff_codes)) {
  if (JOIN == "row") {
    idx <- suppressWarnings(as.integer(eff_codes))
    cat(sprintf("  突合: **行番号**（effort の meshcode = mesh の row_number）\n"))
    cat(sprintf("    effort の meshcode の範囲: %d 〜 %d / mesh の行数: %d\n",
                min(idx, na.rm = TRUE), max(idx, na.rm = TRUE), nrow(mesh)))
    if (any(is.na(idx)))
      cat("    ⚠ 整数にならない meshcode がある。行番号ではない可能性\n")
    else if (max(idx, na.rm = TRUE) > nrow(mesh)) {
      cat("    ⚠ **行番号が mesh の行数を超えている。ファイルが違う。**\n")
      cat("      make_data&plot.R は mesh2_convex7.gpkg を使っている。--mesh で合わせること\n")
    } else {
      has_eff[idx[!is.na(idx)]] <- TRUE
      cat(sprintf("    → %d セルに検出努力あり\n", sum(has_eff)))
    }
  } else {
    if (is.na(mc)) stop("--join code だが mesh に meshcode 列が無い")
    has_eff <- as.character(mesh[[mc]]) %in% eff_codes
    cat(sprintf("  突合: meshcode 列 → %d セル一致\n", sum(has_eff)))
    if (!any(has_eff))
      cat("    ⚠ 一致ゼロ。effort の meshcode は行番号なので --join row を使うこと\n")
  }
}

## --- メッシュの世代を並べて比べる（行番号は世代ごとに違う）------------------
others <- setdiff(Sys.glob(file.path(dirname(MESH), "mesh2_convex*.gpkg")), MESH)
if (length(others)) {
  cat("\n  同じ場所にある他の世代:\n")
  for (o in others) {
    n <- tryCatch(nrow(sf::st_read(o, quiet = TRUE)), error = function(e) NA_integer_)
    cat(sprintf("    %-28s %s セル%s\n", basename(o),
                if (is.na(n)) "?" else format(n, big.mark = ","),
                if (!is.na(n) && n == nrow(mesh)) "  ← 行数は同じ（並び順までは不明）" else ""))
  }
  cat("  ＊ **行番号で突合している以上、世代が違えば指す場所も違う。**\n")
}

## --- 陸との重なり ------------------------------------------------------------
on_land <- NULL
if (!is.na(LAND) && length(LAND)) {
  cat("\n  陸のポリゴン: ", LAND, "\n", sep = "")
  land <- sf::st_read(LAND, quiet = TRUE)
  land <- sf::st_transform(land, sf::st_crs(mesh))
  land <- sf::st_make_valid(sf::st_union(land))
  hit <- sf::st_intersects(mesh, land, sparse = FALSE)[, 1]
  on_land <- hit
  cat(sprintf("  **陸に掛かるセル: %d / %d（%.1f%%）**\n",
              sum(hit), nrow(mesh), 100 * mean(hit)))
  cat(sprintf("  → 海だけのセル: %d（%.1f%%）\n", sum(!hit), 100 * mean(!hit)))
  cat("\n  ★ 陸に切った場合のコスト比（loglf は約 O(ncell^2.5)）:\n")
  cat(sprintf("    ncell %d → %d で **%.0f分の1**\n",
              nrow(mesh), sum(hit), (nrow(mesh) / max(1, sum(hit)))^2.5))
  if (!is.null(eff_codes) && any(has_eff))
    cat(sprintf("    （検出努力のあるセルのうち海だけのもの: %d）\n", sum(has_eff & !hit)))
} else {
  cat("\n  陸のポリゴンが見つからないので、海の割合は数えない。\n")
  cat("  --land <shp/gpkg> で渡すと数える。候補:\n")
  cat("    ", paste(LAND_CANDIDATES[nzchar(LAND_CANDIDATES)], collapse = "\n    "), "\n", sep = "")
}

## --- 作図 --------------------------------------------------------------------
## 26,459セルを塗ると重いので、枠線だけを薄く描く。
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
png(OUT, width = 1100, height = 900, res = 110)
op <- par(mar = c(2, 2, 3, 1))
plot(sf::st_geometry(mesh), border = "#cfd6d0", lwd = 0.25,
     main = sprintf("%s  (%d cells)", basename(MESH), nrow(mesh)))
if (!is.null(on_land))
  plot(sf::st_geometry(mesh)[on_land], border = "#557a4b", lwd = 0.3, add = TRUE)
if (!is.na(LAND) && length(LAND))
  plot(sf::st_geometry(land), border = "#17211d", lwd = 0.9, add = TRUE)
if (any(has_eff))
  plot(sf::st_geometry(mesh)[has_eff], col = "#a8402c60", border = "#a8402c",
       lwd = 0.3, add = TRUE)
legend("topleft", bty = "n", cex = 0.85,
       legend = c(sprintf("すべてのセル (%d)", nrow(mesh)),
                  if (!is.null(on_land)) sprintf("陸に掛かる (%d)", sum(on_land)),
                  if (any(has_eff)) sprintf("検出努力あり (%d)", sum(has_eff))),
       col = c("#cfd6d0", if (!is.null(on_land)) "#557a4b",
               if (any(has_eff)) "#a8402c"),
       lwd = c(2, if (!is.null(on_land)) 2, if (any(has_eff)) 2))
par(op); dev.off()
cat("\n図: ", OUT, "\n", sep = "")
cat("  薄い灰 = 全セル / 緑 = 陸に掛かるセル / 赤 = 検出努力のあるセル\n")
