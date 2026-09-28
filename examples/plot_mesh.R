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
                c("--mesh", "--land", "--rds", "--out", "--no-effort"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

if (!requireNamespace("sf", quietly = TRUE)) stop("sf パッケージが要ります")

MESH <- .opt("--mesh", "../../griddata/mesh2_convex3.gpkg")
OUT  <- .opt("--out",  "reports/figures/band-fig-mesh.png")
USE_EFFORT <- !("--no-effort" %in% .args)

## 陸のポリゴン。候補を順に試す（環境によって置き場所が違う）
LAND_CANDIDATES <- c(.opt("--land", ""),
                     "../../griddata/QGIS/poly_20251210.shp",
                     "../../griddata/Japan_merge2.shp",
                     "S:/common/personal_backup/kumada/Virbsagi/R/Japan_merge2.shp")
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
if (USE_EFFORT && file.exists("config.R")) {
  source("config.R", encoding = "UTF-8")
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
mc <- intersect(c("meshcode", "MESHCODE", "mesh"), names(mesh))[1]
has_eff <- if (!is.null(eff_codes) && !is.na(mc))
             as.character(mesh[[mc]]) %in% eff_codes else rep(FALSE, nrow(mesh))
if (!is.null(eff_codes) && !is.na(mc) && !any(has_eff))
  cat("  ⚠ meshcode が一致しない。effort と mesh で符号の付け方が違う可能性\n")

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
