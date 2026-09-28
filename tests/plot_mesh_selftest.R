# tests/plot_mesh_selftest.R -------------------------------------------------
#
# examples/plot_mesh.R を、実データ抜きで確かめる。
#
#   Rscript tests/plot_mesh_selftest.R <作業用フォルダ>
#
# 合成の「日本もどき」（斜めに伸びた陸）と、その凸包に切ったメッシュを作り、
# **陸に掛かるセルの数と海だけのセルの数が正しく出るか**を見る。
# 実データ（griddata）は使わない。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
DIR <- if (length(args)) args[1] else stop("使い方: Rscript tests/plot_mesh_selftest.R <dir>")
if (!requireNamespace("sf", quietly = TRUE)) stop("sf パッケージが要ります")
dir.create(DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(20260928)
## 合成の緯度経度なので球面演算は要らない。s2 の妥当性チェックを避ける。
suppressMessages(sf::sf_use_s2(FALSE))

## **弧を描く細長い「陸」。** まっすぐな帯だと凸包が陸とほぼ同じ形になり、
## 海だけのセルが出ない（最初そう書いて 100% 陸になった）。
## 日本のように曲がっていて初めて、凸包が海を大きく含む。
t   <- seq(0, 1, length.out = 60)
arc <- cbind(130 + 12 * t, 31 + 14 * sin(pi * t / 2))     # 右上へ反った弧
line <- sf::st_sfc(sf::st_linestring(arc), crs = 4326)
land <- sf::st_sf(id = 1, geometry = sf::st_buffer(line, 0.25))
stopifnot(all(sf::st_is_valid(land)))

## 陸の上に散らした「放鳥ステーション」
pts <- sf::st_sample(land, 60)
stations <- sf::st_sf(id = seq_along(pts), geometry = pts)

## その凸包に 0.5度グリッドを切る（make_mesh_20251127.R と同じ考え方）
hull <- sf::st_convex_hull(sf::st_union(stations))
grid <- sf::st_make_grid(hull, cellsize = 0.5)
grid <- grid[lengths(sf::st_intersects(grid, hull)) > 0]
mesh <- sf::st_sf(meshcode = sprintf("%05d", seq_along(grid)), geometry = grid)

MESHF <- file.path(DIR, "selftest_mesh.gpkg")
LANDF <- file.path(DIR, "selftest_land.gpkg")
sf::st_write(mesh, MESHF, delete_dsn = TRUE, quiet = TRUE)
sf::st_write(land, LANDF, delete_dsn = TRUE, quiet = TRUE)

hit <- lengths(sf::st_intersects(mesh, land)) > 0
cat("合成データを書きました\n")
cat("  メッシュ: ", MESHF, "\n", sep = "")
cat("  陸      : ", LANDF, "\n", sep = "")
cat("\n  期待する数値:\n")
cat(sprintf("    セル数           : %d\n", nrow(mesh)))
cat(sprintf("    陸に掛かるセル   : %d（%.1f%%）\n", sum(hit), 100 * mean(hit)))
cat(sprintf("    海だけのセル     : %d（%.1f%%）\n", sum(!hit), 100 * mean(!hit)))
cat(sprintf("    コスト比 (ncell^2.5): %.0f分の1\n", (nrow(mesh) / sum(hit))^2.5))
cat("\n  ＊ 細長い陸の凸包なので、海だけのセルが多数を占めるはず。\n")
cat("    足環データのメッシュ（凸包、海を含む）と同じ構図。\n")

cat("\n次のコマンドで確認:\n")
cat("  Rscript examples/plot_mesh.R --mesh ", MESHF, " --land ", LANDF,
    " --no-effort --out ", file.path(DIR, "selftest_mesh.png"), "\n", sep = "")
