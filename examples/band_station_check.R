# examples/band_station_check.R ----------------------------------------------
#
# **同じ10kmメッシュの中に標識ステーションが複数あるか、
#   そこで「消えている移動」がどれだけあるかを数える。**
#
#   Rscript examples/band_station_check.R
#   Rscript examples/band_station_check.R --top 12
#   Rscript examples/band_station_check.R --sp-col SPNAME --species "Japanese Tit"
#
# 生データを**読むだけ**。書き出すのは --out の CSV だけ（集計値のみ）。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-29）
#
# 透過性（`conn_0` / `conn_agri` / `conn_wtr`）の情報源は
# **「別のメッシュで再捕獲された個体」**だけで、実測は
#
#   シジュウカラ・全国 …「つながり」14本 / 関東 … **4本**
#
# 合成データでの実験（`examples/identify_conn.R`、58分・24試行）では
#
#   つながり 〜5本   … SE が真値の3〜4倍。`conn_wtr` の符号一致 **0.50**（推定不能）
#   つながり 11〜20本 … SE が真値の1/3（実用域）
#
# **この数本の差が「推定できる/できない」を分ける。**
#
# ## ところが
#
# `d$effort` は **(メッシュ × 年)** の 4,229行。**同じ10kmセル内の複数の
# ステーションは1つにまとめられている。** 実際 `band_data` の `PCODE` は
# **1,445 種類**あるのに、努力のあるメッシュは **964 セル**しかない。
#
#   → **2〜10 km 離れたステーション間の移動が「同じセル」として消えている**
#
# もしそうなら、**メッシュを細かくするだけで「つながり」が増える。**
# 「4本しかない」という前提そのものが動く。
#
# ---------------------------------------------------------------------------
# ★ この処理は make_data&plot.R:83-109 をそのまま再現している
#
#   band_data_20251204.csv  … PCODE はあるが**座標もメッシュも無い**（14列）
#   PLACE.DBF               … PCODE → LAT/LONG（度分表記）
#   mesh2_convex7.gpkg      … メッシュ。**行番号**が effort$meshcode の正体
#
#   read_csv(shift_jis) → left_join(place) → st_as_sf(4326) → st_transform(3100)
#     → st_join(mesh, st_within, left=FALSE) → meshcode2 = row_number()
#
# **`left = FALSE` は内部結合**なので、メッシュの外に落ちるステーションの
# 記録は捨てられる。ここも同じにしないと数が合わない。
#
# ⚠ **CSV は shift_jis。** UTF-8 で読むと行数が変わる
#   （UTF-8 で 1,382,793 行 / shift_jis で 1,379,999 行）。
#
# ---------------------------------------------------------------------------
# ★ 出力について
#
# **既定では集計値しか出さない**（列名・件数・分布）ので、そのまま貼れる。
# 生の値を見たいときだけ `--peek` を付ける（**貼る前に中身を確認すること**）。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--csv", "--place", "--mesh", "--species", "--sp-col", "--top", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--peek"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--peek"), collapse = " "))

PEEK    <- "--peek" %in% .args
SPECIES <- .opt("--species", NULL)
SP_COL  <- .opt("--sp-col", "SPNAMK")
TOPN    <- as.integer(.opt("--top", "10"))
OUT     <- .opt("--out", "results/band_station_check.csv")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")

CSV   <- .opt("--csv",   if (exists("BAND_CSV"))  BAND_CSV  else NA_character_)
MESH  <- .opt("--mesh",  if (exists("BAND_MESH")) BAND_MESH else NA_character_)
PLACE <- .opt("--place", file.path(if (exists("DATA_ROOT")) DATA_ROOT else "../..",
                                   "PLACE.DBF"))
for (p in c(CSV, MESH, PLACE))
  if (is.na(p) || !file.exists(p))
    stop("見つかりません: ", p, "\n--csv / --mesh / --place で指定してください。")

say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 74))

## ★★ **`Sys.setlocale("LC_ALL", "Japanese_Japan.932")` を呼んではいけない。**
##
## `make_data&plot.R:8` はそれをやっているが、**このファイルは UTF-8 なので
## 真似すると壊れる**。`Rscript` はファイルを**逐次パースしながら実行**するため、
## 途中でロケールを CP932 に切り替えると**それ以降の行が Shift-JIS として
## 解釈され**、日本語コメントのバイト列が壊れてパースエラーになる
## （2026-09-29 に実際に `予想外の '==' です ... say("====` で落ちた）。
##
## そもそもここでは不要。`PLACE.DBF` から使うのは PCODE / LAT / LONG だけで
## すべて ASCII、CSV は `read_csv` に `locale(encoding = "shift_jis")` を
## 直接渡しているので、システムのロケールに依存しない。
suppressMessages({ library(sf); library(dplyr); library(readr); library(foreign) })
source("functions.R", encoding = "UTF-8")
if (!exists("convert_to_decimal"))
  stop("functions.R に convert_to_decimal がありません。度分表記を10進に直せません。")

say("=========================================================================")
say("  標識ステーションとメッシュの関係を数える")
say("=========================================================================")
say("  CSV   : ", CSV, sprintf("（%.1f MB）", file.size(CSV) / 1e6))
say("  PLACE : ", PLACE)
say("  メッシュ: ", MESH)

# ===========================================================================
# §1 読み込み（make_data&plot.R:83-91 と同じ）
# ===========================================================================
say("")
say("§1 読み込み")

## ⚠ shift_jis。UTF-8 で読むと行数が変わる
band <- suppressWarnings(read_csv(CSV,
  col_types = cols(PCODE = col_character(), RING = "character",
                   GUID = "character", SPC = "character"),
  locale = locale(encoding = "shift_jis"), progress = FALSE))
say(sprintf("  band_data: **%s 行 × %d 列**", format(nrow(band), big.mark = ","), ncol(band)))
say("    列: ", paste(names(band), collapse = ", "))
if (!all(c("PCODE", "RING", "GUID", SP_COL) %in% names(band)))
  stop("必要な列がありません: ", paste(setdiff(c("PCODE","RING","GUID",SP_COL), names(band)),
                                      collapse = ", "))

place <- read.dbf(PLACE, as.is = TRUE)
say(sprintf("  PLACE.DBF: %s 行 / 列: %s",
            format(nrow(place), big.mark = ","), paste(names(place), collapse = ", ")))

## make_data&plot.R:15,18,19 と同じ補正。**これをしないと数が合わない**
band  <- band  %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))
place <- place %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))
n_before <- nrow(place)
place <- place %>% filter(PCODE != 380036)   # 石鎚山のおかしなデータ（2025-12-04）
say(sprintf("  補正: PCODE 910053 → 110099 / 380036 を除外（%d → %d 行）",
            n_before, nrow(place)))

## 度分表記 → 10進（make_data&plot.R:25-26）
place$Lat <- sapply(place$LAT,  convert_to_decimal)
place$Lon <- sapply(place$LONG, convert_to_decimal)
ok <- is.finite(place$Lat) & is.finite(place$Lon)
if (any(!ok)) say(sprintf("  ⚠ 座標に変換できない地点 %d 件は除外", sum(!ok)))
place <- place[ok, , drop = FALSE]

say(sprintf("  band_data の PCODE: **%s 種類**",
            format(length(unique(band$PCODE)), big.mark = ",")))
say(sprintf("  PLACE に座標がある PCODE: %s 種類",
            format(length(unique(place$PCODE)), big.mark = ",")))
miss <- setdiff(unique(band$PCODE), unique(place$PCODE))
if (length(miss))
  say(sprintf("  ⚠ 座標が引けない PCODE: %s 種類（記録 %s 件が落ちる）",
              format(length(miss), big.mark = ","),
              format(sum(band$PCODE %in% miss), big.mark = ",")))

if (PEEK) { say(""); say("  --peek: PLACE の先頭3行"); print(utils::head(place, 3)) }

# ===========================================================================
# §2 ステーション → メッシュ（make_data&plot.R:93-109 と同じ）
# ===========================================================================
say("")
hr(); say("§2 ステーションをメッシュに落とす"); hr()

## **ステーション単位で落とす。** 記録140万行すべてを st_join するのは無駄
st_pts <- place %>% dplyr::select(PCODE, Lat, Lon) %>% distinct(PCODE, .keep_all = TRUE) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% st_transform(crs = 3100)

mesh <- st_read(MESH, quiet = TRUE) %>% mutate(meshcode2 = row_number())
say(sprintf("  メッシュ %s セル", format(nrow(mesh), big.mark = ",")))

## `left = FALSE` は**内部結合**。メッシュの外の地点は落ちる（本家と同じ）
st_in <- st_join(st_pts, mesh %>% dplyr::select(meshcode2), join = st_within, left = FALSE)
st_map <- st_in %>% st_drop_geometry() %>% dplyr::select(PCODE, meshcode = meshcode2)

## ★ 投影座標（EPSG:3100、単位メートル）を残す。**消えている移動の距離**を出すため。
## これが分からないと「どこまでメッシュを細かくすればよいか」が決まらない
st_xy <- st_coordinates(st_in)
ST_X <- setNames(st_xy[, 1], st_in$PCODE)
ST_Y <- setNames(st_xy[, 2], st_in$PCODE)

say(sprintf("  座標のある %s 地点のうち、**メッシュ内に入るのは %s 地点**",
            format(nrow(st_pts), big.mark = ","), format(nrow(st_map), big.mark = ",")))
say(sprintf("  → メッシュ外で落ちる地点: %s", format(nrow(st_pts) - nrow(st_map), big.mark = ",")))

n_station <- length(unique(st_map$PCODE))
n_mesh    <- length(unique(st_map$meshcode))
say("")
say("  **ステーション ", format(n_station, big.mark = ","), " 箇所**")
say("  **それが入るメッシュ ", format(n_mesh, big.mark = ","), " セル**")
say(sprintf("  → 1セルあたり平均 **%.2f ステーション**", n_station / n_mesh))

## ★ ここが核心
per <- table(st_map$meshcode)
say("")
say("  1メッシュあたりのステーション数の分布:")
tb <- table(as.integer(per))
for (k in names(tb))
  say(sprintf("    %3s ステーション: %6s セル%s", k, format(tb[[k]], big.mark = ","),
              if (as.integer(k) >= 2) "  ← この中の移動は今は見えない" else ""))
n_multi_cell <- sum(per >= 2)
say("")
say(sprintf("  **2つ以上のステーションを含むセル: %s / %s（%.1f%%）**",
            format(n_multi_cell, big.mark = ","), format(n_mesh, big.mark = ","),
            100 * n_multi_cell / n_mesh))
if (n_multi_cell == 0) {
  say("  → **メッシュを細かくしても移動は増えない。** 1セル1ステーション")
} else {
  say(sprintf("  → そのセルに含まれるステーションは合計 %s 箇所",
              format(sum(per[per >= 2]), big.mark = ",")))
}

# ===========================================================================
# §3 種ごとに「消えている移動」を数える
# ===========================================================================
say("")
hr(); say("§3 種ごとの移動 — メッシュ単位 vs ステーション単位"); hr()

## 記録をメッシュに結合（内部結合なので、メッシュ外の記録は落ちる＝本家と同じ）
d <- band %>% dplyr::select(PCODE, GUID, RING, YEAR, all_of(SP_COL)) %>%
  inner_join(st_map, by = "PCODE")
say(sprintf("  メッシュに割り当てられた記録: **%s / %s 件（%.1f%%）**",
            format(nrow(d), big.mark = ","), format(nrow(band), big.mark = ","),
            100 * nrow(d) / nrow(band)))

## ★ 個体は **GUID + RING の組**。RING は 100,000 種類（00000-99999）しかなく、
## GUID（99種類）が系列の接頭辞。**組にしないと別個体を同一視する**
d$.ring <- paste(d$GUID, d$RING, sep = "")
d$.sp   <- as.character(d[[SP_COL]])
say(sprintf("  個体の識別: RING 単独 %s 通り → GUID と組で **%s 通り**",
            format(length(unique(d$RING)), big.mark = ","),
            format(length(unique(d$.ring)), big.mark = ",")))

sp_list <- if (!is.null(SPECIES)) {
  if (!(SPECIES %in% d$.sp)) stop("種が見つかりません: ", SPECIES,
                                  "\n（--sp-col ", SP_COL, " の値。--peek で確認できます）")
  SPECIES
} else names(sort(table(d$.sp), decreasing = TRUE))[seq_len(min(TOPN, length(unique(d$.sp))))]

## 個体ごとの「異なる○の数」と「つながり（独立な組）の数」
n_distinct_by <- function(id, key) {
  u <- unique(data.frame(id = id, k = key, stringsAsFactors = FALSE))
  tabulate(match(u$id, unique(u$id)), nbins = length(unique(u$id)))
}
pair_count <- function(id, key) {
  u <- unique(data.frame(id = id, k = key, stringsAsFactors = FALSE))
  s <- split(u$k, u$id)
  s <- s[lengths(s) >= 2]
  if (!length(s)) return(0L)
  pr <- unlist(lapply(s, function(v) {
    cb <- utils::combn(sort(as.character(v)), 2)
    paste(cb[1, ], cb[2, ], sep = "")
  }), use.names = FALSE)
  length(unique(pr))
}

rows <- list()
for (sp in sp_list) {
  x <- d[d$.sp == sp, , drop = FALSE]
  nm <- n_distinct_by(x$.ring, x$meshcode)
  ns <- n_distinct_by(x$.ring, x$PCODE)
  moved_mesh <- sum(nm >= 2); moved_station <- sum(ns >= 2)
  pair_mesh <- pair_count(x$.ring, x$meshcode)
  pair_st   <- pair_count(x$.ring, x$PCODE)

  say("")
  say("  【", sp, "】 記録 ", format(nrow(x), big.mark = ","),
      " / 個体 ", format(length(unique(x$.ring)), big.mark = ","))
  say(sprintf("    メッシュ単位    : 移動 %5d 個体 / つながり %5d 本  ← いまの解析はこれ",
              moved_mesh, pair_mesh))
  say(sprintf("    ステーション単位: 移動 %5d 個体 / つながり %5d 本  ← 細かくすれば見える上限",
              moved_station, pair_st))
  say(sprintf("    **差（同じセル内の別ステーション）: 移動 %+d 個体 / つながり %+d 本**",
              moved_station - moved_mesh, pair_st - pair_mesh))

  ## ★ **消えている移動の距離**。同じセル内で別ステーションに移った個体について、
  ## その2地点が実際に何 km 離れていたか。**必要なメッシュ解像度を決める数字**
  u <- unique(data.frame(id = x$.ring, st = x$PCODE, ms = x$meshcode,
                         stringsAsFactors = FALSE))
  s <- split(u, u$id)
  s <- s[vapply(s, function(z) nrow(z) >= 2 && length(unique(z$ms)) == 1L, logical(1))]
  hid_km <- if (!length(s)) numeric(0) else
    unlist(lapply(s, function(z) {
      cb <- utils::combn(z$st, 2)
      sqrt((ST_X[cb[1, ]] - ST_X[cb[2, ]])^2 + (ST_Y[cb[1, ]] - ST_Y[cb[2, ]])^2) / 1000
    }), use.names = FALSE)
  hid_km <- hid_km[is.finite(hid_km)]
  if (length(hid_km)) {
    q <- stats::quantile(hid_km, c(0.5, 0.9), names = FALSE)
    say(sprintf("    消えている移動の距離: 中央 %.2f km / 9割点 %.2f km / 最大 %.2f km",
                q[1], q[2], max(hid_km)))
    say(sprintf("      1km超 %d / 2km超 %d / 5km超 %d 件（全 %d 件）",
                sum(hid_km > 1), sum(hid_km > 2), sum(hid_km > 5), length(hid_km)))
  }

  rows[[length(rows) + 1L]] <- data.frame(
    species = sp, n_record = nrow(x), n_ind = length(unique(x$.ring)),
    moved_mesh = moved_mesh, pair_mesh = pair_mesh,
    moved_station = moved_station, pair_station = pair_st,
    hidden_ind = moved_station - moved_mesh, hidden_pair = pair_st - pair_mesh,
    hidden_km_med = if (length(hid_km)) stats::median(hid_km) else NA_real_,
    hidden_km_max = if (length(hid_km)) max(hid_km) else NA_real_,
    hidden_over1km = sum(hid_km > 1), hidden_over2km = sum(hid_km > 2),
    hidden_over5km = sum(hid_km > 5))
}

res <- do.call(rbind, rows)
say("")
hr(); say("  まとめ"); hr()
print(res, row.names = FALSE)

say("")
say("  ★ 読み方")
say("    pair_mesh    … いまの解析で使えている「つながり」の本数")
say("    pair_station … ステーション単位で数えた場合の本数")
say("    hidden_pair  … **その差。メッシュ集計で消えている情報**")
say("")
say("    合成データでの実験（examples/identify_conn.R）:")
say("      つながり 〜5本   … SE が真値の3〜4倍。符号一致 0.50（推定不能）")
say("      つながり 11〜20本 … SE が真値の1/3（実用域）")
say("    **hidden_pair が大きければ、メッシュを細かくする価値がある。**")
say("")
say("    hidden_km_* … **消えている移動の距離**。必要なメッシュ解像度を決める。")
say("                  1km超が多ければ 1km メッシュで拾えるが、ncell は100倍になる")
say("")
say("  ＊ moved_mesh は examples/band_moves.R の n_moved と一致するはず（検算）")

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(res, OUT, row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, "（集計値のみ）")
