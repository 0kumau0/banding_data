# examples/band_station_check.R ----------------------------------------------
#
# **同じ10kmメッシュの中に標識ステーションが複数あるか、
#   そこで「消えている移動」がどれだけあるかを数える。**
#
#   Rscript examples/band_station_check.R                    # まず構造だけ見る
#   Rscript examples/band_station_check.R --species シジュウカラ
#   Rscript examples/band_station_check.R --station STATION --ring RING --lat LAT --lon LON
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
# 合成データでの実験（`examples/identify_conn.R`）では、
# **4本では SE が真値の3〜4倍、`conn_wtr` の符号一致は 0.50（コイン投げ）**。
# 11〜20本で実用域に入る。**この数本の差が「推定できる/できない」を分ける。**
#
# ## ところが
#
# `d$effort` は **(メッシュ × 年)** の 4,229行。つまり
# **同じ10kmセル内の複数のステーションは1つにまとめられている。**
#
#   → **2〜10 km 離れたステーション間の移動は、いま「同じセル」として
#      消えている可能性がある。**
#
# もしそうなら、**メッシュを細かくするだけで「つながり」が増える。**
# 「4本しかない」という前提そのものが動く。それを確かめる。
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
.known <- c("--csv", "--species", "--station", "--ring", "--ring2", "--sp-col",
            "--lat", "--lon", "--mesh", "--date", "--top", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--peek"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--peek"), collapse = " "))

PEEK    <- "--peek" %in% .args
SPECIES <- .opt("--species", NULL)
TOPN    <- as.integer(.opt("--top", "8"))
OUT     <- .opt("--out", "results/band_station_check.csv")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
CSV <- .opt("--csv", if (exists("BAND_CSV")) BAND_CSV else NULL)
if (is.null(CSV) || !file.exists(CSV))
  stop("集計前の CSV が見つかりません: ", if (is.null(CSV)) "(config.R に BAND_CSV が無い)" else CSV,
       "\n--csv <パス> で指定してください。")

say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 74))

say("=========================================================================")
say("  標識ステーションとメッシュの関係を数える")
say("=========================================================================")
say("  ファイル: ", CSV, sprintf("（%.1f MB）", file.size(CSV) / 1e6))

# ===========================================================================
# §1 構造
# ===========================================================================
say("")
say("§1 構造")

## 大きいかもしれないので、まず数行だけ読んで列名と型を見る
head5 <- utils::read.csv(CSV, nrows = 5, fileEncoding = "UTF-8",
                         check.names = FALSE, stringsAsFactors = FALSE)
say("  列数 ", ncol(head5))

dat <- if (requireNamespace("data.table", quietly = TRUE)) {
  say("  data.table::fread で読み込み中...")
  as.data.frame(data.table::fread(CSV, encoding = "UTF-8",
                                  check.names = FALSE, data.table = FALSE))
} else {
  say("  read.csv で読み込み中（data.table があればもっと速い）...")
  utils::read.csv(CSV, fileEncoding = "UTF-8", check.names = FALSE,
                  stringsAsFactors = FALSE)
}
say("  **", format(nrow(dat), big.mark = ","), " 行 × ", ncol(dat), " 列**")

say("")
say(sprintf("  %-22s %-10s %12s  %s", "列名", "型", "異なり数", "欠損"))
for (nm in names(dat)) {
  v <- dat[[nm]]
  say(sprintf("  %-22s %-10s %12s  %s", nm, class(v)[1],
              format(length(unique(v)), big.mark = ","),
              format(sum(is.na(v)), big.mark = ",")))
}
if (PEEK) {
  say("")
  say("  --peek: 先頭5行（**貼る前に中身を確認すること**）")
  print(head5)
}

# ===========================================================================
# §2 列の推定
# ===========================================================================
say("")
say("§2 どの列が何かを推定する（違っていれば引数で上書き）")

## 名前のパターンから探す。**当たらなければ引数で渡してもらう**
guess <- function(patterns, override) {
  if (!is.null(override)) {
    if (!(override %in% names(dat))) stop("指定された列がありません: ", override)
    return(override)
  }
  for (p in patterns) {
    hit <- grep(p, names(dat), ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

## 実データ（band_data_20251204.csv、1,379,999行 × 15列）の構造:
##   PCODE NO DAY GUID RING SPC SEX AGE STAT NDATE COMM YEAR SPNAMK SPNAME meshcode
##
##   PCODE    … 放鳥場所コード（例 470119。頭2桁が県コードで 47 = 沖縄）
##              **これがステーションにあたる**
##   GUID+RING … 足環。**RING 単独では系列をまたいで重複しうるので組で使う**
##   SPNAMK   … 半角カナの種名（ﾒｼﾞﾛ）。SPNAME は英名
##              ⚠ rds の splist は全角（メジロ）なので**直接は突き合わせられない**
##   meshcode … <int>。**JIS コード（6桁）ではなく mesh2_convex7.gpkg の行番号**
##   **緯度経度の列は無い** → ステーション間の距離は計算できない
COL <- list(
  station = guess(c("^pcode", "^station", "ステーション", "^site", "放鳥", "場所"),
                  .opt("--station")),
  ring    = guess(c("^ring", "足環", "^id$"),                        .opt("--ring")),
  species = guess(c("^spnamk", "^spname", "^species", "^sp$", "種名"), .opt("--sp-col")),
  lat     = guess(c("^lat", "緯度"),                                  .opt("--lat")),
  lon     = guess(c("^lon", "^lng", "経度"),                          .opt("--lon")),
  mesh    = guess(c("mesh", "メッシュ"),                              .opt("--mesh")),
  date    = guess(c("^day$", "^date", "^year", "年月日"),             .opt("--date")))

## 足環は GUID + RING の組で1個体。**片方だけでは一意にならない**
RING2 <- .opt("--ring2", if ("GUID" %in% names(dat)) "GUID" else NA_character_)
if (!is.na(RING2) && !(RING2 %in% names(dat)))
  stop("指定された列がありません: ", RING2)

for (k in names(COL))
  say(sprintf("  %-8s → %s", k, if (is.na(COL[[k]])) "**見つからない**" else COL[[k]]))
if (!is.na(RING2))
  say(sprintf("  %-8s → %s（%s と組で1個体）", "ring2", RING2, COL$ring))
say("")
say("  違っていれば: --station <列> --ring <列> --ring2 <列> --sp-col <列> --mesh <列>")

if (is.na(COL$ring))
  stop("個体を識別する列（足環番号）が見つかりません。--ring で指定してください。")
if (is.na(COL$station))
  stop("ステーションの列が見つかりません。--station で指定してください（実データでは PCODE）。")
if (is.na(COL$lat) || is.na(COL$lon))
  say("  ＊ 緯度経度の列が無いので、**ステーション間の距離は出さない**（件数だけ数える）")

# ===========================================================================
# §3 ステーション → 10km メッシュ
# ===========================================================================
say("")
hr(); say("§3 ステーションと10kmメッシュの対応"); hr()

## JIS 2次メッシュ（約10km四方）のコードを緯度経度から計算する。
##   1次メッシュ: p = floor(緯度 × 1.5)、u = floor(経度 − 100)
##   2次メッシュ: それぞれを8分割した q, v を付ける
## 例: 東京(35.7, 139.7) → p=53, q=4, u=39, v=5 → "533945"
mesh2code <- function(lat, lon) {
  p <- floor(lat * 1.5); q <- floor((lat * 1.5 - p) * 8)
  u <- floor(lon - 100); v <- floor((lon - 100 - u) * 8)
  ifelse(is.na(lat) | is.na(lon), NA_character_,
         sprintf("%02d%02d%d%d", p, u, q, v))
}

dat$.station <- as.character(dat[[COL$station]])

## メッシュコード。mesh 列があればそれを使い、無ければ座標から計算
if (!is.na(COL$mesh)) {
  dat$.mesh <- as.character(dat[[COL$mesh]])
  say("  メッシュは既存の列 `", COL$mesh, "` を使う")
  ## ★ **この値が JIS コードか行番号かで意味が変わる。** 範囲で見分ける。
  ## `effort$meshcode` は mesh2_convex7.gpkg の行番号（実測で 156〜13265）
  mv <- suppressWarnings(as.integer(dat$.mesh))
  if (!all(is.na(mv))) {
    rng <- range(mv, na.rm = TRUE)
    say(sprintf("    値の範囲 %d 〜 %d / 異なり %s",
                rng[1], rng[2], format(length(unique(mv)), big.mark = ",")))
    if (rng[2] <= 13454 && rng[1] >= 1) {
      say("    → **6桁の JIS コードではない。mesh2_convex7.gpkg の行番号**とみられる")
      say("      （effort$meshcode と同じ性質。範囲 1〜13,454 に収まっている）")
    } else if (rng[1] >= 300000) {
      say("    → 6桁の JIS 2次メッシュコードとみられる")
    } else {
      say("    → ⚠ どちらとも判断できない。--mesh で別の列を指定するか、要確認")
    }
  }
} else if (!is.na(COL$lat) && !is.na(COL$lon)) {
  dat$.mesh <- mesh2code(as.numeric(dat[[COL$lat]]), as.numeric(dat[[COL$lon]]))
  say("  メッシュは緯度経度から計算した（JIS 2次メッシュ）")
} else {
  stop("メッシュ列も緯度経度も無いので、ステーションをメッシュに割り当てられません。")
}

ok <- !is.na(dat$.mesh) & !is.na(dat$.station) & nzchar(dat$.station)
say(sprintf("  ステーションとメッシュの両方が分かる行: %s / %s（%.1f%%）",
            format(sum(ok), big.mark = ","), format(nrow(dat), big.mark = ","),
            100 * mean(ok)))
dat <- dat[ok, , drop = FALSE]

st <- unique(data.frame(station = dat$.station, mesh = dat$.mesh,
                        stringsAsFactors = FALSE))
n_station <- length(unique(st$station))
n_mesh    <- length(unique(st$mesh))
say("")
say("  **ステーション ", format(n_station, big.mark = ","), " 箇所**")
say("  **それが入るメッシュ ", format(n_mesh, big.mark = ","), " セル**")
say(sprintf("  → 1セルあたり平均 %.2f ステーション", n_station / n_mesh))

## ★ ここが核心。**1セルに2つ以上ステーションがあるなら、
##   その間の移動は現在の集計では見えていない**
per <- table(st$mesh)
say("")
say("  1メッシュあたりのステーション数の分布:")
tb <- table(as.integer(per))
for (k in names(tb))
  say(sprintf("    %3s ステーション: %5s セル%s", k, format(tb[[k]], big.mark = ","),
              if (as.integer(k) >= 2) "  ← この中の移動は今は見えない" else ""))
n_multi_cell <- sum(per >= 2)
say("")
say(sprintf("  **2つ以上のステーションを含むセル: %s / %s（%.1f%%）**",
            format(n_multi_cell, big.mark = ","), format(n_mesh, big.mark = ","),
            100 * n_multi_cell / n_mesh))
if (n_multi_cell == 0)
  say("  → **メッシュを細かくしても移動は増えない。** 1セル1ステーション")

# ===========================================================================
# §4 種ごとに「消えている移動」を数える
# ===========================================================================
say("")
hr(); say("§4 種ごとの移動 — メッシュ単位 vs ステーション単位"); hr()

if (is.na(COL$species)) {
  say("  ⚠ 種の列が見つからないので、全体をひとまとめに数える（--sp-col で指定可）")
  dat$.sp <- "（全体）"
} else {
  dat$.sp <- as.character(dat[[COL$species]])
}
## ★ 個体の識別子。**GUID + RING の組**。RING 単独だと系列をまたいで重複しうる
dat$.ring <- if (!is.na(RING2))
  paste(as.character(dat[[RING2]]), as.character(dat[[COL$ring]]), sep = "") else
  as.character(dat[[COL$ring]])
if (!is.na(RING2)) {
  n1 <- length(unique(as.character(dat[[COL$ring]])))
  n2 <- length(unique(dat$.ring))
  say(sprintf("  個体の識別: %s 単独なら %s 通り / %s と組で **%s 通り**",
              COL$ring, format(n1, big.mark = ","), RING2, format(n2, big.mark = ",")))
  if (n2 > n1)
    say("    → **組で使わないと別個体を同一視してしまう**（差 ",
        format(n2 - n1, big.mark = ","), "）")
}

## 種を絞るか、記録数の多い順に上位を見る
sp_list <- if (!is.null(SPECIES)) {
  if (!(SPECIES %in% dat$.sp)) stop("種が見つかりません: ", SPECIES)
  SPECIES
} else {
  names(sort(table(dat$.sp), decreasing = TRUE))[seq_len(min(TOPN, length(unique(dat$.sp))))]
}

## 個体ごとに「異なるメッシュの数」と「異なるステーションの数」を数える。
## **その差が、メッシュ集計で消えている移動。**
count_distinct <- function(id, key) {
  u <- unique(data.frame(id = id, key = key, stringsAsFactors = FALSE))
  tapply(u$key, u$id, length)
}

## ステーション間の距離（同じセル内）。細かいメッシュにする価値の目安
station_xy <- NULL
if (!is.na(COL$lat) && !is.na(COL$lon)) {
  sxy <- unique(data.frame(station = dat$.station,
                           lat = as.numeric(dat[[COL$lat]]),
                           lon = as.numeric(dat[[COL$lon]]),
                           stringsAsFactors = FALSE))
  sxy <- sxy[!duplicated(sxy$station), , drop = FALSE]
  station_xy <- setNames(split(sxy[, c("lat", "lon")], sxy$station), sxy$station)
}
dist_km <- function(a, b) {
  if (is.null(station_xy)) return(NA_real_)
  p <- station_xy[[a]]; q <- station_xy[[b]]
  if (is.null(p) || is.null(q)) return(NA_real_)
  dlat <- (q$lat - p$lat) * 111.32
  dlon <- (q$lon - p$lon) * 111.32 * cos(mean(c(p$lat, q$lat)) * pi / 180)
  sqrt(dlat^2 + dlon^2)
}

rows <- list()
for (sp in sp_list) {
  d <- dat[dat$.sp == sp, , drop = FALSE]
  n_rec <- nrow(d)
  n_ind <- length(unique(d$.ring))

  nm <- count_distinct(d$.ring, d$.mesh)       # 個体ごとの異なるメッシュ数
  ns <- count_distinct(d$.ring, d$.station)    # 個体ごとの異なるステーション数
  moved_mesh    <- sum(nm >= 2)
  moved_station <- sum(ns >= 2)
  ## **同じメッシュ内で別ステーション**＝いま消えている移動
  hidden <- sum(ns >= 2 & nm < 2)

  ## つながり（独立な組）の数も数える
  pair_of <- function(key) {
    u <- unique(data.frame(id = d$.ring, k = key, stringsAsFactors = FALSE))
    sp2 <- split(u$k, u$id)
    pr <- unlist(lapply(sp2[lengths(sp2) >= 2], function(v) {
      cb <- utils::combn(sort(v), 2); paste(cb[1, ], cb[2, ], sep = "")
    }), use.names = FALSE)
    length(unique(pr))
  }
  pair_mesh <- pair_of(d$.mesh)
  pair_st   <- pair_of(d$.station)

  ## 同じセル内のステーション間距離（消えている移動の距離）
  dmed <- NA_real_
  if (!is.null(station_xy) && hidden > 0) {
    u <- unique(data.frame(id = d$.ring, st = d$.station, ms = d$.mesh,
                           stringsAsFactors = FALSE))
    sp2 <- split(u, u$id)
    dd <- unlist(lapply(sp2, function(x) {
      if (nrow(x) < 2 || length(unique(x$ms)) >= 2) return(NULL)
      cb <- utils::combn(x$st, 2)
      vapply(seq_len(ncol(cb)), function(i) dist_km(cb[1, i], cb[2, i]), numeric(1))
    }), use.names = FALSE)
    if (length(dd)) dmed <- median(dd, na.rm = TRUE)
  }

  say("")
  say("  【", sp, "】 記録 ", format(n_rec, big.mark = ","),
      " / 個体 ", format(n_ind, big.mark = ","))
  say(sprintf("    メッシュ単位  : 移動 %5d 個体 / つながり %5d 本  ← いまの解析はこれ",
              moved_mesh, pair_mesh))
  say(sprintf("    ステーション単位: 移動 %5d 個体 / つながり %5d 本  ← 細かくすれば見える上限",
              moved_station, pair_st))
  say(sprintf("    **差（同じセル内の別ステーション）: %d 個体 / つながり %d 本**",
              hidden, pair_st - pair_mesh))
  if (is.finite(dmed))
    say(sprintf("    その移動の距離の中央値: %.1f km", dmed))

  rows[[length(rows) + 1L]] <- data.frame(
    species = sp, n_record = n_rec, n_ind = n_ind,
    moved_mesh = moved_mesh, pair_mesh = pair_mesh,
    moved_station = moved_station, pair_station = pair_st,
    hidden_ind = hidden, hidden_pair = pair_st - pair_mesh,
    hidden_km_med = dmed)
}

res <- do.call(rbind, rows)
say("")
hr(); say("  まとめ"); hr()
print(format(res, digits = 3), row.names = FALSE)

say("")
say("  ★ 読み方")
say("    pair_mesh    … いまの解析で使えている「つながり」の本数")
say("    pair_station … ステーション単位で数えた場合の本数")
say("    hidden_pair  … **その差。メッシュ集計で消えている情報**")
say("")
say("    合成データでの実験（examples/identify_conn.R）では")
say("      つながり 〜5本  … SE が真値の3〜4倍。符号一致 0.50（推定不能）")
say("      つながり 11〜20本 … SE が真値の1/3（実用域）")
say("    **hidden_pair が大きければ、メッシュを細かくする価値がある。**")

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(res, OUT, row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, "（集計値のみ）")
