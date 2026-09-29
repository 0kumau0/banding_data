# examples/band_region_scan.R ------------------------------------------------
#
# **「狭い範囲 × 細かいメッシュ」で、計算可能かつ推定可能な構成があるかを探す。**
#
#   Rscript examples/band_region_scan.R
#   Rscript examples/band_region_scan.R --species ｼｼﾞｭｳｶﾗ --sp-col SPNAMK
#   Rscript examples/band_region_scan.R --width 20,30,50,80 --res 0.5,1,2,5
#
# 生データを**読むだけ**。書き出すのは --out の CSV だけ（集計値のみ）。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-29）
#
# ここまでで分かっていること:
#
#   関東 10km … つながり  4本 → **推定不能**（SE が真値の3〜4倍、符号一致 0.50）
#   全国 10km … つながり 14本 → 実用域の入口。だが **SGD 300反復で1,332日**
#   全国  5km … つながり 16本 → **5,330日**
#
# **計算できる構成では推定できず、推定できる構成では計算できない。**
#
# ## ただし「範囲」と「解像度」を別々に動かしていた
#
# 同時に動かせば相殺できる:
#
#   範囲を狭める   → セル数が減る
#   解像度を上げる → セル数が増えるが、つながりが増える
#
# 実際、**1セルに 101 / 64 / 57 / 53 / 50 ステーションという密集地がある**
# （`examples/band_station_check.R`）。そこを 1km で切れば検出器が100個に分かれる。
#
#   50km × 50km を 1km メッシュ → **ncell 2,500**（クマの 8,497 より小さい）
#
# **その範囲にシジュウカラの移動が十分あるか**を数えるのがこのスクリプト。
#
# ---------------------------------------------------------------------------
# ★ やること
#
# 候補となる正方形の範囲（幅 W）と解像度（r）の組み合わせごとに、
#
#   ncell    = (W/r)^2
#   neffort  = 検出器のあるセル × 年 の組み合わせ数
#   nind     = その範囲内で捕獲された個体数
#   つながり = 範囲内で「異なるセル」で捕まった個体が結んだ組の数
#   loglf    = 上から見積もった1回の所要時間
#
# を計算し、**「つながり ≥ 11 かつ loglf が現実的」な構成**を探す。
#
# 範囲の置き場所は、**捕獲のあるステーションを中心に総当たり**して最良を取る。
#
# ⚠ **範囲を狭めると、外へ出た移動は観測から消える**（端の効果）。
#   密度推定の対象もその範囲に限定される。**これは代償として残る。**
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--csv", "--place", "--species", "--sp-col", "--width", "--res",
            "--step", "--out", "--top")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

## 既定は **半角カナ**（SPNAMK の表記）。全角のシジュウカラでは一致しない。
## コマンドラインで日本語を渡すと Windows で化けることがあるので、
## 既定値はエスケープで書いておく（ｼｼﾞｭｳｶﾗ）
SPECIES <- .opt("--species", "ｼｼﾞｭｳｶﾗ")
SP_COL  <- .opt("--sp-col", "SPNAMK")
## 2026-09-29: 最初 120km までで走らせたら**最大の幅で最良**になった。
## 探索範囲の端で最適になるのは範囲が狭すぎる印。しかも計算に余裕があった
## （W120/r5 で SGD 300反復が 1.07日）ので、**大きい側へ広げる**。
WIDTHS  <- as.numeric(strsplit(.opt("--width", "50,80,120,200,300,500,800"), ",")[[1]])
RESOL   <- as.numeric(strsplit(.opt("--res",   "1,2,5,10,20"),   ",")[[1]])
STEP    <- as.numeric(.opt("--step", "10"))     # 範囲の中心をずらす刻み（km）
TOPN    <- as.integer(.opt("--top", "12"))
OUT     <- .opt("--out", "results/band_region_scan.csv")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
CSV   <- .opt("--csv",   if (exists("BAND_CSV")) BAND_CSV else NA_character_)
PLACE <- .opt("--place", file.path(if (exists("DATA_ROOT")) DATA_ROOT else "../..",
                                   "PLACE.DBF"))
for (p in c(CSV, PLACE))
  if (is.na(p) || !file.exists(p)) stop("見つかりません: ", p)

## ⚠ `Sys.setlocale("LC_ALL", ...)` を呼ばないこと。UTF-8 のファイルでは
## それ以降の行のパースが壊れる（2026-09-29 に踏んだ。CLAUDE.md 参照）
suppressMessages({ library(sf); library(dplyr); library(readr); library(foreign) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 74))

say("=========================================================================")
say("  狭い範囲 × 細かいメッシュ の探索: ", SPECIES)
say("=========================================================================")

# ===========================================================================
# §1 読み込み（examples/band_station_check.R と同じ経路）
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

## 投影座標（EPSG:3100、メートル）→ km。**メッシュには落とさない。**
## ここでは任意の解像度で切り直すので、既存の gpkg は使わない
pts <- place %>% dplyr::select(PCODE, Lat, Lon) %>% distinct(PCODE, .keep_all = TRUE) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% st_transform(3100)
xy <- st_coordinates(pts) / 1000
stxy <- data.frame(PCODE = pts$PCODE, x = xy[, 1], y = xy[, 2], stringsAsFactors = FALSE)

if (!(SP_COL %in% names(band))) stop("列がありません: ", SP_COL)
sp_vals <- unique(band[[SP_COL]])
if (!(SPECIES %in% sp_vals)) {
  cand <- grep(substr(SPECIES, 1, 2), sp_vals, value = TRUE)
  stop("種が見つかりません: ", SPECIES, "\n  近そうな値: ",
       paste(utils::head(cand, 10), collapse = " / "),
       "\n  --species で正確な値を、--sp-col で列を指定してください。")
}

d <- band %>% filter(.data[[SP_COL]] == SPECIES) %>%
  dplyr::select(PCODE, GUID, RING, YEAR) %>%
  inner_join(stxy, by = "PCODE")
d$ind <- paste(d$GUID, d$RING, sep = "")

say(sprintf("  %s: 記録 %s / 個体 %s / ステーション %s",
            SPECIES, format(nrow(d), big.mark = ","),
            format(length(unique(d$ind)), big.mark = ","),
            format(length(unique(d$PCODE)), big.mark = ",")))
say(sprintf("  範囲: x %.0f〜%.0f km / y %.0f〜%.0f km",
            min(d$x), max(d$x), min(d$y), max(d$y)))

## ★ 個体ごとの「捕まったステーションの集合」。**2箇所以上の個体だけが
## つながりを生む**ので、それを先に抜いておく（以降の走査が軽くなる）
u <- unique(d[, c("ind", "PCODE", "x", "y")])
n_st_per_ind <- table(u$ind)
movers <- names(n_st_per_ind)[n_st_per_ind >= 2L]
um <- u[u$ind %in% movers, , drop = FALSE]
say(sprintf("  2箇所以上のステーションで捕まった個体: **%s**（全国・解像度に依らない上限）",
            format(length(movers), big.mark = ",")))

## ★ 努力は**全種の標識活動**。この種が捕れた場所だけに絞ると
## neffort を過小評価し、計算時間を甘く見積もることになる
eff <- band %>% dplyr::select(PCODE, YEAR) %>% distinct() %>%
  inner_join(stxy, by = "PCODE")
say(sprintf("  努力の組（全種のステーション × 年）: %s", format(nrow(eff), big.mark = ",")))

# ===========================================================================
# §2 コストの見積もり
# ===========================================================================
## `pcap`（修正後）: results/pcap_bench.csv の実測から。
##   基準 nmu 400 / neffort 25 / nind 19,200 → 7.02秒。nind の指数 1.10
## `advdiff`: クマ（ncell 8,497）で loglf 181秒のうち約4%（pcap が96%）→ 約7秒。
##   O(ncell^2.5)
est_sec <- function(ncell, nind, neff) {
  pcap <- 7.02 * (as.numeric(ncell) / 400) * (as.numeric(neff) / 25) *
    (as.numeric(nind) / 19200)^1.10
  advd <- 7 * (as.numeric(ncell) / 8497)^2.5
  pcap + advd
}
fmt_t <- function(s) if (s < 60) sprintf("%.0f秒", s) else
  if (s < 3600) sprintf("%.1f分", s / 60) else
    if (s < 86400) sprintf("%.1f時間", s / 3600) else sprintf("%.1f日", s / 86400)

LL_PER_ITER <- 5.2; N_ITER <- 300

# ===========================================================================
# §3 走査
# ===========================================================================
say("")
hr(); say("§3 範囲の幅 × 解像度 の走査"); hr()
say("  幅: ", paste(WIDTHS, collapse = ", "), " km")
say("  解像度: ", paste(RESOL, collapse = ", "), " km")
say(sprintf("  範囲の中心は %.0f km 刻みで総当たり", STEP))

## ★ 中心の候補は**移動個体がいる場所の周りだけ**でよい。
## 移動が2件も入らない範囲は、そもそも評価する意味が無い。
## 全国を総当たりすると数万点になって走査が終わらない（移動個体は33件しかない）
if (!nrow(um)) stop("2箇所以上で捕獲された個体がいない。この種では探索できない。")
cx <- seq(floor(min(um$x) / STEP) * STEP, ceiling(max(um$x) / STEP) * STEP, by = STEP)
cy <- seq(floor(min(um$y) / STEP) * STEP, ceiling(max(um$y) / STEP) * STEP, by = STEP)
centers <- expand.grid(cx = cx, cy = cy)
keep <- vapply(seq_len(nrow(centers)), function(i)
  any(abs(um$x - centers$cx[i]) <= max(WIDTHS) / 2 &
      abs(um$y - centers$cy[i]) <= max(WIDTHS) / 2), logical(1))
centers <- centers[keep, , drop = FALSE]
say(sprintf("  中心の候補: %s 点（移動個体の周辺のみ）",
            format(nrow(centers), big.mark = ",")))

## ある範囲・ある解像度での「つながり」の本数
pairs_in <- function(sub, r) {
  if (!nrow(sub)) return(0L)
  cid <- paste(floor(sub$x / r), floor(sub$y / r), sep = "_")
  s <- split(cid, sub$ind)
  s <- s[vapply(s, function(v) length(unique(v)) >= 2L, logical(1))]
  if (!length(s)) return(0L)
  pr <- unlist(lapply(s, function(v) {
    v <- sort(unique(v)); cb <- utils::combn(v, 2)
    paste(cb[1, ], cb[2, ], sep = "|")
  }), use.names = FALSE)
  length(unique(pr))
}

rows <- list()
for (W in WIDTHS) {
  h <- W / 2
  for (i in seq_len(nrow(centers))) {
    x0 <- centers$cx[i]; y0 <- centers$cy[i]
    inbox <- function(X, Y) abs(X - x0) <= h & abs(Y - y0) <= h
    ## 範囲内の努力・記録・移動個体
    e <- eff[inbox(eff$x, eff$y), , drop = FALSE]
    if (nrow(e) < 4) next
    dd <- d[inbox(d$x, d$y), , drop = FALSE]
    nind <- length(unique(dd$ind))
    if (nind < 50) next
    mm <- um[inbox(um$x, um$y), , drop = FALSE]

    for (r in RESOL) {
      np <- pairs_in(mm, r)
      if (np < 2) next
      ncell <- (W / r)^2
      ## 努力の数＝(検出器セル × 年) の異なり
      neff <- length(unique(paste(floor(e$x / r), floor(e$y / r), e$YEAR, sep = "_")))
      sec <- est_sec(ncell, nind, neff)
      rows[[length(rows) + 1L]] <- data.frame(
        W = W, res = r, cx = x0, cy = y0,
        ncell = ncell, n_eff = neff, nind = nind, n_pair = np,
        loglf_sec = sec, sgd_day = sec * LL_PER_ITER * N_ITER / 86400)
    }
  }
}

if (!length(rows)) stop("条件に合う構成が1つも見つからなかった。--width / --res を広げること。")
res <- do.call(rbind, rows)
say(sprintf("  評価した構成: %s", format(nrow(res), big.mark = ",")))

# ===========================================================================
# §4 結果
# ===========================================================================
say("")
hr(); say("  ★ 推定できて、かつ計算できる構成"); hr()

## 識別可能性の実験（examples/identify_conn.R）の閾値
##   〜5本 … 推定不能 / 11〜20本 … 実用域
FEASIBLE_DAY <- 30       # SGD 300反復 × 多点出発3点で90日を上限とみる
good <- res[res$n_pair >= 11 & res$sgd_day <= FEASIBLE_DAY, , drop = FALSE]

if (!nrow(good)) {
  say("  **該当なし。** つながり11本以上かつ SGD 300反復が", FEASIBLE_DAY, "日以内、")
  say("  という条件を満たす構成は見つからなかった。")
  say("")
  say("  参考: つながりが多い順の上位")
  b <- res[order(-res$n_pair, res$sgd_day), ][seq_len(min(TOPN, nrow(res))), ]
  b$loglf <- vapply(b$loglf_sec, fmt_t, character(1))
  print(b[, c("W", "res", "ncell", "n_eff", "nind", "n_pair", "loglf", "sgd_day")],
        row.names = FALSE)
  say("")
  say("  参考: 計算が軽い順（つながり8本以上）")
  b2 <- res[res$n_pair >= 8, ]
  if (nrow(b2)) {
    b2 <- b2[order(b2$sgd_day), ][seq_len(min(TOPN, nrow(b2))), ]
    b2$loglf <- vapply(b2$loglf_sec, fmt_t, character(1))
    print(b2[, c("W", "res", "ncell", "n_eff", "nind", "n_pair", "loglf", "sgd_day")],
          row.names = FALSE)
  }
} else {
  say(sprintf("  **%s 構成が該当**（つながり ≥ 11 かつ SGD 300反復 ≤ %d日）",
              format(nrow(good), big.mark = ","), FEASIBLE_DAY))
  say("")
  b <- good[order(-good$n_pair, good$sgd_day), ][seq_len(min(TOPN, nrow(good))), ]
  b$loglf <- vapply(b$loglf_sec, fmt_t, character(1))
  print(b[, c("W", "res", "cx", "cy", "ncell", "n_eff", "nind", "n_pair", "loglf", "sgd_day")],
        row.names = FALSE)
}

## ★★ **境界線**: つながり N 本を買うのに最低何日かかるか。
## 合否より、この曲線のほうが判断に使える
say("")
hr(); say("  ★ 境界線 — つながり N 本を買うのに必要な最小の計算時間"); hr()
say(sprintf("  %8s %10s %8s %7s %8s %9s %12s",
            "つながり", "最小SGD日数", "幅km", "解像度", "ncell", "nind", "neffort"))
fr <- do.call(rbind, lapply(sort(unique(res$n_pair)), function(np) {
  s <- res[res$n_pair >= np, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  s[which.min(s$sgd_day), , drop = FALSE]
}))
for (i in seq_len(nrow(fr)))
  say(sprintf("  %8d %10.1f %8.0f %7.1f %8s %9s %12s",
              fr$n_pair[i], fr$sgd_day[i], fr$W[i], fr$res[i],
              format(fr$ncell[i], big.mark = ","),
              format(fr$nind[i], big.mark = ","),
              format(fr$n_eff[i], big.mark = ",")))
say("")
say("  ＊ 「つながり N 本**以上**を満たす構成のうち、最も軽いもの」")
say("  ＊ 多点出発が3点要るので、実際の所要は上の値 × 3")

say("")
say("  ★ 比較のための既知の構成")
say(sprintf("    関東 10km : ncell    943 / nind 16,308 / neffort   958 / つながり  4"))
say(sprintf("    全国 10km : ncell 13,454 / nind 33,551 / neffort 4,229 / つながり 14"))
say("    クマ      : ncell  8,497 / nind    109 / neffort   227 / loglf 181秒")
say("")
say("  ★ 判定の基準（examples/identify_conn.R、合成データ24試行）")
say("    つながり 〜5本   … SE が真値の3〜4倍。符号一致 0.50（**推定不能**）")
say("    つながり 11〜20本 … SE が真値の1/3（**実用域**）")
say("    つながり 21〜40本 … SE が真値の1/5")
say("")
say("  ⚠ **範囲を狭めると、外へ出た移動は観測から消える**（端の効果）。")
say("    密度推定の対象もその範囲に限定される。これは代償として残る。")
say("  ⚠ 解像度を上げると advdiff の CFL 条件が厳しくなる（stepad を増やす必要）。")

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(res, OUT, row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, sprintf("（%s 構成）", format(nrow(res), big.mark = ",")))
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
