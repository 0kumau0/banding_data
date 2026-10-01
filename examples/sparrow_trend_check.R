# examples/sparrow_trend_check.R ---------------------------------------------
#
# **スズメの個体数トレンドを標識データから推定できるか、データ側の下検分。**
#
#   Rscript examples/sparrow_trend_check.R
#   Rscript examples/sparrow_trend_check.R --species ｽｽﾞﾒ,ﾒｼﾞﾛ,ｳｸﾞｲｽ
#   Rscript examples/sparrow_trend_check.R --breaks 1961,1971,1981,1991,2001,2011
#
# 生データを**読むだけ**。書き出すのは --out の CSV（集計値のみ）。
# **既定の出力は集計値だけなので、そのまま貼れる**（生の値は --peek のときのみ）。
#
# ---------------------------------------------------------------------------
# 検討していること（2026-10-01）
#
# 「スズメの減少が指摘されている。標識データは1960年代からあるので、
#   10年ごとに区切って全国密度を推定し、変化を見られないか」
#
# ## なぜ下検分が要るのか
#
# スズメは **2009〜2018 で個体 26,113・複数回捕獲 564（2.2%）・移動 3件**。
# **個体数は十分あるのに空間的再捕獲がほとんど無い。**
#
# SCR 系では 密度 `D` / 検出率 `g0` / 行動圏スケール `σ`（透過性）が絡む。
# σ を決めるのは空間的再捕獲なので、**10年ごとに独立に解くと σ の推定誤差が
# そのまま密度のトレンドに化ける。**
#
# → **σ は全期間をプールして1回だけ推定し、固定して期間ごとに D と g0 を解く**
#   という設計にしたい。そのために、以下を数える。
#
# ## 確かめること
#
#   1. 期間ごとの規模（記録・個体・複数回・移動・つながり）
#      → 各期間で g0 が推定できるか / プールして σ が何本になるか
#   2. **ステーションの継続性** — 全期間で稼働している地点は何箇所か
#      → 調査地の移動という交絡に対処できるか
#   3. 努力量の時代変化（ステーション数・延べ調査日）
#      → 「努力の非標準化」がどれだけ深刻か
#   4. **スズメがどんな環境の地点で捕れているか**（PLACE.DBF の HABITAT）
#      → スズメは人の生活圏の鳥だが、標識地はヨシ原・森林が多い。
#        「ついでに捕れている」度合い
#   5. ★ **期間をまたぐ個体** — 1975年と1985年に捕まった個体は、
#      どちらの期間に属するのか。多ければ期間ごとのデータが独立でなくなり、
#      扱いを決める必要がある
#      （**標識番号の再利用は制度上無い**とのこと（2026-10-01、ユーザ確認）。
#        したがって長い間隔は再利用ではなく、実際の長寿個体か記録の誤り）
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--dbf", "--place", "--splist", "--species", "--breaks",
            "--res", "--maxgap", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--peek"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--peek"), collapse = " "))

PEEK   <- "--peek" %in% .args
SPP    <- strsplit(.opt("--species",
  "ｽｽﾞﾒ,ﾒｼﾞﾛ,ｳｸﾞｲｽ"), ",")[[1]]  # ｽｽﾞﾒ,ﾒｼﾞﾛ,ｳｸﾞｲｽ
BREAKS <- as.integer(strsplit(.opt("--breaks", "1961,1971,1981,1991,2001,2011"), ",")[[1]])
RES    <- as.numeric(.opt("--res", "20"))     # つながりを数えるメッシュ（km）
MAXGAP <- as.numeric(.opt("--maxgap", "10"))  # これを超える間隔は足環の再利用を疑う
OUT    <- .opt("--out", "results/sparrow_trend_check")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
RT    <- if (exists("DATA_ROOT")) DATA_ROOT else "../.."
DBF   <- .opt("--dbf",    file.path(RT, "6118LANDBIRD.DBF"))
PLACE <- .opt("--place",  file.path(RT, "PLACE.DBF"))
SPLIST<- .opt("--splist", file.path(RT, "Splist.DBF"))
for (p in c(DBF, PLACE))
  if (!file.exists(p)) stop("見つかりません: ", p, "\n--dbf / --place で指定してください。")

## ⚠ Sys.setlocale は呼ばない（UTF-8 ファイルではパースが壊れる。CLAUDE.md）
suppressMessages({ library(sf); library(dplyr); library(foreign) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 76))

say("=========================================================================")
say("  スズメのトレンド推定 — データ側の下検分")
say("=========================================================================")
say("  標識データ: ", DBF, sprintf("（%.0f MB）", file.size(DBF) / 1e6))
say("  ⚠ read.dbf は遅い。数分かかることがある")

# ===========================================================================
# §1 読み込み
# ===========================================================================
say("")
say("§1 読み込み")
d <- read.dbf(DBF, as.is = TRUE)
say(sprintf("  **%s 行 × %d 列**", format(nrow(d), big.mark = ","), ncol(d)))
say("  列: ", paste(names(d), collapse = ", "))

## 年。YEAR が無ければ DAY（yyyymmdd）から取る
if (!("YEAR" %in% names(d))) {
  if (!("DAY" %in% names(d))) stop("YEAR も DAY も無いので年が分からない")
  d$YEAR <- as.integer(substr(as.character(d$DAY), 1, 4))
  say("  YEAR は DAY（yyyymmdd）から作った")
}
d$YEAR <- as.integer(d$YEAR)
yr <- range(d$YEAR, na.rm = TRUE)
say(sprintf("  **年の範囲: %d 〜 %d**（%d 年分）", yr[1], yr[2], yr[2] - yr[1] + 1))

## 種名。DBF は SPC（コード）だけのことがあるので Splist.DBF で引く
sp_col <- NA_character_
for (nm in c("SPNAMK", "SPNAME", "SPC"))
  if (nm %in% names(d)) { sp_col <- nm; break }
if (identical(sp_col, "SPC") && file.exists(SPLIST)) {
  sl <- read.dbf(SPLIST, as.is = TRUE)
  say("  Splist.DBF: ", nrow(sl), " 行 / 列: ", paste(names(sl), collapse = ", "))
  if (all(c("SPC", "SPNAMK") %in% names(sl))) {
    d <- d %>% left_join(sl %>% dplyr::select(SPC, SPNAMK) %>% distinct(SPC, .keep_all = TRUE),
                         by = "SPC")
    sp_col <- "SPNAMK"
    say("  種名は Splist.DBF の SPNAMK を結合した")
  }
}
if (is.na(sp_col)) stop("種を表す列が見つかりません")
d$.sp <- as.character(d[[sp_col]])
say("  種の列: ", sp_col, "（", length(unique(d$.sp)), " 種）")

miss <- setdiff(SPP, unique(d$.sp))
if (length(miss)) {
  cand <- unique(d$.sp)
  say("  ⚠ 見つからない種: ", paste(miss, collapse = ", "))
  say("    （--species で正確な表記を指定。--peek で一覧の一部が見られます）")
  if (PEEK) print(utils::head(sort(cand), 40))
  SPP <- setdiff(SPP, miss)
  if (!length(SPP)) stop("対象の種が1つも無い")
}

## 個体は GUID + RING の組
if (!all(c("GUID", "RING", "PCODE") %in% names(d)))
  stop("GUID / RING / PCODE のいずれかがありません")
d$.ring <- paste(as.character(d$GUID), as.character(d$RING), sep = "")
d$PCODE <- as.character(d$PCODE)

## 期間（10年区切り）
bk <- c(BREAKS, yr[2] + 1L)
d$.period <- cut(d$YEAR, breaks = bk, right = FALSE,
                 labels = sprintf("%d-%d", bk[-length(bk)], bk[-1] - 1L))
say("  期間: ", paste(levels(d$.period), collapse = " / "))
n_out <- sum(is.na(d$.period))
if (n_out) say(sprintf("  ⚠ 期間の外の記録 %s 件は除外", format(n_out, big.mark = ",")))
d <- d[!is.na(d$.period), , drop = FALSE]

# ===========================================================================
# §2 地点の座標と環境
# ===========================================================================
say("")
hr(); say("§2 地点"); hr()
place <- read.dbf(PLACE, as.is = TRUE) %>%
  mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE)) %>%
  filter(PCODE != 380036)
source("functions.R", encoding = "UTF-8")
place$Lat <- sapply(place$LAT, convert_to_decimal)
place$Lon <- sapply(place$LONG, convert_to_decimal)
place <- place[is.finite(place$Lat) & is.finite(place$Lon), , drop = FALSE]
place$PCODE <- as.character(place$PCODE)

pts <- place %>% dplyr::select(PCODE, Lat, Lon) %>% distinct(PCODE, .keep_all = TRUE) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% st_transform(3100)
xy <- st_coordinates(pts) / 1000
stxy <- data.frame(PCODE = pts$PCODE, x = xy[, 1], y = xy[, 2], stringsAsFactors = FALSE)

d <- d %>% inner_join(stxy, by = "PCODE")
say(sprintf("  座標が引ける記録: %s 件", format(nrow(d), big.mark = ",")))
d$.cell <- paste(floor(d$x / RES), floor(d$y / RES), sep = "_")
say(sprintf("  つながりは **%.0f km メッシュ**で数える", RES))

# ===========================================================================
# §3 ★ 足環の再利用の検査
# ===========================================================================
say("")
hr(); say("§3 期間をまたぐ個体と、再捕獲の間隔"); hr()
say("  **標識番号の再利用は制度上無い**（2026-10-01、ユーザ確認）ので、")
say("  長い間隔は実際の長寿個体か記録の誤り。")
say("  ここで見たいのは **期間をまたぐ個体がどれだけいるか**。")
say("  多ければ期間ごとのデータが独立でなくなり、扱いを決める必要がある。")
say("")
say(sprintf("    %-8s %9s %9s %7s %7s %7s %13s %13s",
            "種", "再捕獲個体", "間隔中央", "9割点", "最大",
            "", "期間をまたぐ", "%.0f年超" ))
for (sp in SPP) {
  x <- d[d$.sp == sp, , drop = FALSE]
  g  <- tapply(x$YEAR, x$.ring, function(v) max(v) - min(v))
  np_ind <- tapply(as.character(x$.period), x$.ring, function(v) length(unique(v)))
  g <- g[g > 0]
  if (!length(g)) { say(sprintf("    %-8s 再捕獲なし", sp)); next }
  say(sprintf("    %-8s %9s %9.0f %7.0f %7.0f %7s %13s %13s",
              sp, format(length(g), big.mark = ","),
              stats::median(g), stats::quantile(g, 0.9, names = FALSE), max(g), "",
              format(sum(np_ind >= 2L), big.mark = ","),
              format(sum(g > MAXGAP), big.mark = ",")))
}
say("")
say("  ★ 読み方")
say("    **期間をまたぐ** … 2つ以上の期間で捕獲された個体。")
say("      多ければ、期間ごとの解析で同じ個体を二重に数えることになる。")
say("      （初回捕獲の期間に割り当てる、またはまたぐ個体を除く、などの決めが要る）")
say(sprintf("    **%.0f年超** … 記録の誤りの可能性。多ければ中身を確認すること", MAXGAP))

# ===========================================================================
# §4 期間ごとの規模
# ===========================================================================
say("")
hr(); say("§4 期間ごとの規模"); hr()

pair_count <- function(id, key) {
  u <- unique(data.frame(id = id, k = key, stringsAsFactors = FALSE))
  s <- split(u$k, u$id); s <- s[lengths(s) >= 2]
  if (!length(s)) return(0L)
  length(unique(unlist(lapply(s, function(v) {
    cb <- utils::combn(sort(as.character(v)), 2)
    paste(cb[1, ], cb[2, ], sep = "")
  }), use.names = FALSE)))
}

rows <- list()
for (sp in SPP) {
  say("")
  say("  【", sp, "】")
  say(sprintf("    %-10s %9s %9s %8s %8s %9s %9s %11s",
              "期間", "記録", "個体", "複数回", "移動", "つながり", "地点", "延べ調査日"))
  for (pd in levels(d$.period)) {
    x <- d[d$.sp == sp & d$.period == pd, , drop = FALSE]
    e <- d[d$.period == pd, , drop = FALSE]          # 努力は全種の標識活動
    if (!nrow(x)) { say(sprintf("    %-10s %9s", pd, "—")); next }
    ncap  <- table(x$.ring)
    ncell <- tapply(x$.cell, x$.ring, function(v) length(unique(v)))
    r <- data.frame(species = sp, period = pd, n_record = nrow(x),
                    n_ind = length(ncap), n_multi = sum(ncap > 1),
                    n_moved = sum(ncell >= 2), n_pair = pair_count(x$.ring, x$.cell),
                    n_station = length(unique(e$PCODE)),
                    station_days = nrow(unique(e[, c("PCODE", "YEAR", "DAY")])))
    rows[[length(rows) + 1L]] <- r
    say(sprintf("    %-10s %9s %9s %8s %8s %9s %9s %11s", pd,
                format(r$n_record, big.mark = ","), format(r$n_ind, big.mark = ","),
                format(r$n_multi, big.mark = ","), format(r$n_moved, big.mark = ","),
                format(r$n_pair, big.mark = ","), format(r$n_station, big.mark = ","),
                format(r$station_days, big.mark = ",")))
  }
  ## ★ プールしたときのつながり（σ を1回だけ推定する設計の前提）
  x <- d[d$.sp == sp, , drop = FALSE]
  say(sprintf("    %-10s %9s %9s %8s %8s %9s",
              "**全期間**", format(nrow(x), big.mark = ","),
              format(length(unique(x$.ring)), big.mark = ","), "",
              format(sum(tapply(x$.cell, x$.ring, function(v) length(unique(v))) >= 2), big.mark = ","),
              format(pair_count(x$.ring, x$.cell), big.mark = ",")))
}

say("")
say("  ★ 読み方")
say("    **つながり（全期間）** … σ をプールして推定したときの情報量。")
say("      合成データでは 2本以上で推定可能 / 11〜20本で実用域")
say("    **複数回（期間ごと）** … 各期間で g0 を推定できるか")
say("    **延べ調査日** … 努力量の代理。時代変化が大きければ交絡が深刻")

# ===========================================================================
# §5 ステーションの継続性
# ===========================================================================
say("")
hr(); say("§5 ステーションの継続性（調査地の移動という交絡への対処可能性）"); hr()
st_pd <- unique(d[, c("PCODE", ".period")])
n_pd <- table(st_pd$PCODE)
np <- length(levels(d$.period))
say(sprintf("  稼働したステーション: %s 箇所", format(length(n_pd), big.mark = ",")))
say("  何期間に現れるか:")
tb <- table(as.integer(n_pd))
for (k in names(tb))
  say(sprintf("    %2s 期間: %6s 箇所%s", k, format(tb[[k]], big.mark = ","),
              if (as.integer(k) == np) "  ← 全期間で稼働" else ""))
full <- names(n_pd)[n_pd == np]
say("")
say(sprintf("  **全 %d 期間で稼働: %s 箇所**", np, format(length(full), big.mark = ",")))
if (length(full)) {
  xs <- d[d$PCODE %in% full & d$.sp == SPP[1], , drop = FALSE]
  say(sprintf("    そこでの %s の記録: %s 件 / 個体 %s", SPP[1],
              format(nrow(xs), big.mark = ","),
              format(length(unique(xs$.ring)), big.mark = ",")))
  say("    期間ごとの記録数:")
  for (pd in levels(d$.period))
    say(sprintf("      %-10s %8s", pd,
                format(sum(xs$.period == pd), big.mark = ",")))
}
say("")
say("  ★ ここが多ければ、**継続地点だけに限定した解析**ができる。")
say("    調査地の移動という交絡を断てるので、トレンドの信頼性が大きく上がる")

# ===========================================================================
# §6 スズメはどんな環境の地点で捕れているか
# ===========================================================================
if ("HABITAT" %in% names(place)) {
  say("")
  hr(); say("§6 捕獲地点の環境（PLACE.DBF の HABITAT）"); hr()
  hb <- place %>% dplyr::select(PCODE, HABITAT) %>% distinct(PCODE, .keep_all = TRUE)
  dh <- d %>% left_join(hb, by = "PCODE")
  say(sprintf("    %-16s %12s %12s", "環境", SPP[1], "全種"))
  tot <- table(dh$HABITAT)
  t1  <- table(dh$HABITAT[dh$.sp == SPP[1]])
  ord <- names(sort(t1, decreasing = TRUE))
  for (k in utils::head(ord, 12))
    say(sprintf("    %-16s %12s %12s", k, format(t1[[k]], big.mark = ","),
                format(if (k %in% names(tot)) tot[[k]] else 0, big.mark = ",")))
  say("")
  say("  ★ **スズメは人の生活圏の鳥**だが、標識地はヨシ原・森林が多い。")
  say("    「ついでに捕れている」なら、密度の水準は全国を代表しない")
  say("    （トレンドは、偏りが時代を通じて一定なら有効でありうる）")
}

# ===========================================================================
# §7 保存
# ===========================================================================
res <- do.call(rbind, rows)
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(res, paste0(OUT, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, ".csv（集計値のみ）")
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
say("")
say("  ★ この下検分で見るべき4点")
say("    1. **全期間のつながり（§4）** … σ をプールして推定できるか。**ここが本丸**")
say("    2. 継続ステーション（§5）… 調査地の移動という交絡を断てるか")
say("    3. 努力量の時代変化（§4 の延べ調査日）… 交絡の深刻さ")
say("    4. 期間をまたぐ個体（§3）… 期間ごとのデータが独立か")
