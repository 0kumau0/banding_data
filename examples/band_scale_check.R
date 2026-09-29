# examples/band_scale_check.R ------------------------------------------------
#
# **足環データの規模を数える。Step 4 の前提。**
#
#   Rscript examples/band_scale_check.R
#   Rscript examples/band_scale_check.R --rds <パス> --out results/band_scale_check.csv
#
# ---------------------------------------------------------------------------
# なぜこの3つを数えるのか
#
#   ① 個体数 nind        … loglf のコストは nind の約1.8乗（results/pcap_bench.csv）
#   ② 複数回 / 単回の比   … sampling_rate で買える速度の上限を決める。
#                          この SGD は単回捕獲個体だけを間引くので、
#                          実効 nind = n_multi + rate * n_single
#   ③ 1個体あたり検出数   … **多峰性の危険域に入るかどうか**。
#                          2026-09-24 の研究（231データセット）では
#                          1.73 と 1.29 で失敗0件、1.11 で 7.8% が
#                          単一初期値では劣った峰に落ちた。
#                          **1.3 を超えていれば多点出発は要らない見込み。**
#                          reports/20260924_multistart_report.md
#
# ncell（メッシュ数）はこの rds には入っていない。make_mesh_20251127.R の
# 出力を見る必要があるので、--mesh <gpkg/shp> を渡したときだけ数える。
#
# ---------------------------------------------------------------------------
# 出力について
#
# **集計値しか出さない。個体や捕獲記録そのものは表示も保存もしない。**
# 生データを扱うスクリプトなので、出力を人に渡しても差し支えない形にしてある。
# results/band_scale_check.csv も種ごとの件数だけ。
#
# ⚠ このスクリプトは**生の足環データを読む**。編集機の Claude は
#    実行しないこと（活動範囲の外）。ユーザが自分で走らせる前提。
# ---------------------------------------------------------------------------

## --- 引数 -------------------------------------------------------------------
.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.bad <- setdiff(grep("^--", .args, value = TRUE),
                c("--rds", "--out", "--mesh", "--min-ind"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

OUTFILE <- .opt("--out", "results/band_scale_check.csv")
MESHFILE <- .opt("--mesh", "")
MIN_IND <- as.integer(.opt("--min-ind", "30"))   # この個体数未満の種は表の末尾にまとめる

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルート（banding_data）で実行してください。\n",
       "作業ディレクトリ: ", getwd())
source("config.R", encoding = "UTF-8")

RDS <- .opt("--rds", if (exists("BAND_RDS")) BAND_RDS else "")
if (!nzchar(RDS)) stop("BAND_RDS が config.R に定義されていません。--rds で指定してください。")
if (!file.exists(RDS))
  stop("見つかりません: ", RDS, "\n",
       "config.R の BAND_RDS を手元のファイルに合わせてください",
       "（20251205 / 20251217 / kanto / 素 の4種類がある）。")

cat("== 足環データの規模チェック ==\n")
cat("  読み込み: ", RDS, "\n", sep = "")
d <- readRDS(RDS)

## --- 1. まず構造を報告する --------------------------------------------------
##
## **中身の形を仮定しないこと。** 想定と違っても、何が入っていたかは分かるようにする。
cat("\n-- 読み込んだものの構造 --\n")
cat("  class  : ", paste(class(d), collapse = ", "), "\n", sep = "")
cat("  length : ", length(d), "\n", sep = "")
if (!is.null(names(d)))
  cat("  names  : ", paste(utils::head(names(d), 10), collapse = ", "),
      if (length(d) > 10) sprintf(" ...（全 %d 件）", length(d)) else "", "\n", sep = "")

if (is.data.frame(d)) {
  cat("  → data.frame なので、全体を1つのまとまりとして扱う\n")
  d <- list(all = d)
}
if (!is.list(d)) stop("リストでも data.frame でもありません。手作業で確認してください。")

## 中身の形を仮定しない。**入れ子のどこに data.frame があっても拾う。**
## 2026-09-28: 1要素目が236種の種名ベクトルで、データはもっと深い階層にあった。
## 「1要素目が data.frame」という仮定で書いたら止まったので、探索に変えた。
## data.frame と**行列（疎行列を含む）**の両方を拾う。
## 足環データは ADCR 用に集計済みで、個体は検出行列の列として入っている
## （クマの detectmat_231225.csv と同じ形）。捕獲記録の表ではない。
is_mat <- function(x) is.matrix(x) || inherits(x, "Matrix")
find_objs <- function(x, path = "", depth = 0, max_depth = 4) {
  if (is.data.frame(x)) return(list(list(path = path, obj = x, kind = "df")))
  if (is_mat(x))        return(list(list(path = path, obj = x, kind = "mat")))
  if (!is.list(x) || depth >= max_depth) return(list())
  nms <- names(x); if (is.null(nms)) nms <- rep("", length(x))
  out <- list()
  for (i in seq_along(x)) {
    lbl <- if (nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
    out <- c(out, find_objs(x[[i]], paste0(path, "$", lbl), depth + 1L, max_depth))
  }
  out
}
## 疎行列でも動く列和
csum <- function(m) if (inherits(m, "Matrix")) Matrix::colSums(m) else colSums(m)

## --- 最上位の要素をすべて要約する（何が入っているかを見せる）----------------
cat("\n-- 最上位の要素 --\n")
nms <- names(d); if (is.null(nms)) nms <- rep("", length(d))
for (i in seq_len(min(length(d), 30L))) {
  x <- d[[i]]
  lbl <- if (nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
  desc <- if (is.data.frame(x)) sprintf("data.frame %d行 x %d列", nrow(x), ncol(x))
          else if (is.list(x))  sprintf("list（長さ %d）", length(x))
          else                  sprintf("%s（長さ %d）", paste(class(x), collapse = "/"), length(x))
  cat(sprintf("  %-28s %s\n", lbl, desc))
  if (!is.data.frame(x) && !is.list(x) && length(x))
    cat("      先頭: ", paste(utils::head(as.character(x), 5), collapse = ", "), " ...\n", sep = "")
}
if (length(d) > 30L) cat(sprintf("  …（全 %d 要素）\n", length(d)))

## --- 中身を探す --------------------------------------------------------------
## detect_list に名前が無く splist がある場合は、**先頭から順に対応する**と見なして
## 種名を付ける（2026-09-28 の実データがこの形。splist 236種のうち先頭30種にデータがある）。
sp_names <- NULL
for (nm in c("splist", "SPLIST", "species")) if (!is.null(d[[nm]])) sp_names <- d[[nm]]
for (nm in c("detect_list", "detect", "detectmat")) {
  if (!is.null(d[[nm]]) && is.list(d[[nm]]) && is.null(names(d[[nm]])) &&
      !is.null(sp_names) && length(sp_names) >= length(d[[nm]])) {
    names(d[[nm]]) <- sp_names[seq_along(d[[nm]])]
    cat("\n  ", nm, " に名前が無いので splist の先頭 ", length(d[[nm]]),
        " 種を順に割り当てた\n", sep = "")
    cat("    ⚠ 並び順が splist と一致している前提。違っていたら種名がずれる\n")
  }
}

found <- find_objs(d)
mats <- Filter(function(f) f$kind == "mat", found)
dfs  <- Filter(function(f) f$kind == "df",  found)

cat(sprintf("\n-- 見つかったもの: 行列 %d 件 / data.frame %d 件 --\n",
            length(mats), length(dfs)))
for (f in utils::head(c(mats, dfs), 12L))
  cat(sprintf("  %-30s %s %6d行 x %5d列 %s\n", f$path,
              if (f$kind == "mat") "行列      " else "data.frame",
              nrow(f$obj), ncol(f$obj),
              if (f$kind == "df") paste0("列: ", paste(utils::head(names(f$obj), 8),
                                                       collapse = ", ")) else
                paste0("(", class(f$obj)[1], ")")))
if (length(found) > 12L) cat(sprintf("  …（全 %d 件）\n", length(found)))

## --- 努力の表（effort）------------------------------------------------------
## クマと同じ形なら、行 = 検出努力 / 列 = 個体。effort の行数が検出行列の行数と一致する。
eff <- NULL
for (f in dfs)
  if (all(c("effort") %in% tolower(names(f$obj))) ||
      length(intersect(c("meshcode", "effortID"), names(f$obj))) >= 1L) { eff <- f; break }

n_effort <- NA_integer_; n_mesh <- NA_integer_; yr <- NULL
if (!is.null(eff)) {
  e <- eff$obj
  n_effort <- nrow(e)
  mc <- intersect(c("meshcode", "MESHCODE", "mesh"), names(e))[1]
  if (!is.na(mc)) n_mesh <- length(unique(e[[mc]]))
  yc <- intersect(c("YEAR", "year"), names(e))[1]
  if (!is.na(yc)) yr <- range(e[[yc]], na.rm = TRUE)
  cat("\n-- 検出努力（", sub("^\\$", "", eff$path), "）--\n", sep = "")
  cat(sprintf("  行数（検出努力の数）: %d\n", n_effort))
  if (!is.na(n_mesh)) cat(sprintf("  努力が置かれたメッシュ: %d\n", n_mesh))
  if (!is.null(yr))   cat(sprintf("  年の範囲: %s 〜 %s\n", yr[1], yr[2]))
}

## --- 努力行 → セル、およびセルの中心座標 ------------------------------------
## **effort$meshcode は mesh の行番号**（reports/20260928_band_scale.md §4）。
## これが「どの努力がどのセルか」で、移動の判定に要る。
EFF_CELL <- NULL
if (!is.null(eff) && !is.na(mc))
  EFF_CELL <- suppressWarnings(as.integer(as.character(eff$obj[[mc]])))
if (!is.null(EFF_CELL) && anyNA(EFF_CELL)) {
  cat("  ⚠ meshcode が整数にならない行がある。移動の集計はできない\n")
  EFF_CELL <- NULL
}

## メッシュの中心座標（km）。--mesh を渡したときだけ。移動距離の算出に使う。
MESH_XY <- NULL; meshsf <- NULL
if (nzchar(MESHFILE) && file.exists(MESHFILE) && requireNamespace("sf", quietly = TRUE)) {
  meshsf <- sf::st_read(MESHFILE, quiet = TRUE)
  xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(meshsf)))
  MESH_XY <- xy[, 1:2, drop = FALSE] / 1000      # m → km
  cat(sprintf("  メッシュ %s: %d セル（移動距離の算出に使う）\n",
              basename(MESHFILE), nrow(MESH_XY)))
  if (!is.null(EFF_CELL) && max(EFF_CELL) > nrow(MESH_XY)) {
    cat("  ⚠ **行番号がメッシュの行数を超えている。世代が違う。**移動距離は出さない\n")
    MESH_XY <- NULL
  }
}

## --- 集計の対象を決める ------------------------------------------------------
## 行列があれば ADCR 形式（集計済み）。無ければ捕獲記録の表を探す。
MODE <- if (length(mats)) "detect" else "records"
cat("\n  集計の形式: ",
    if (MODE == "detect") "ADCR形式（検出行列。列 = 個体）"
    else "捕獲記録の表（GUID + RING で個体を識別）", "\n", sep = "")

if (MODE == "records") {
  ID_CANDIDATES <- c("GUID", "RING")
  has_id <- sapply(dfs, function(f) length(intersect(ID_CANDIDATES, names(f$obj))) > 0)
  if (!any(has_id))
    stop("検出行列も、GUID / RING を持つ表も見つかりません。\n",
         "上の一覧を見て、どこに個体の情報があるか教えてください。")
  dfs <- dfs[has_id]
  el <- dfs[[1]]$obj
  ID_COLS <- intersect(ID_CANDIDATES, names(el))
  PLACE_COL <- intersect(c("PLACE", "PLACECODE", "place"), names(el))[1]
  SP_COL    <- intersect(c("SPNAMK", "SPNAME", "species"), names(el))[1]
  cat("  個体の識別に使う列: ", paste(ID_COLS, collapse = " + "), "\n", sep = "")
}

## --- 3. まとまりごとに数える ------------------------------------------------
one <- function(x, label) {
  if (!is.data.frame(x) || !nrow(x)) return(NULL)
  ok <- Reduce(`&`, lapply(ID_COLS, function(c) !is.na(x[[c]])))
  x <- x[ok, , drop = FALSE]
  if (!nrow(x)) return(NULL)

  id  <- do.call(paste, c(lapply(ID_COLS, function(c) x[[c]]), sep = "_"))
  cnt <- table(id)                      # 個体ごとの捕獲回数
  n_ind  <- length(cnt)
  n_rec  <- nrow(x)
  n_mult <- sum(cnt >  1)
  n_sing <- sum(cnt == 1)

  ## 実効 nind = n_multi + rate * n_single（単回だけ間引く設計）
  eff <- function(r) n_mult + r * n_sing
  ## 速度比 = (実効 nind の比)^1.78（results/pcap_bench.csv の指数）
  spd <- function(r) (n_ind / eff(r))^1.78

  data.frame(
    group = label,
    n_record = n_rec,
    n_ind = n_ind,
    n_multi = n_mult,
    n_single = n_sing,
    det_per_ind = n_rec / n_ind,
    multi_frac = n_mult / n_ind,
    max_cap = max(cnt),
    n_place = if (!is.na(PLACE_COL)) length(unique(x[[PLACE_COL]])) else NA_integer_,
    eff_nind_r10 = eff(0.10),
    speedup_r10 = spd(0.10),
    eff_nind_r20 = eff(0.20),
    speedup_r20 = spd(0.20),
    stringsAsFactors = FALSE)
}

## まとまりの切り方を、見つかった形に合わせる。
##   data.frame が複数    … 1つずつを1行にする（種ごとのリストなど）
##   data.frame が1つだけ … 種の列があれば種で割る。無ければ全体で1行
## --- 検出行列から数える（ADCR 形式）-----------------------------------------
## **行 = 検出努力 / 列 = 個体**（secrad.r の self$nind = ncol(detect)）。
## 向きを間違えると個体数が検出器数に化けるので、effort の行数と突き合わせて確かめる。
rsum <- function(m) if (inherits(m, "Matrix")) Matrix::rowSums(m) else rowSums(m)

## --- ★ 移動が観測された個体の数（2026-09-29 追加）--------------------------
##
## **再捕獲の回数は、連結性の情報量の代理にならない。**
## 同じメッシュで何度捕まっても、移動は1つも観測されない。
## ADCR が連結性を推定するのに使うのは**セル間の移動**。
##
## ヤマガラは1個体あたり検出数が30種で最高（1.34）だが、
## **複数回捕獲のほぼ全てが同じメッシュ内**だとユーザから指摘（2026-09-29）。
## それなら移動の情報はほぼゼロで、**連結性は推定できない。**
##
## ここでは「**2つ以上の異なるメッシュで捕獲された個体**」を数える。
## 検出行列の列ごとに、検出のあった努力行 → そのセル、の異なり数を見る。
movement_stats <- function(m, eff_cell, xy = NULL) {
  if (is.null(eff_cell)) return(list(n_moved = NA_integer_, d_med = NA_real_,
                                     d_max = NA_real_))
  if (!inherits(m, "dgCMatrix")) m <- methods::as(m, "dgCMatrix")
  p <- m@p
  if (length(m@i) == 0L) return(list(n_moved = 0L, d_med = NA_real_, d_max = NA_real_))

  ## 非ゼロ要素を (個体, セル) の組にして、組の異なり数を数える
  jj <- rep.int(seq_len(ncol(m)), diff(p))      # 個体（列）
  cc <- eff_cell[m@i + 1L]                      # セル（行 → 努力 → セル）
  ok <- !is.na(cc)
  jj <- jj[ok]; cc <- cc[ok]
  o  <- order(jj, cc)
  jj <- jj[o]; cc <- cc[o]
  n  <- length(jj)
  newpair <- if (n <= 1L) TRUE else
    c(TRUE, (jj[-1] != jj[-n]) | (cc[-1] != cc[-n]))
  n_cells <- tabulate(jj[newpair], nbins = ncol(m))
  moved <- which(n_cells >= 2L)

  ## 移動した個体ごとの「最も離れた2セル間の距離」
  d_med <- d_max <- NA_real_
  if (length(moved) && !is.null(xy)) {
    uj <- jj[newpair]; uc <- cc[newpair]
    sel <- uj %in% moved
    uj <- uj[sel]; uc <- uc[sel]
    sp <- split(uc, uj)
    dd <- vapply(sp, function(cells) {
      if (length(cells) < 2L) return(0)
      pts <- xy[cells, , drop = FALSE]
      max(stats::dist(pts))
    }, numeric(1))
    d_med <- stats::median(dd); d_max <- max(dd)
  }
  list(n_moved = length(moved), d_med = d_med, d_max = d_max)
}

one_detect <- function(m, label) {
  if (is.null(m) || !nrow(m) || !ncol(m)) return(NULL)
  cnt    <- csum(m)                      # 個体ごとの検出回数
  n_ind  <- ncol(m)
  n_rec  <- sum(cnt)
  n_mult <- sum(cnt >  1)
  n_sing <- sum(cnt == 1)
  eff_ <- function(r) n_mult + r * n_sing
  mv <- movement_stats(m, EFF_CELL, MESH_XY)
  data.frame(
    group = label, n_record = n_rec, n_ind = n_ind,
    n_multi = n_mult, n_single = n_sing,
    det_per_ind = n_rec / n_ind, multi_frac = n_mult / n_ind,
    ## ★ 連結性の情報量はここ。複数回捕獲されても同じセルなら移動は観測されない
    n_moved = mv$n_moved,                     # 2セル以上で捕獲された個体
    moved_frac = mv$n_moved / max(1L, n_mult), # 複数回個体のうち移動した割合
    move_km_med = mv$d_med, move_km_max = mv$d_max,
    max_cap = max(cnt),
    n_place = sum(rsum(m) > 0),          # 検出があった努力の数
    eff_nind_r10 = eff_(0.10), speedup_r10 = (n_ind / eff_(0.10))^1.78,
    eff_nind_r20 = eff_(0.20), speedup_r20 = (n_ind / eff_(0.20))^1.78,
    stringsAsFactors = FALSE)
}

if (MODE == "detect") {
  ## 向きの確認。effort の行数と検出行列の行数が一致するはず
  if (!is.na(n_effort)) {
    nr <- sapply(mats, function(f) nrow(f$obj))
    if (all(nr == n_effort)) {
      cat("  ✓ 検出行列の行数が検出努力の数（", n_effort, "）と一致。",
          "行 = 努力 / 列 = 個体 で読む\n", sep = "")
    } else {
      cat("  ⚠ 検出行列の行数（", paste(utils::head(unique(nr), 5), collapse = ", "),
          "）が検出努力の数（", n_effort, "）と一致しない。\n", sep = "")
      cat("    向きが逆の可能性がある。**個体数が検出器数に化けていないか確認すること。**\n")
    }
  }
  rows <- lapply(mats, function(f) one_detect(f$obj, sub("^\\$detect_list\\$|^\\$", "", f$path)))
} else if (length(dfs) > 1L) {
  cat("  → data.frame ごとに集計する\n")
  rows <- lapply(dfs, function(f) one(f$obj, sub("^\\$", "", f$path)))
} else if (!is.na(SP_COL)) {
  cat("  → 1つの data.frame を列 ", SP_COL, " で分けて集計する\n", sep = "")
  sp <- as.character(el[[SP_COL]])
  rows <- lapply(unique(sp[!is.na(sp)]),
                 function(s) one(el[sp == s & !is.na(sp), , drop = FALSE], s))
} else {
  cat("  → 種を表す列が無いので、全体を1つとして集計する\n")
  rows <- list(one(el, "all"))
}
rows <- Filter(Negate(is.null), rows)
if (!length(rows)) stop("集計できるまとまりがありませんでした。")
tab <- do.call(rbind, rows)
tab <- tab[order(-tab$n_ind), ]

## --- 4. 表示 -----------------------------------------------------------------
cat("\n-- まとまりごとの規模（個体数の多い順）--\n")
show <- tab[tab$n_ind >= MIN_IND, ]
if (nrow(show)) {
  cols <- c("group", "n_record", "n_ind", "n_multi", "det_per_ind",
            "n_moved", "moved_frac")
  if (any(is.finite(tab$move_km_med))) cols <- c(cols, "move_km_med", "move_km_max")
  cols <- c(cols, "max_cap", "n_place")
  print(format(show[, cols], digits = 3), row.names = FALSE)
  cat("   n_moved   = **2つ以上の異なるメッシュで捕獲された個体**（移動が観測された）\n")
  cat("   moved_frac = n_moved / n_multi（複数回捕獲のうち移動した割合）\n")
  cat("   **連結性の情報を運ぶのは n_moved であって n_multi ではない。**\n")
}

## --- ★ 連結性の情報量で並べ直す ---------------------------------------------
if (any(is.finite(tab$n_moved))) {
  cat("\n-- ★ 移動が観測された個体数の多い順（連結性の情報量）--\n")
  mv <- tab[order(-tab$n_moved), ]
  cols <- c("group", "n_ind", "n_multi", "n_moved", "moved_frac", "det_per_ind")
  if (any(is.finite(tab$move_km_med))) cols <- c(cols, "move_km_med", "move_km_max")
  print(format(utils::head(mv, 12), digits = 3)[, cols], row.names = FALSE)
  cat("\n   ＊ 1個体あたり検出数（det_per_ind）の順位と一致しないことに注意。\n")
  cat("     **同じメッシュで何度捕まっても移動は観測されない。**\n")
}
small <- tab[tab$n_ind < MIN_IND, ]
if (nrow(small))
  cat(sprintf("\n（個体数 %d 未満の %d 件は省略。合計 個体 %d / 記録 %d）\n",
              MIN_IND, nrow(small), sum(small$n_ind), sum(small$n_record)))

## --- 5. Step 4 の判断に直結する部分 -----------------------------------------
tot_rec <- sum(tab$n_record); tot_ind <- sum(tab$n_ind)
tot_m <- sum(tab$n_multi);    tot_s <- sum(tab$n_single)

## **ADCR の解析は種ごとに行う。判定も種ごと。**
## 合計は「どれだけのデータがあるか」の参考にしかならない。
cat("\n== 参考: 全種の合計（実際の解析は種ごとに行うので、判定には使わない）==\n")
cat(sprintf("  記録 %d / 個体 %d / 複数回 %d / 単回 %d\n", tot_rec, tot_ind, tot_m, tot_s))

## --- ① 多峰性の危険域か。種ごとに判定する ----------------------------------
zone <- function(x) ifelse(x >= 1.3, "安全", ifelse(x >= 1.15, "境界", "危険"))
tab$zone <- zone(tab$det_per_ind)

cat("\n-- ① 多峰性の危険域か（種ごと。reports/20260924_multistart_report.md）--\n")
cat("   判定: 1.30以上=安全（231データセットで失敗0件） / 1.15-1.30=境界 / 1.15未満=危険\n\n")
zt <- table(factor(tab$zone, levels = c("安全", "境界", "危険")))
for (z in names(zt))
  cat(sprintf("  %-4s : %2d 種  （個体数の合計 %d）\n", z, zt[[z]],
              sum(tab$n_ind[tab$zone == z])))

if (zt[["危険"]] > 0) {
  cat("\n  ⚠ 危険域の種:\n")
  dz <- tab[tab$zone == "危険", ]
  dz <- dz[order(-dz$n_ind), ]
  print(format(dz[, c("group", "n_ind", "n_multi", "det_per_ind")], digits = 3),
        row.names = FALSE)
  cat("\n  → 1.11 では 7.8% が単一初期値で劣った峰に落ちた。外したときの遅れは\n")
  cat("     対数尤度で中央値 9.1。**この種では多点出発が必須。**\n")
  cat("     メッシュ解像度や検出器の定義で 1.3 を超えられないかを先に検討する価値がある。\n")
}

cat(sprintf("\n  1個体あたり検出数の分布: 最小 %.2f / 中央 %.2f / 最大 %.2f\n",
            min(tab$det_per_ind), median(tab$det_per_ind), max(tab$det_per_ind)))

## --- ② sampling_rate で買える速度。個体数の多い種ほど効く -------------------
cat("\n-- ② sampling_rate で買える速度（種ごと。単回だけ間引く設計）--\n")
big <- tab[order(-tab$n_ind), ][seq_len(min(5L, nrow(tab))), ]
cat("   個体数の多い5種:\n")
print(format(big[, c("group", "n_ind", "n_multi", "n_single",
                     "eff_nind_r20", "speedup_r20", "eff_nind_r10", "speedup_r10")],
             digits = 3), row.names = FALSE)
cat("   ＊ eff_nind = 1反復で使う実効個体数、speedup = (n_ind/eff_nind)^1.78\n")
cat("     指数 1.78 は results/pcap_bench.csv の実測。\n")
cat("     **複数回個体は毎回全部使うので、単回の割合が高いほど間引きが効く。**\n")

## --- 規模の警告 --------------------------------------------------------------
## クマは109個体。桁が変われば、測っていない領域に入る。
mx <- max(tab$n_ind)
if (mx > 5000) {
  cat(sprintf("\n  ⚠ 最大の種で %d 個体（クマの %.0f 倍）。\n", mx, mx / 109))
  cat("    loglf のコストは nind の約1.8乗（results/pcap_bench.csv）。\n")
  cat("    **これは本家 pcap_poisson の性能バグに由来する**（ind_cov.minCoeff() が\n")
  cat("    最内ループの中にあり O(nind^2) になる。reports/pcap_fix_decision.md）。\n")
  cat("    修正すれば指数は 1.10 に落ちる。nind 19,200 での実測で 11.8倍速。\n")
  cat(sprintf("    この規模なら **約%.0f倍** になる見込み。\n",
              11.8 * (mx / 19200)^0.68))
  cat("    2026-09-14 に『論文化のとき記述が煩雑になる』として修正を見送ったが、\n")
  cat("    **この規模では実行可能性そのものに関わる。判断を見直す価値がある。**\n")
}

cat("\n-- ③ ncell --\n")
if (!is.na(n_mesh)) {
  cat(sprintf("  検出努力が置かれたメッシュ: **%d**\n", n_mesh))
  cat("  ＊ これは努力のあるメッシュの数。**ADCR の ncell は解析範囲全体のメッシュ数**で、\n")
  cat("    ふつうこれより大きい（クマは努力227に対し ncell 8497）。\n")
  cat(sprintf("    クマの比（8497/227 = 37倍）をあてはめると ncell は %d 前後になりうる。\n",
              round(n_mesh * 8497 / 227)))
}
if (nzchar(MESHFILE) && file.exists(MESHFILE)) {
  if (requireNamespace("sf", quietly = TRUE)) {
    m <- sf::st_read(MESHFILE, quiet = TRUE)
    cat(sprintf("  %s のセル数 = **%d**\n", basename(MESHFILE), nrow(m)))
    cat("  ＊ loglf のコストは ncell が支配する（約 O(ncell^2.5)）。\n")
    cat(sprintf("    クマは 8497。比 %.1f 倍 → コストの目安 %.0f 倍\n",
                nrow(m) / 8497, (nrow(m) / 8497)^2.5))
  } else cat("  sf パッケージが無いので読めません\n")
} else {
  cat("  解析範囲全体の ncell は、この rds には入っていない。\n")
  cat("  make_mesh_20251127.R の出力を --mesh <ファイル> で渡すと数える。\n")
}

## --- 6. 保存 -----------------------------------------------------------------
## **集計値のみ。** 個体や記録そのものは入らないので、人に渡してよい。
dir.create(dirname(OUTFILE), showWarnings = FALSE, recursive = TRUE)
write.csv(tab, OUTFILE, row.names = FALSE)
cat("\n保存: ", OUTFILE, "（集計値のみ。生データは含まない）\n", sep = "")
