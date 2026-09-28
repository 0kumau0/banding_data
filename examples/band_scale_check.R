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
find_dfs <- function(x, path = "", depth = 0, max_depth = 4) {
  if (is.data.frame(x)) return(list(list(path = path, df = x)))
  if (!is.list(x) || depth >= max_depth) return(list())
  nms <- names(x); if (is.null(nms)) nms <- rep("", length(x))
  out <- list()
  for (i in seq_along(x)) {
    lbl <- if (nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
    out <- c(out, find_dfs(x[[i]], paste0(path, "$", lbl), depth + 1L, max_depth))
  }
  out
}

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

## --- data.frame を探す -------------------------------------------------------
found <- find_dfs(d)
cat(sprintf("\n-- 見つかった data.frame: %d 件 --\n", length(found)))
if (!length(found))
  stop("data.frame が1つも見つかりません（4階層まで探索）。\n",
       "上の一覧を見て、どこに捕獲記録があるか教えてください。")

ID_CANDIDATES <- c("GUID", "RING")
has_id <- sapply(found, function(f) length(intersect(ID_CANDIDATES, names(f$df))) > 0)

for (i in seq_len(min(length(found), 12L))) {
  f <- found[[i]]
  cat(sprintf("  %-34s %6d行 x %3d列  %s\n", f$path, nrow(f$df), ncol(f$df),
              if (has_id[i]) "← 個体IDあり" else ""))
  if (i <= 3L || has_id[i])
    cat("      列: ", paste(utils::head(names(f$df), 15), collapse = ", "),
        if (ncol(f$df) > 15) " ..." else "", "\n", sep = "")
}
if (length(found) > 12L) cat(sprintf("  …（全 %d 件）\n", length(found)))

if (!any(has_id)) {
  stop("GUID / RING を持つ data.frame がありません。\n",
       "上の列名を見て、個体を識別できる列を教えてください（ID_CANDIDATES を直します）。")
}

found <- found[has_id]
el <- found[[1]]$df
ID_COLS <- intersect(ID_CANDIDATES, names(el))
cat("\n  個体の識別に使う列: ", paste(ID_COLS, collapse = " + "), "\n", sep = "")
cat("  集計する data.frame: ", length(found), " 件\n", sep = "")

PLACE_COL <- intersect(c("PLACE", "PLACECODE", "place"), names(el))[1]
DATE_COL  <- intersect(c("DATE", "YEAR", "date", "year"), names(el))[1]
SP_COL    <- intersect(c("SPNAMK", "SPNAME", "species"), names(el))[1]

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
if (length(found) > 1L) {
  cat("  → data.frame ごとに集計する\n")
  rows <- lapply(found, function(f) one(f$df, sub("^\\$", "", f$path)))
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
  print(format(show[, c("group", "n_record", "n_ind", "n_multi", "n_single",
                        "det_per_ind", "max_cap", "n_place")],
               digits = 3), row.names = FALSE)
}
small <- tab[tab$n_ind < MIN_IND, ]
if (nrow(small))
  cat(sprintf("\n（個体数 %d 未満の %d 件は省略。合計 個体 %d / 記録 %d）\n",
              MIN_IND, nrow(small), sum(small$n_ind), sum(small$n_record)))

## --- 5. Step 4 の判断に直結する部分 -----------------------------------------
tot_rec <- sum(tab$n_record); tot_ind <- sum(tab$n_ind)
tot_m <- sum(tab$n_multi);    tot_s <- sum(tab$n_single)

cat("\n== 全体 ==\n")
cat(sprintf("  記録 %d / 個体 %d / 複数回 %d / 単回 %d\n", tot_rec, tot_ind, tot_m, tot_s))
cat(sprintf("  複数回 : 単回 = 1 : %.1f\n", tot_s / max(1, tot_m)))
cat(sprintf("  **1個体あたり検出数 = %.3f**\n", tot_rec / tot_ind))

cat("\n-- ① 多峰性の危険域か（reports/20260924_multistart_report.md）--\n")
dpi <- tot_rec / tot_ind
cat(sprintf("  1個体あたり %.3f 検出\n", dpi))
if (dpi >= 1.3) {
  cat("  → **安全域。** 1.29 と 1.73 では231データセット中0件の失敗。\n")
  cat("     単一の初期値からの最尤推定でよい見込み。\n")
} else if (dpi >= 1.15) {
  cat("  → **境界。** 失敗が観測されたのは 1.13 以下。余裕は小さい。\n")
  cat("     多点出発（3点以上）を予算に入れておくこと。\n")
} else {
  cat("  → **⚠ 危険域。** 1.11 では 7.8% が単一初期値で劣った峰に落ちた。\n")
  cat("     外したときの遅れは対数尤度で中央値 9.1。**多点出発は必須。**\n")
  cat("     メッシュ解像度や検出器の定義で 1.3 を超えられないかを先に検討する価値がある。\n")
}

cat("\n-- ② sampling_rate で買える速度（単回だけ間引く設計）--\n")
cat(sprintf("  rate 1.00 : 実効 %6.0f 個体（1.0倍）\n", tot_ind))
for (r in c(0.5, 0.2, 0.1)) {
  e <- tot_m + r * tot_s
  cat(sprintf("  rate %.2f : 実効 %6.0f 個体（**%.1f倍**）\n", r, e, (tot_ind / e)^1.78))
}
cat("  ＊ 指数 1.78 は results/pcap_bench.csv の実測。複数回個体は毎回全部使うので、\n")
cat("    単回の割合が高いほど間引きが効く。\n")

cat("\n-- ③ ncell --\n")
if (nzchar(MESHFILE) && file.exists(MESHFILE)) {
  if (requireNamespace("sf", quietly = TRUE)) {
    m <- sf::st_read(MESHFILE, quiet = TRUE)
    cat(sprintf("  %s のセル数 = **%d**\n", basename(MESHFILE), nrow(m)))
    cat("  ＊ loglf のコストは ncell が支配する（約 O(ncell^2.5)）。\n")
    cat(sprintf("    クマは 8497。比 %.1f 倍 → コストの目安 %.0f 倍\n",
                nrow(m) / 8497, (nrow(m) / 8497)^2.5))
  } else cat("  sf パッケージが無いので読めません\n")
} else {
  cat("  このスクリプトでは数えない（rds に入っていない）。\n")
  cat("  make_mesh_20251127.R の出力を --mesh <ファイル> で渡すと数える。\n")
}

## --- 6. 保存 -----------------------------------------------------------------
## **集計値のみ。** 個体や記録そのものは入らないので、人に渡してよい。
dir.create(dirname(OUTFILE), showWarnings = FALSE, recursive = TRUE)
write.csv(tab, OUTFILE, row.names = FALSE)
cat("\n保存: ", OUTFILE, "（集計値のみ。生データは含まない）\n", sep = "")
