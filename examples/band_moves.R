# examples/band_moves.R ------------------------------------------------------
#
# **「移動が観測された個体」を1件ずつ開けて見る。**
#
#   Rscript examples/band_moves.R                       # シジュウカラの全件
#   Rscript examples/band_moves.R --species ヤマガラ
#   Rscript examples/band_moves.R --all                 # 全30種を横断して長距離移動を探す
#   Rscript examples/band_moves.R --all --min-km 500
#
# 生データを**読むだけ**。書き出すのは --out の CSV 2本だけ。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-29）
#
# `results/band_scale_check.csv` で、**全30種の `move_km_max` が揃って
# 3020.34766210779 km** になっている。留鳥のシジュウカラもメジロもヤマガラも、
# 渡り鳥のアオジも、全部同じ値。**偶然ではありえない。**
#
# しかも `n_moved` が 1 の種（センダイムシクイ・エゾムシクイ・オオルリ・
# シマセンニュウ）は `move_km_med` も 3020.3 なので、
# **その1件の「移動」がまさにこの 3020 km の跳び**にあたる。
#
# ## 仮説
#
# 効率の表（`d$effort`、4,229行）は**全種で共有**されている。
# したがって、**1行でも `meshcode` が誤った場所を指していれば、
# そこで捕まった個体は種を問わず巨大な移動を示す。**
#
# それが当たっていれば、`n_moved` の値そのものが水増しされている。
# シジュウカラの 18件は 17件以下、関東の 5件はもっと少ないかもしれない。
#
# 連結性が推定できるかは移動の件数で決まるので、**まずこれを確かめる。**
#
# ## 出力
#
#   (a) 種ごとの移動の明細  — どの努力行 → どのセル → JIS コード → 年 → 座標
#   (b) 全種横断の集計      — 長距離移動に関与したセルと努力行の頻度
#
# (b) で**特定のセルが種をまたいで繰り返し現れる**なら、仮説は裏づけられる。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--species", "--mesh", "--rds", "--min-km", "--max-print", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--all"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--all"), collapse = " "))

ALL       <- "--all" %in% .args
SPECIES   <- .opt("--species", "シジュウカラ")
MIN_KM    <- as.numeric(.opt("--min-km", "500"))
MAX_PRINT <- as.integer(.opt("--max-print", "40"))
OUT       <- .opt("--out", "results/band_moves")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
MESH <- .opt("--mesh", BAND_MESH)
RDS  <- .opt("--rds",  BAND_RDS)
for (p in c(MESH, RDS)) if (!file.exists(p)) stop("見つかりません: ", p)

suppressMessages({ library(sf); library(Matrix) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 78))

# ===========================================================================
# §1 入力
# ===========================================================================
say("=========================================================================")
say("  移動の明細を開ける")
say("=========================================================================")

mesh <- st_read(MESH, quiet = TRUE)
xy   <- st_coordinates(st_centroid(st_geometry(mesh)))[, 1:2, drop = FALSE] / 1000  # km
d    <- readRDS(RDS)
effort <- as.data.frame(d$effort)

## ★ effort$meshcode は JIS コードではなく、このメッシュの「行番号」
eff_cell <- as.integer(as.character(effort$meshcode))
stopifnot(!anyNA(eff_cell), min(eff_cell) >= 1L, max(eff_cell) <= nrow(mesh))

## メッシュ側の JIS コード列（あれば、セルの正体を人が読める形で出す）
jis <- if ("meshcode" %in% names(mesh)) as.character(mesh$meshcode) else rep(NA_character_, nrow(mesh))

say("  メッシュ  ", MESH, "  ", format(nrow(mesh), big.mark = ","), " セル")
say("  データ    ", RDS)
say("  effort    ", format(nrow(effort), big.mark = ","), " 行（全種で共有）")
say("  座標系    ", st_crs(mesh)$input)
say("  メッシュの広がり: x ", round(diff(range(xy[, 1]))), " km / y ",
    round(diff(range(xy[, 2]))), " km / 対角 ",
    round(sqrt(diff(range(xy[, 1]))^2 + diff(range(xy[, 2]))^2)), " km")

# ===========================================================================
# §2 部品
# ===========================================================================

## 疎行列の格納要素を (個体, 努力行, 回数) の表にほどく。
## **行 = 努力 / 列 = 個体**（secrad.r の向き）
##
## ★★ **値が 0 の要素を必ず落とすこと**（2026-09-29 にここで間違えた）。
##
## 疎行列は「格納されている要素」と「値が 0 でない要素」が一致するとは限らない。
## この rds の検出行列には **明示的に格納された 0**（`m@x == 0`）が入っており、
## `m@i` / `m@p` をそのまま使うと**捕獲されていない努力行を捕獲地点として数える**。
##
## 実際、シジュウカラの最後の列（個体 #33551）は捕獲 0回なのに 812 セルに
## 「出現」し、3020 km の移動として計上されていた。
## **`examples/band_scale_check.R` の `movement_stats` も同じ書き方**なので、
## `results/band_scale_check.csv` の `n_moved` と `move_km_*` にも混入している。
## 2つの実装が一致したのは、同じ見落としを共有していたから。
unpack <- function(m, drop_zero = TRUE) {
  if (!inherits(m, "dgCMatrix")) m <- methods::as(m, "dgCMatrix")
  z <- data.frame(ind = rep.int(seq_len(ncol(m)), diff(m@p)),
                  eff = m@i + 1L,
                  n   = m@x)
  if (drop_zero) z <- z[z$n > 0, , drop = FALSE]
  z
}

## 明示的な 0 がどれだけ入っているかを報告する
zero_report <- function(m, sp) {
  if (!inherits(m, "dgCMatrix")) m <- methods::as(m, "dgCMatrix")
  nz <- length(m@x); n0 <- sum(m@x == 0)
  if (!n0) { say(sprintf("  %s: 格納 %s 要素、明示的な 0 は無し", sp,
                         format(nz, big.mark = ","))); return(invisible(0L)) }
  ## 0 を含む列（個体）と、全部 0 の列
  ind0 <- rep.int(seq_len(ncol(m)), diff(m@p))[m@x == 0]
  allz <- sum(tabulate(ind0, nbins = ncol(m)) == diff(m@p) & diff(m@p) > 0)
  say(sprintf("  ⚠ %s: 格納 %s 要素のうち **%s が値 0**（%.1f%%）。0 を含む個体 %s / 全部 0 の個体 %s",
              sp, format(nz, big.mark = ","), format(n0, big.mark = ","),
              100 * n0 / nz, format(length(unique(ind0)), big.mark = ","),
              format(allz, big.mark = ",")))
  invisible(n0)
}

## 「2つ以上の異なるセルで捕獲された個体」を返す。
## **band_scale_check.R / band_subset.R と同じ定義**（3実装が一致することを確認済み）
moved_ids <- function(z, nind) {
  o <- order(z$ind, z$cell)
  zz <- z[o, , drop = FALSE]
  n <- nrow(zz)
  if (!n) return(integer(0))
  first <- c(TRUE, (zz$ind[-1] != zz$ind[-n]) | (zz$cell[-1] != zz$cell[-n]))
  which(tabulate(zz$ind[first], nbins = nind) >= 2L)
}

## 1個体の「最も離れた2セル」とその距離
farthest <- function(cells) {
  u <- unique(cells)
  pts <- xy[u, , drop = FALSE]
  dm <- as.matrix(dist(pts))
  w <- which(dm == max(dm), arr.ind = TRUE)[1, ]
  list(a = u[w[1]], b = u[w[2]], km = max(dm))
}

## 1種ぶんの移動明細を作る
moves_of_species <- function(sp) {
  i_sp <- which(d$splist == sp)
  if (!length(i_sp) || i_sp > length(d$detect_list)) return(NULL)
  m <- d$detect_list[[i_sp]]
  z <- unpack(m)
  z$cell <- eff_cell[z$eff]
  mv <- moved_ids(z, ncol(m))
  if (!length(mv)) return(data.frame())
  zm <- z[z$ind %in% mv, , drop = FALSE]
  sp_list <- split(zm, zm$ind)
  do.call(rbind, lapply(sp_list, function(zi) {
    f <- farthest(zi$cell)
    data.frame(species = sp, ind = zi$ind[1],
               n_cap = sum(zi$n), n_cell = length(unique(zi$cell)),
               cell_a = f$a, cell_b = f$b, km = f$km,
               jis_a = jis[f$a], jis_b = jis[f$b],
               year_min = min(effort$YEAR[zi$eff]),
               year_max = max(effort$YEAR[zi$eff]),
               stringsAsFactors = FALSE)
  }))
}

# ===========================================================================
# §3 1種の明細
# ===========================================================================
SPP <- if (ALL) d$splist[seq_len(length(d$detect_list))] else SPECIES

if (!ALL) {
  say("")
  hr(); say("  §3 ", SPECIES, " の移動 — 全件の明細"); hr()

  i_sp <- which(d$splist == SPECIES)
  if (!length(i_sp)) stop("種が見つかりません: ", SPECIES)
  m <- d$detect_list[[i_sp]]
  zero_report(m, SPECIES)

  ## **0 を落とす前と後で n_moved を並べる。**
  ## 差が出るなら results/band_scale_check.csv の値は過大
  mv_raw <- local({ zr <- unpack(m, drop_zero = FALSE)
                    zr$cell <- eff_cell[zr$eff]; moved_ids(zr, ncol(m)) })
  z <- unpack(m); z$cell <- eff_cell[z$eff]
  mv <- moved_ids(z, ncol(m))
  say("  個体 ", format(ncol(m), big.mark = ","), " / 検出 ",
      format(sum(Matrix::colSums(m)), big.mark = ","))
  say("  移動が観測された個体: 0 を含めると ", length(mv_raw),
      " / **0 を落とすと ", length(mv), "**")
  if (length(mv_raw) != length(mv))
    say("  → **results/band_scale_check.csv の n_moved は ",
        length(mv_raw), " で、", length(mv_raw) - length(mv), " 件が過大。**")

  if (length(mv)) {
    zm <- z[z$ind %in% mv, , drop = FALSE]
    ord <- sapply(split(zm, zm$ind), function(zi) farthest(zi$cell)$km)
    ids <- as.integer(names(sort(ord, decreasing = TRUE)))
    say("")
    for (id in head(ids, MAX_PRINT)) {
      zi <- z[z$ind == id, , drop = FALSE]
      f  <- farthest(zi$cell)
      say(sprintf("  個体 #%-6d 捕獲 %2d回 / 異なるセル %d / **最大 %.1f km**",
                  id, sum(zi$n), length(unique(zi$cell)), f$km))
      zi <- zi[order(effort$YEAR[zi$eff]), , drop = FALSE]
      for (r in seq_len(nrow(zi))) {
        e <- zi$eff[r]; cl <- zi$cell[r]
        say(sprintf("      努力行 %5d  セル %6d  JIS %-8s  %d年  努力 %5s  x %8.1f  y %8.1f  捕獲 %d回",
                    e, cl, ifelse(is.na(jis[cl]), "-", jis[cl]),
                    effort$YEAR[e], format(effort$effort[e]),
                    xy[cl, 1], xy[cl, 2], zi$n[r]))
      }
      say("")
    }
    if (length(ids) > MAX_PRINT)
      say("  （残り ", length(ids) - MAX_PRINT, " 件は --max-print で増やす）")
  }
}

# ===========================================================================
# §4 全種横断 — 長距離移動に関与したセルを特定する
# ===========================================================================
say("")
hr()
say("  §4 全種横断: ", MIN_KM, " km 以上の移動に関与したセル")
hr()
say("  effort の表は全種で共有なので、**1行でも meshcode が誤っていれば")
say("  種を問わず巨大な移動が現れる**。それを探す")
say("")

all_moves <- do.call(rbind, lapply(d$splist[seq_len(length(d$detect_list))], function(sp) {
  r <- moves_of_species(sp)
  if (is.null(r) || !nrow(r)) return(NULL)
  say(sprintf("    %-14s 移動 %5d 件 / うち %4.0f km 以上 %4d 件 / 最大 %7.1f km",
              sp, nrow(r), MIN_KM, sum(r$km >= MIN_KM), max(r$km)))
  r
}))

far <- all_moves[all_moves$km >= MIN_KM, , drop = FALSE]
say("")
say("  全種合計: 移動 ", format(nrow(all_moves), big.mark = ","), " 件 / うち ",
    MIN_KM, " km 以上 **", format(nrow(far), big.mark = ","), " 件**")

if (nrow(far)) {
  ## 長距離移動の端点になったセルを数える。
  ## **同じセルが種をまたいで何度も現れるなら、そのセルが怪しい**
  tb <- sort(table(c(far$cell_a, far$cell_b)), decreasing = TRUE)
  say("")
  say("  長距離移動の端点になったセル（上位）:")
  say(sprintf("    %8s %-10s %7s %9s %9s %8s %s",
              "セル", "JIS", "出現数", "x(km)", "y(km)", "努力行数", "種数"))
  for (k in head(names(tb), 12)) {
    cl <- as.integer(k)
    rows_here <- which(eff_cell == cl)
    spp_here <- length(unique(far$species[far$cell_a == cl | far$cell_b == cl]))
    say(sprintf("    %8d %-10s %7d %9.1f %9.1f %8d %d",
                cl, ifelse(is.na(jis[cl]), "-", jis[cl]), tb[[k]],
                xy[cl, 1], xy[cl, 2], length(rows_here), spp_here))
  }

  say("")
  say("  ★ 読み方")
  say("    - **1つのセルが多くの種に現れる** → そのセルの meshcode が疑わしい")
  say("    - 出現数が全体に散らばっている   → 本当に長距離移動している（渡り鳥など）")
  say("    - JIS コードと x/y が地理的に整合しているかも確かめること")

  ## 最頻のセルを使う努力行の一覧（原因の特定に直結する）
  top <- as.integer(names(tb)[1])
  rows_top <- which(eff_cell == top)
  say("")
  say("  最頻セル ", top, "（JIS ", ifelse(is.na(jis[top]), "-", jis[top]),
      "）を指す努力行 ", length(rows_top), " 件:")
  show <- head(rows_top, 12)
  for (r in show)
    say(sprintf("      努力行 %5d  %d年  effortID %s  努力 %s",
                r, effort$YEAR[r],
                if ("effortID" %in% names(effort)) format(effort$effortID[r]) else "-",
                format(effort$effort[r])))
  if (length(rows_top) > length(show))
    say("      （残り ", length(rows_top) - length(show), " 行）")
}

# ===========================================================================
# §5 保存
# ===========================================================================
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(all_moves, paste0(OUT, "_all.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (nrow(far))
  write.csv(far, paste0(OUT, "_far.csv"), row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, "_all.csv（全移動 ", nrow(all_moves), " 件）")
if (nrow(far)) say("        ", OUT, "_far.csv（", MIN_KM, " km 以上 ", nrow(far), " 件）")
