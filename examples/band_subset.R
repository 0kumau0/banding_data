# examples/band_subset.R -----------------------------------------------------
#
# **足環データを部分メッシュ（既定は関東）に切り出したら、データセットが
#   どうなるかを示す。**
#
#   Rscript examples/band_subset.R
#   Rscript examples/band_subset.R --species シジュウカラ --mesh-sub ../../griddata/mesh2_convex_kanto.gpkg
#   Rscript examples/band_subset.R --species ヤマガラ --out results/band_subset_yamagara.csv
#
# 生データを**読むだけ**。書き出すのは --out の CSV だけ。
#
# ---------------------------------------------------------------------------
# なぜ切り出しを検討するのか（2026-09-29）
#
# シジュウカラを全国（`mesh2_convex7.gpkg`、13,454セル）で解くと、
# `pcap` のコストが `nmu × nind × neffort` = 13,454 × 33,551 × 4,229 になり、
# `loglf` 1回が数時間〜十数時間、SGD 300反復では年単位になる
# （`docs\20260929_作業記録.md`）。
#
# **間引き（`--rate`）は解にならない。** 複数回捕獲個体は毎回全部使う設計なので
# `nind` は 4,587 を下回れない。
#
# **範囲を狭めると `nmu`・`neffort`・`nind` が同時に減り、コストは積なので
# 効果が掛け算になる。** それが実際どのくらいかを、推測ではなく数えて示す。
#
# ---------------------------------------------------------------------------
# ★ 突合の要点 — ここを間違えると全部ずれる
#
# **`effort$meshcode` は JIS メッシュコードではない。`mesh2_convex7.gpkg` の
#   「行番号」**（`make_data&plot.R:99-100` が `row_number()` で振ったもの）。
#
#   mesh <- st_read("../../griddata/mesh2_convex7.gpkg") %>% mutate(meshcode2 = row_number())
#
# したがって:
#
#   - **別の世代の gpkg を使うと、同じ行番号が別の場所を指す。**
#     `convex5` / `convex6` / `convex7` は行数まで同じ（13,454）なので、
#     取り違えても「範囲の照合」では気づけない（2026-09-28 に実際に間違えた）
#   - **部分メッシュの行番号とも対応しない。** 関東の gpkg は独自に 1..1,013 で
#     番号が振られている
#
# そこで対応付けは**行番号ではなく JIS メッシュコード列（`meshcode`）**で行う。
# 両方の gpkg が持っているので厳密に突き合わせられる。
# 列が無い場合だけ、重心の空間的包含に落とす（どちらを使ったか出力に残す）。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--species", "--mesh-all", "--mesh-sub", "--rds", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(.known, collapse = " "))

SPECIES <- .opt("--species", "シジュウカラ")
OUT_CSV <- .opt("--out", "results/band_subset.csv")

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")

MESH_ALL <- .opt("--mesh-all", BAND_MESH)                 # 解析に使う世代（convex7）
MESH_SUB <- .opt("--mesh-sub",
                 file.path(GRID_ROOT, "mesh2_convex_kanto.gpkg"))
RDS      <- .opt("--rds", BAND_RDS)
for (p in c(MESH_ALL, MESH_SUB, RDS))
  if (!file.exists(p)) stop("見つかりません: ", p)

suppressMessages({ library(sf); library(Matrix) })
say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
hr  <- function() say(strrep("-", 74))

# ===========================================================================
# §1 入力 — 何を読んだかを全部書き出す
# ===========================================================================
say("=========================================================================")
say("  足環データの部分メッシュ切り出し: ", SPECIES)
say("=========================================================================")
say("")
say("§1 入力")

mesh_all <- st_read(MESH_ALL, quiet = TRUE)
mesh_sub <- st_read(MESH_SUB, quiet = TRUE)
d        <- readRDS(RDS)

effort <- as.data.frame(d$effort)
i_sp   <- which(d$splist == SPECIES)
if (!length(i_sp)) stop("種が見つかりません: ", SPECIES)
if (i_sp > length(d$detect_list))
  stop(SPECIES, " は splist の ", i_sp, " 番目だが detect_list は ",
       length(d$detect_list), " 種ぶんしかない")
detect <- d$detect_list[[i_sp]]           # 行 = 努力 / 列 = 個体（secrad.r の向き）

say("  A. 全国メッシュ  ", MESH_ALL)
say("       ", format(nrow(mesh_all), big.mark = ","), " セル / 列: ",
    paste(setdiff(names(mesh_all), "geom"), collapse = ", "))
say("  B. 部分メッシュ  ", MESH_SUB)
say("       ", format(nrow(mesh_sub), big.mark = ","), " セル / 列: ",
    paste(setdiff(names(mesh_sub), "geom"), collapse = ", "))
say("  C. 足環データ    ", RDS)
say("       effort      ", format(nrow(effort), big.mark = ","), " 行 × ",
    ncol(effort), " 列: ", paste(names(effort), collapse = ", "))
say("       detect_list ", length(d$detect_list), " 種 / ", SPECIES, " は ",
    format(nrow(detect), big.mark = ","), " × ",
    format(ncol(detect), big.mark = ","), "（行=努力 / 列=個体）")

stopifnot(nrow(detect) == nrow(effort))
say("       ✓ detect の行数と effort の行数が一致（対応が取れている）")

# ===========================================================================
# §2 A と B の対応付け — **行番号ではなく JIS メッシュコードで**
# ===========================================================================
say("")
say("§2 全国メッシュのどのセルが部分メッシュに入るか")

CODE_COL <- "meshcode"        # gpkg が持つ JIS 2次メッシュコード
use_code <- CODE_COL %in% names(mesh_all) && CODE_COL %in% names(mesh_sub)

if (use_code) {
  code_all <- as.character(mesh_all[[CODE_COL]])
  code_sub <- as.character(mesh_sub[[CODE_COL]])
  keep_cell <- which(code_all %in% code_sub)          # ← convex7 の「行番号」
  method <- paste0("JIS メッシュコード列 `", CODE_COL, "` の一致")

  ## 部分メッシュが全国メッシュの部分集合になっているかを確かめる。
  ## なっていなければ、そもそも別系統のメッシュを比べている
  orphan <- setdiff(code_sub, code_all)
  say("  方法: ", method)
  say("       部分メッシュ ", format(length(code_sub), big.mark = ","),
      " セルのうち、全国メッシュに無いもの: ", length(orphan))
  if (length(orphan))
    say("       ⚠ ", length(orphan), " セルが全国メッシュに無い（",
        sprintf("%.0f%%", 100 * length(orphan) / length(code_sub)),
        "）。例: ", paste(head(orphan, 3), collapse = ", "), "\n",
        "         → **部分メッシュは全国メッシュの部分集合ではない。**\n",
        "           別の世代・別の作り方の gpkg。解析に使えるのは共通部分だけで、\n",
        "           部分メッシュのファイルをそのまま使うと effort との対応が取れない")
} else {
  ## コード列が無い場合だけ、重心が部分メッシュの内側にあるかで判定する。
  ## 境界上のセルの扱いが曖昧になるので、**使ったことを必ず残す**
  say("  ⚠ `", CODE_COL, "` 列が両方に無いので、重心の空間的包含で判定する")
  ctr <- suppressWarnings(st_centroid(st_geometry(mesh_all)))
  if (!identical(st_crs(mesh_all), st_crs(mesh_sub)))
    stop("CRS が違うので空間判定できない: ",
         st_crs(mesh_all)$input, " vs ", st_crs(mesh_sub)$input)
  inside <- lengths(st_within(ctr, st_union(st_geometry(mesh_sub)))) > 0
  keep_cell <- which(inside)
  method <- "重心の空間的包含（st_within）"
}

say("  → 全国 ", format(nrow(mesh_all), big.mark = ","), " セルのうち **",
    format(length(keep_cell), big.mark = ","), " セル** が部分メッシュに入る")
if (!length(keep_cell)) stop("部分メッシュに入るセルが1つも無い。メッシュの取り違えを疑うこと")

## 位置の確認（取り違えていれば経緯度がおかしくなるので一目で分かる）
bb_all <- st_bbox(mesh_all); bb_sub <- st_bbox(mesh_sub)
say("  範囲（全国）: x ", round(bb_all["xmin"]), "〜", round(bb_all["xmax"]),
    " / y ", round(bb_all["ymin"]), "〜", round(bb_all["ymax"]))
say("  範囲（部分）: x ", round(bb_sub["xmin"]), "〜", round(bb_sub["xmax"]),
    " / y ", round(bb_sub["ymin"]), "〜", round(bb_sub["ymax"]))

# ===========================================================================
# §3 努力の絞り込み
# ===========================================================================
say("")
say("§3 検出努力の絞り込み")

## ★ effort$meshcode は「convex7 の行番号」。JIS コードではない
eff_cell <- as.integer(as.character(effort$meshcode))
stopifnot(!anyNA(eff_cell), min(eff_cell) >= 1L, max(eff_cell) <= nrow(mesh_all))
say("  effort$meshcode は全国メッシュの行番号（範囲 ", min(eff_cell), "〜",
    max(eff_cell), "）。JIS コードではない")

keep_eff <- which(eff_cell %in% keep_cell)
say("  努力 ", format(nrow(effort), big.mark = ","), " 行 → **",
    format(length(keep_eff), big.mark = ","), " 行**（",
    sprintf("%.1f%%", 100 * length(keep_eff) / nrow(effort)), "）")
if (!length(keep_eff)) stop("部分メッシュ内に努力が1行も無い")

eff_sub  <- effort[keep_eff, , drop = FALSE]
occ_all  <- length(unique(effort$YEAR))
occ_sub  <- length(unique(eff_sub$YEAR))
say("  努力のあるセル: ", format(length(unique(eff_cell)), big.mark = ","),
    " → **", format(length(unique(eff_cell[keep_eff])), big.mark = ","), "**")
say("  機会（年）    : ", occ_all, " → **", occ_sub, "**（",
    min(eff_sub$YEAR), "〜", max(eff_sub$YEAR), "）")

# ===========================================================================
# §4 個体の絞り込み
# ===========================================================================
say("")
say("§4 個体の絞り込み")

## 検出行列を**行（＝努力）**で切る。列で切ると個体を削ってしまう
detect_sub <- detect[keep_eff, , drop = FALSE]

cnt_all <- Matrix::colSums(detect)          # 全国での検出回数
cnt_sub <- Matrix::colSums(detect_sub)      # 部分メッシュ内での検出回数
keep_ind <- which(cnt_sub > 0)              # 部分メッシュ内で1回以上捕まった個体

## ★ 検出0回の列（各種1つ入っているダミー個体）は個体として数えない
n_ind_all <- sum(cnt_all > 0)
if (ncol(detect) != n_ind_all)
  say("  ＊ 検出0回の列が ", ncol(detect) - n_ind_all,
      " 列ある（ダミー個体）。個体数からは除く")

say("  個体 ", format(n_ind_all, big.mark = ","), " → **",
    format(length(keep_ind), big.mark = ","), "**（",
    sprintf("%.1f%%", 100 * length(keep_ind) / n_ind_all), "）")
say("  検出 ", format(sum(cnt_all), big.mark = ","), " → **",
    format(sum(cnt_sub), big.mark = ","), "**")

## ★ **境界で切られる情報**。ここが範囲を狭める代償
n_cross <- sum(cnt_sub > 0 & (cnt_all - cnt_sub) > 0)
lost_cap <- sum((cnt_all - cnt_sub)[keep_ind])
say("  ⚠ 部分メッシュの内と外の両方で捕まった個体: **",
    format(n_cross, big.mark = ","), "**（この個体の外側の捕獲 ",
    format(lost_cap, big.mark = ","), " 件が落ちる）")
say("     → **境界をまたぐ移動は観測できなくなる。** 連結性の推定が",
    "範囲の内側に限定されることを意味する")

detect_sub <- detect_sub[, keep_ind, drop = FALSE]
cnt        <- cnt_sub[keep_ind]
n_multi    <- sum(cnt > 1); n_single <- sum(cnt == 1)

# ===========================================================================
# §5 移動（連結性の情報量）
# ===========================================================================
say("")
say("§5 移動の観測数")

## セル番号を 1..length(keep_cell) に振り直す。
## **部分メッシュで解析するなら行番号もその中で閉じている必要がある**
cell_new <- match(eff_cell[keep_eff], keep_cell)
stopifnot(!anyNA(cell_new), max(cell_new) <= length(keep_cell))

## 「2つ以上の異なるセルで捕まった個体」を数える。
## **再捕獲回数ではなく、これが連結性の情報量**
## （ヤマガラは再捕獲率が最高でも、ほぼ同じセル内なので移動が見えない）
count_moved <- function(m, cell_of_row) {
  if (!inherits(m, "dgCMatrix")) m <- methods::as(m, "dgCMatrix")
  p  <- m@p
  jj <- rep.int(seq_len(ncol(m)), diff(p))     # 格納要素の列（＝個体）
  cc <- cell_of_row[m@i + 1L]                  # 格納要素のセル
  ## ★★ **値が 0 の格納要素を落とす**（2026-09-29 に修正）。
  ## 各種ちょうど1列、全要素が 0 のダミー個体が入っており、
  ## 落とさないと「捕獲0回の個体が800セル以上を移動した」ことになる。
  ## 全30種の move_km_max が揃って 3020.3 km になっていた原因。
  ## examples/band_moves.R が実証、examples/band_scale_check.R も同時に修正
  keep <- m@x > 0
  jj <- jj[keep]; cc <- cc[keep]
  o  <- order(jj, cc); jj <- jj[o]; cc <- cc[o]
  n  <- length(jj)
  if (!n) return(integer(0))
  newpair <- c(TRUE, (jj[-1] != jj[-n]) | (cc[-1] != cc[-n]))
  tabulate(jj[newpair], nbins = ncol(m))       # 個体ごとの「異なるセル数」
}
n_cells_per_ind <- count_moved(detect_sub, cell_new)
n_moved <- sum(n_cells_per_ind >= 2L)

n_moved_all <- {
  nc <- count_moved(detect, eff_cell)
  sum(nc >= 2L)
}

say("  2セル以上で捕まった個体（＝移動が観測された個体）")
say("    全国    : ", format(n_moved_all, big.mark = ","))
say("    部分    : **", format(n_moved, big.mark = ","), "**")
say("  1個体あたり検出: 全国 ", sprintf("%.3f", sum(cnt_all) / n_ind_all),
    " → 部分 **", sprintf("%.3f", sum(cnt) / length(cnt)), "**")

## --- 検算 -------------------------------------------------------------------
## `examples/band_scale_check.R` が別の経路で同じ量を数えている。
## **独立した2つの実装が一致するかを自動で確かめる。**
## ずれたらどちらかが間違っているので、先にそれを解決すること
SCALE_CSV <- "results/band_scale_check.csv"
if (file.exists(SCALE_CSV)) {
  sc <- read.csv(SCALE_CSV, stringsAsFactors = FALSE)
  row <- sc[sc$group == SPECIES, , drop = FALSE]
  if (nrow(row) == 1L) {
    chk <- data.frame(
      量 = c("n_ind", "n_multi", "n_single", "n_record", "n_moved"),
      band_scale_check = c(row$n_ind, row$n_multi, row$n_single,
                           row$n_record, row$n_moved),
      今回 = c(n_ind_all, sum(cnt_all > 1), sum(cnt_all == 1),
               sum(cnt_all), n_moved_all))
    chk$一致 <- ifelse(chk$band_scale_check == chk$今回, "✓", "★ずれ")
    say("")
    say("  検算（", SCALE_CSV, " の全国値と突き合わせ）:")
    for (i in seq_len(nrow(chk)))
      say(sprintf("    %-9s %8s vs %8s  %s", chk$量[i],
                  format(chk$band_scale_check[i], big.mark = ","),
                  format(chk$今回[i], big.mark = ","), chk$一致[i]))
    if (any(chk$一致 != "✓"))
      say("    ★ ずれている。どちらかの数え方が誤り。先にこれを解決すること")
  }
}

## --- ★ 連結性が推定できるだけの移動があるか -------------------------------
## **これが足りなければ、計算が速くなっても意味がない。**
## ADCR が推定するのは透過性（conn_0 / conn_agri / conn_wtr の3係数）で、
## その情報源は「別のセルで再捕獲された個体」だけ
if (n_moved < 30) {
  say("")
  say("  ⚠⚠ **移動が観測された個体が ", n_moved, " しかない。**")
  say("     ADCR が推定する透過性の係数は conn_0 / conn_agri / conn_wtr の3つで、")
  say("     その情報源はこの個体だけ。**係数3つに対して観測 ", n_moved, " 件。**")
  say("     計算が速くなっても、推定できるかどうかは別の問題として残る")
}

# ===========================================================================
# §6 計算コストの見込み
# ===========================================================================
say("")
say("§6 計算コストの見込み")

## results/pcap_bench.csv の実測からの外挿。
## 基準: nmu 400 / neffort 25 / nind 19,200 → 現行 83.09秒 / 修正版 7.02秒。
## nmu と neffort には線形、nind は実測の指数（現行 1.78 / 修正版 1.10）。
## **as.numeric() は必須**（整数だと積が 2^31 を超えて NA になる）
est_pcap <- function(nmu, nind, neff, fixed = FALSE) {
  t0 <- if (fixed) 7.02 else 83.09
  k  <- if (fixed) 1.10 else 1.78
  t0 * (as.numeric(nmu) / 400) * (as.numeric(neff) / 25) *
    (as.numeric(nind) / 19200)^k
}
fmt_t <- function(s) if (s < 60) sprintf("%.0f 秒", s) else
  if (s < 3600) sprintf("%.1f 分", s / 60) else
    if (s < 86400) sprintf("%.1f 時間", s / 3600) else sprintf("%.1f 日", s / 86400)

## 1反復 = loglf 約5.2回（クマの実測 936秒 / 181秒）、参照解到達に 300反復
LL_PER_ITER <- 5.2; N_ITER <- 300

scen <- rbind(
  data.frame(範囲 = "全国", nmu = nrow(mesh_all), neffort = nrow(effort),
             nind = n_ind_all),
  data.frame(範囲 = "部分", nmu = length(keep_cell), neffort = length(keep_eff),
             nind = length(keep_ind)))
scen$loglf_現行 <- sapply(seq_len(nrow(scen)), function(i)
  est_pcap(scen$nmu[i], scen$nind[i], scen$neffort[i]))
scen$loglf_修正 <- sapply(seq_len(nrow(scen)), function(i)
  est_pcap(scen$nmu[i], scen$nind[i], scen$neffort[i], fixed = TRUE))

for (i in seq_len(nrow(scen))) {
  say(sprintf("  %-4s nmu %6s × nind %6s × neffort %5s", scen$範囲[i],
              format(scen$nmu[i], big.mark = ","),
              format(scen$nind[i], big.mark = ","),
              format(scen$neffort[i], big.mark = ",")))
  say(sprintf("       loglf 1回: 現行 %-10s / 修正後 %-10s",
              fmt_t(scen$loglf_現行[i]), fmt_t(scen$loglf_修正[i])))
  say(sprintf("       SGD %d反復: 現行 %-10s / 修正後 %-10s", N_ITER,
              fmt_t(scen$loglf_現行[i] * LL_PER_ITER * N_ITER),
              fmt_t(scen$loglf_修正[i] * LL_PER_ITER * N_ITER)))
}
say(sprintf("  → 部分メッシュにすると **%.0f 倍** 軽くなる（nmu・neffort・nind の積の比）",
            (as.numeric(scen$nmu[1]) * scen$nind[1] * scen$neffort[1]) /
            (as.numeric(scen$nmu[2]) * scen$nind[2] * scen$neffort[2])))
say("  ＊ 見積もりは results/pcap_bench.csv からの外挿。実測ではない")
say("  ＊ 多峰性のため3点以上の多点出発が要る（上の値 × 3）")

# ===========================================================================
# §7 まとめ
# ===========================================================================
say("")
hr()
say("  まとめ: ", SPECIES, "  全国 → ", basename(MESH_SUB))
hr()
out <- data.frame(
  species     = SPECIES,
  mesh_all    = basename(MESH_ALL),   mesh_sub = basename(MESH_SUB),
  match_by    = method,
  ncell_all   = nrow(mesh_all),       ncell_sub   = length(keep_cell),
  neffort_all = nrow(effort),         neffort_sub = length(keep_eff),
  ncell_eff_all = length(unique(eff_cell)),
  ncell_eff_sub = length(unique(eff_cell[keep_eff])),
  nocc_all    = occ_all,              nocc_sub    = occ_sub,
  nind_all    = n_ind_all,         nind_sub    = length(keep_ind),
  ndet_all    = sum(cnt_all),         ndet_sub    = sum(cnt),
  nmulti_sub  = n_multi,              nsingle_sub = n_single,
  det_per_ind_all = sum(cnt_all) / n_ind_all,
  det_per_ind_sub = sum(cnt) / length(cnt),
  nmoved_all  = n_moved_all,          nmoved_sub  = n_moved,
  ncross      = n_cross,              lost_cap    = lost_cap,
  loglf_now_sub_sec = scen$loglf_現行[2],
  loglf_fix_sub_sec = scen$loglf_修正[2])

tbl <- data.frame(
  項目 = c("メッシュのセル数", "努力の行数", "努力のあるセル", "機会（年）",
           "個体数", "うち複数回捕獲", "うち単回捕獲", "検出の総数",
           "1個体あたり検出", "移動が見えた個体"),
  全国 = c(out$ncell_all, out$neffort_all, out$ncell_eff_all, out$nocc_all,
           out$nind_all, sum(cnt_all > 1), sum(cnt_all == 1), out$ndet_all,
           round(out$det_per_ind_all, 3), out$nmoved_all),
  部分 = c(out$ncell_sub, out$neffort_sub, out$ncell_eff_sub, out$nocc_sub,
           out$nind_sub, out$nmulti_sub, out$nsingle_sub, out$ndet_sub,
           round(out$det_per_ind_sub, 3), out$nmoved_sub))
tbl$残る割合 <- ifelse(tbl$全国 > 0, sprintf("%.1f%%", 100 * tbl$部分 / tbl$全国), "-")
print(tbl, row.names = FALSE)

say("")
say("  ★ 判断材料")
say("    - 移動が見えた個体 ", n_moved_all, " → ", n_moved,
    " 。**連結性はこの数で決まる**（再捕獲の回数ではない）")
say("    - 1個体あたり検出 ", sprintf("%.3f", out$det_per_ind_sub),
    "。多点出発の必要性の目安は 1.11 が危険域 / 1.29 で 0/77")
say("       （reports/20260924_multistart_report.md）")
say("    - 境界をまたぐ個体 ", n_cross,
    " 。**この個体の移動は観測から消える**。端の効果として残る")

dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT_CSV, row.names = FALSE)
say("")
say("  保存: ", OUT_CSV)
