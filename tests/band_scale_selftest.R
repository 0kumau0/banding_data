# tests/band_scale_selftest.R ------------------------------------------------
#
# examples/band_scale_check.R を、実データ抜きで確かめる。
#
#   Rscript tests/band_scale_selftest.R <出力先.rds>
#   Rscript examples/band_scale_check.R --rds <出力先.rds> --out <集計先.csv> --min-ind 1
#
# ---------------------------------------------------------------------------
# 2026-09-28 に2回、構造の仮定を外している。
#
#   1回目 … 「1要素目が data.frame」と仮定 → 実際は236種の種名ベクトルだった
#   2回目 … 「GUID + RING の捕獲記録」と仮定 → 実際は **ADCR 形式**
#           （effort の表 ＋ 種ごとの検出行列）。クマの
#           effort_231225.csv / detectmat_231225.csv と同じ形で、
#           **個体は行列の列**として入っている
#
# ここでは実データと同じ形（splist / effort / detect_list）を合成して、
# **検出行列から個体数と検出回数を正しく数えられるか**を確認する。
# 生データは使わない。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
OUT <- if (length(args)) args[1] else stop("使い方: Rscript tests/band_scale_selftest.R <out.rds>")
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Matrix パッケージが要ります")

set.seed(20260928)
SPP <- c("アオジ", "オオジュリン", "メジロ")
N_EFFORT <- 300L                                  # 検出努力の数（実データは 4229）
N_MESH   <- 80L

effort <- data.frame(
  YEAR     = sample(2000:2020, N_EFFORT, replace = TRUE),
  meshcode = sample(sprintf("%06d", seq_len(N_MESH)), N_EFFORT, replace = TRUE),
  effort   = round(runif(N_EFFORT, 1, 30)),
  effortID = seq_len(N_EFFORT),
  stringsAsFactors = FALSE)

## 1種ぶんの検出行列。**行 = 検出努力 / 列 = 個体**（secrad.r の向き）。
## 個体ごとの検出回数を指定の割合で偏らせる。
make_detect <- function(n_ind, p_multi) {
  n_cap <- 1L + rbinom(n_ind, 3L, p_multi)        # 1回 〜 4回
  j <- rep(seq_len(n_ind), n_cap)
  i <- unlist(lapply(n_cap, function(k) sample.int(N_EFFORT, k)))
  Matrix::sparseMatrix(i = i, j = j, x = 1,
                       dims = c(N_EFFORT, n_ind))
}

detect_list <- setNames(list(make_detect(400L, 0.10),
                             make_detect(250L, 0.05),
                             make_detect(120L, 0.30)), SPP)

obj <- list(splist      = c(SPP, sprintf("種%03d", 1:233)),   # 236種の名前
            effort      = effort,
            detect_list = detect_list)

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
saveRDS(obj, OUT)

cat("合成データを書きました: ", OUT, "\n", sep = "")
cat("  構造: list(splist = character(236), effort = data.frame(", N_EFFORT,
    "行), detect_list = 疎行列3件)\n", sep = "")
cat("\n  期待する挙動:\n")
cat("    - splist は行列でも data.frame でもないので拾われない\n")
cat("    - effort が『検出努力の表』として認識され、行数 ", N_EFFORT, " が報告される\n", sep = "")
cat("    - detect_list の3件が検出行列として拾われ、**行数が effort と一致**すると報告される\n")
cat("    - 個体数は**列数**（400 / 250 / 120）。行数（", N_EFFORT, "）ではない\n", sep = "")
cat("\n  期待する数値:\n")
for (sp in SPP) {
  m <- detect_list[[sp]]
  cnt <- Matrix::colSums(m)
  cat(sprintf("    %-12s 記録 %4d / 個体 %3d / 複数回 %3d / 単回 %3d / 1個体あたり %.3f\n",
              sp, sum(cnt), ncol(m), sum(cnt > 1), sum(cnt == 1), sum(cnt) / ncol(m)))
}
tot_rec <- sum(sapply(detect_list, function(m) sum(Matrix::colSums(m))))
tot_ind <- sum(sapply(detect_list, ncol))
cat(sprintf("    %-12s 記録 %4d / 個体 %3d /                       1個体あたり %.3f\n",
            "（合計）", tot_rec, tot_ind, tot_rec / tot_ind))

cat("\n次のコマンドで確認:\n")
cat("  Rscript examples/band_scale_check.R --rds ", OUT,
    " --out results/_selftest_band.csv --min-ind 1\n", sep = "")
