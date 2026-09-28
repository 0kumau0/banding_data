# tests/band_scale_selftest.R ------------------------------------------------
#
# examples/band_scale_check.R の**構造探索**を、実データ抜きで確かめる。
#
#   Rscript tests/band_scale_selftest.R <出力先.rds>
#   Rscript examples/band_scale_check.R --rds <出力先.rds> --out <集計先.csv>
#
# 2026-09-28: 足環データの rds は「1要素目が236種の種名ベクトル」という形で、
# 「1要素目が data.frame」という当初の仮定で書いたスクリプトが止まった。
# **入れ子のどこに data.frame があっても拾う**ように書き直したので、
# その探索が効くかを合成データで確認する。
#
# 生データは使わない。捕獲記録を模した小さなリストを作るだけ。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
OUT <- if (length(args)) args[1] else stop("使い方: Rscript tests/band_scale_selftest.R <out.rds>")

set.seed(20260928)
SPP <- c("アオジ", "オオジュリン", "メジロ")

## 1種ぶんの捕獲記録。個体ごとの捕獲回数を意図的に偏らせる
## （複数回捕獲が少なく、単回が多い＝足環データの見込みに近い形）
make_sp <- function(sp, n_ind, p_multi) {
  n_cap <- 1L + rbinom(n_ind, 3L, p_multi)          # 1回 〜 4回
  ring  <- sprintf("%s-%05d", substr(sp, 1, 1), seq_len(n_ind))
  data.frame(
    GUID   = rep(sprintf("G%04d", seq_len(n_ind)), n_cap),
    RING   = rep(ring, n_cap),
    SPNAMK = sp,
    PLACE  = sample(sprintf("P%02d", 1:12), sum(n_cap), replace = TRUE),
    DATE   = sample(2000:2020, sum(n_cap), replace = TRUE),
    stringsAsFactors = FALSE)
}

## **実データと同じ形にする**: 1要素目が種名のベクトル、データはその先のリスト
obj <- list(
  splist = c(SPP, sprintf("種%03d", 1:233)),        # 236 種の名前（データは無い）
  data   = setNames(list(make_sp(SPP[1], 400, 0.10),
                         make_sp(SPP[2], 250, 0.05),
                         make_sp(SPP[3], 120, 0.30)), SPP),
  meta   = list(created = "selftest", note = "合成データ")
)

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
saveRDS(obj, OUT)

cat("合成データを書きました: ", OUT, "\n", sep = "")
cat("  構造: list(splist = 236種の名前, data = 種ごとの data.frame 3件, meta = list)\n")
cat("  期待する挙動:\n")
cat("    - splist（character）と meta（list）は data.frame として拾われない\n")
cat("    - data$<種名> の3件が『個体IDあり』として拾われる\n")
cat("    - 3件それぞれが1行に集計される\n\n")
for (sp in SPP) {
  x <- obj$data[[sp]]
  n_ind <- length(unique(x$RING))
  cat(sprintf("    %-12s 記録 %4d / 個体 %3d / 1個体あたり %.3f\n",
              sp, nrow(x), n_ind, nrow(x) / n_ind))
}
cat("\n次のコマンドで確認:\n")
cat("  Rscript examples/band_scale_check.R --rds ", OUT,
    " --out results/_selftest_band.csv --min-ind 1\n", sep = "")
