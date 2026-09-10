# examples/verify_old_scaling.R ----------------------------------------------
#
# 旧コードの共変量スケーリングが実際どうなっていたかを確かめ、
# 旧解を正しく新パラメータ化へ変換する式を検証する。
#
#   Rscript examples/verify_old_scaling.R
#
# 実データ不要。dplyr の挙動と代数だけで完結する。
#
# ---------------------------------------------------------------------------
# 背景
# ---------------------------------------------------------------------------
#
# 2026-09-09 に SGD_bear_20260909.R を作った際、旧コード
#
#   grid_cov_std <- grid_cov %>% mutate(agri=(agri-mu_agri)/sd_agri,
#                                       agri=(agri-mu_agri)/sd_agri)
#
# を「agri が2回書かれていて wtr が標準化されない」と読んだ。
# そのうえで「agri は正しく標準化されている」と仮定して座標変換の式を立てた。
#
# ところが 2026-09-10、解析機で検算が落ちた（loglf = -704.207 に対し
# 旧記録 -685.520、差 -18.69）。仮定が間違っている。
#
# dplyr の mutate は式を順番に評価し、**後の式は前の式で更新された列を見る**。
# つまり agri には標準化が2回かかっている。wtr は生のまま。
# どちらの共変量も「標準化されていない」状態だった。
# ---------------------------------------------------------------------------

cat("=== 1. dplyr の mutate は同名の列を順番に上書きするか ===\n\n")

suppressPackageStartupMessages(library(dplyr))

x  <- c(1, 2, 3, 4, 5)
mu <- mean(x); sd_ <- sd(x)
d  <- data.frame(v = x)

once  <- d %>% mutate(v = (v - mu) / sd_)
twice <- d %>% mutate(v = (v - mu) / sd_, v = (v - mu) / sd_)

cat("元の値      :", sprintf("%7.4f", x), "\n")
cat("1回標準化   :", sprintf("%7.4f", once$v), " → mean",
    sprintf("%.4f", mean(once$v)), " sd", sprintf("%.4f", sd(once$v)), "\n")
cat("同名で2回   :", sprintf("%7.4f", twice$v), " → mean",
    sprintf("%.4f", mean(twice$v)), " sd", sprintf("%.4f", sd(twice$v)), "\n\n")

expected_twice <- ((x - mu) / sd_ - mu) / sd_
stopifnot(isTRUE(all.equal(twice$v, expected_twice)))
cat("★ 2回目の式は1回目の結果を受け取っている。標準化は2重にかかる。\n")
cat("  結果の sd は 1 ではなく 1/sd(元) =", sprintf("%.4f", 1 / sd_), "になる。\n\n")

# ---------------------------------------------------------------------------
cat("=== 2. 旧モデルが実際に使っていた共変量 ===\n\n")

# Dryad のメッシュ meshutm_0.5km_buff_land.dbf の実測値（ncell = 8497）
MU <- c(agri = 0.1334709, wtr = 0.01422)
SD <- c(agri = 0.2711512, wtr = 0.0723816)

cat(sprintf("生の agri : mean %.5f  sd %.5f\n", MU["agri"], SD["agri"]))
cat(sprintf("生の wtr  : mean %.5f  sd %.5f\n\n", MU["wtr"], SD["wtr"]))

# 旧モデルが渡していた列
#   agri : 2回標準化  →  agri2 = (agri_std - mu_a) / sd_a
#   wtr  : 生のまま
cat("旧モデルが渡していた列の分布:\n")
cat(sprintf("  agri（2回標準化）: mean %.4f  sd %.4f\n",
            -MU["agri"] / SD["agri"], 1 / SD["agri"]))
cat(sprintf("  wtr （生のまま）  : mean %.4f  sd %.4f\n\n", MU["wtr"], SD["wtr"]))
cat("どちらも標準化されていない。**agri は sd が 1 ではなく 3.69。**\n\n")

# ---------------------------------------------------------------------------
cat("=== 3. 正しい座標変換の式 ===\n\n")

# 旧: C = b0 + b_agri * agri2      + b_wtr * wtr_raw
#          agri2 = (agri_std - mu_a)/sd_a ,  wtr_raw = wtr_std*sd_w + mu_w
# 新: C = c0 + c_agri * agri_std   + c_wtr * wtr_std
#
# 展開して係数を合わせると
#   c_agri = b_agri / sd_a
#   c_wtr  = b_wtr  * sd_w
#   c0     = b0 - b_agri*mu_a/sd_a + b_wtr*mu_w

to_std <- function(p, mu, sd) {
  c(dens_0    = unname(p["dens_0"]),
    conn_0    = unname(p["conn_0"]
                       - p["conn_agri"] * mu["agri"] / sd["agri"]
                       + p["conn_wtr"]  * mu["wtr"]),
    conn_agri = unname(p["conn_agri"] / sd["agri"]),
    conn_wtr  = unname(p["conn_wtr"]  * sd["wtr"]),
    g0_1      = unname(p["g0_1"]))
}

# 逆変換（新 → 生スケールの両共変量に対する係数）
to_raw <- function(q, mu, sd) {
  c(dens_0    = unname(q["dens_0"]),
    conn_0    = unname(q["conn_0"]
                       - q["conn_agri"] * mu["agri"] / sd["agri"]
                       - q["conn_wtr"]  * mu["wtr"]  / sd["wtr"]),
    conn_agri = unname(q["conn_agri"] / sd["agri"]),
    conn_wtr  = unname(q["conn_wtr"]  / sd["wtr"]),
    g0_1      = unname(q["g0_1"]))
}

# --- 検証: 任意の mu/sd と係数で線形予測子が保存されるか -------------------
set.seed(1)
worst <- 0
for (i in 1:500) {
  m <- c(agri = runif(1, -2, 2),   wtr = runif(1, -2, 2))
  s <- c(agri = runif(1, .05, 3),  wtr = runif(1, .05, 3))
  b <- c(dens_0 = rnorm(1), conn_0 = rnorm(1), conn_agri = rnorm(1),
         conn_wtr = rnorm(1), g0_1 = rnorm(1))
  q <- to_std(b, m, s); r <- to_raw(q, m, s)

  agri_raw <- rnorm(40, m["agri"], s["agri"])
  wtr_raw  <- rnorm(40, m["wtr"],  s["wtr"])
  agri_std <- (agri_raw - m["agri"]) / s["agri"]
  wtr_std  <- (wtr_raw  - m["wtr"])  / s["wtr"]
  agri2    <- (agri_std - m["agri"]) / s["agri"]      # 旧コードの2重標準化

  C_old <- b["conn_0"] + b["conn_agri"] * agri2    + b["conn_wtr"] * wtr_raw
  C_new <- q["conn_0"] + q["conn_agri"] * agri_std + q["conn_wtr"] * wtr_std
  C_raw <- r["conn_0"] + r["conn_agri"] * agri_raw + r["conn_wtr"] * wtr_raw

  worst <- max(worst, max(abs(C_old - C_new)), max(abs(C_old - C_raw)))
}
cat(sprintf("500通りの mu/sd・係数で線形予測子の最大差: %.3e\n", worst))
stopifnot(worst < 1e-9)
cat("★ 変換式は正しい。\n\n")

# ---------------------------------------------------------------------------
cat("=== 4. 旧解を変換した値（これが新しい初期値／参照点になる） ===\n\n")

OLD_PAR <- c(dens_0    = -1.450490709,
             conn_0    =  0.5579097412,
             conn_agri =  0.3612219522,
             conn_wtr  = -6.410855799,
             g0_1      = -1.791229872)

new_par <- to_std(OLD_PAR, MU, SD)
wrong   <- OLD_PAR                       # 2026-09-09 に使った誤った変換
wrong["conn_wtr"] <- OLD_PAR["conn_wtr"] * SD["wtr"]
wrong["conn_0"]   <- OLD_PAR["conn_0"] + OLD_PAR["conn_wtr"] * MU["wtr"]

print(round(rbind(`旧解（そのまま）`       = OLD_PAR,
                  `誤った変換（09-09）`     = wrong,
                  `正しい変換`             = new_par), 6))

cat("\n生スケール（agri も wtr も生の値）に直すと:\n")
print(round(to_raw(new_par, MU, SD), 6))

cat("\n注意: 旧解の conn_agri = 0.3612 は sd 3.69 の列に対する係数だった。\n")
cat("      正しく標準化した agri に対する係数は",
    sprintf("%.4f", new_par["conn_agri"]), "で、約3.7倍大きい。\n")
cat("      wtr の", sprintf("%.4f", new_par["conn_wtr"]),
    "と比べると、agri の効果のほうがはるかに大きい。\n")
