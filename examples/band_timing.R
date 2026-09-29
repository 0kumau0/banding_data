# examples/band_timing.R -----------------------------------------------------
#
# **足環データで `loglf` を1回測る。** 本番の予定を立てる前の見積もり。
#
#   Rscript examples/band_timing.R                          # シジュウカラ、rate=1
#   Rscript examples/band_timing.R --species ヤマガラ
#   Rscript examples/band_timing.R --rate 0.1 --reps 2
#   Rscript examples/band_timing.R --threads 16
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-29）
#
# `ncell` の項は見積もれている（13,454 セルで約49分/反復）が、
# **個体数の項は測っていない**。シジュウカラは 33,551個体でクマの308倍、
# `pcap` のコストは `nind` の約1.8乗（results/pcap_bench.csv）。
#
# **`loglf` が10分なら300反復で約5日。3時間なら約2か月**で、
# その場合は `pcap` の修正（reports/pcap_fix_decision.md）が必須になる。
# **この1回の測定が、修正するかどうかの判断材料そのもの。**
#
# ---------------------------------------------------------------------------
# make_data&plot.R との違い（意図的に直してある3点）
#
#   1. **grid_cov_std を上書きしない。** 向こうは :234 で標準化した共変量を入れ、
#      :237 で同名に mu/sd の表を代入して消している。
#      ここでは平均・標準偏差は grid_cov_scale という別名にする
#   2. **resolution を 13,454² の距離行列から求めない。** 向こうは :201-206 で
#      2.9GB の行列を2つ作る。メッシュは10km四方の正則格子と実測済みなので直に書く
#   3. **effort_occ を確実に持たせる。** 無ければ YEAR から作る
#
# ⚠ 生データを読む。編集機の Claude は実行しないこと。
# ---------------------------------------------------------------------------

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.bad <- setdiff(grep("^--", .args, value = TRUE),
                c("--species", "--rate", "--reps", "--threads", "--mesh", "--rds"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

SPECIES <- .opt("--species", "シジュウカラ")
RATE    <- as.numeric(.opt("--rate", "1"))
REPS    <- as.integer(.opt("--reps", "1"))
stopifnot(RATE > 0, RATE <= 1, REPS >= 1L)
if (!is.null(.opt("--threads", NULL)))
  Sys.setenv(OMP_NUM_THREADS = .opt("--threads", ""))

if (!file.exists("config.R"))
  stop("config.R が見つかりません。リポジトリルートで実行してください: ", getwd())
source("config.R", encoding = "UTF-8")
MESH <- .opt("--mesh", BAND_MESH)
RDS  <- .opt("--rds",  BAND_RDS)
for (p in c(MESH, RDS)) if (!file.exists(p)) stop("見つかりません: ", p)

t0 <- Sys.time()
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("== 足環データの loglf 計測 ==")
say("  種: ", SPECIES, " / sampling_rate: ", RATE, " / 繰り返し: ", REPS)
say("  メッシュ: ", MESH)
say("  データ  : ", RDS)

suppressMessages({ library(sf); library(Matrix) })
say("尤度エンジンを読み込み中（C++ のコンパイルで30秒ほど）...")
suppressMessages(suppressWarnings(source("adcrsgd/secrad.r", encoding = "UTF-8")))
source("adcrsgd/sgd_utils.R", encoding = "UTF-8")

# ---------------------------------------------------------------------------
# 1. メッシュ
# ---------------------------------------------------------------------------
mesh <- st_read(MESH, quiet = TRUE)
ncell <- nrow(mesh)
xy <- st_coordinates(st_centroid(st_geometry(mesh)))[, 1:2, drop = FALSE] / 1000  # km
coords <- cbind(x = xy[, 1], y = xy[, 2])

## **10km 四方の正則格子であることを確かめてから直に書く**（距離行列は作らない）
ar_km2 <- as.numeric(st_area(mesh)) / 1e6
say(sprintf("メッシュ: %d セル / 1セルの面積 中央 %.1f km²（最小 %.1f 最大 %.1f）",
            ncell, median(ar_km2), min(ar_km2), max(ar_km2)))
if (diff(range(ar_km2)) > 1)
  say("  ⚠ セルの面積が一様でない。area を実面積にすべきか要検討")
area <- rep(round(median(ar_km2)), ncell)
resolution <- c(x = 10, y = 10)      # km。coords が km なので
say("  resolution = (", resolution[1], ", ", resolution[2], ") km / area = ",
    area[1], " km²")

## --- 共変量 -----------------------------------------------------------------
need <- c("cultivated", "openwater")
if (!all(need %in% names(mesh)))
  stop("共変量の列が無い: ", paste(setdiff(need, names(mesh)), collapse = ", "),
       "\nメッシュの列: ", paste(names(mesh), collapse = ", "))
grid_cov_raw <- data.frame(agri = mesh$cultivated, wtr = mesh$openwater)
grid_cov_scale <- data.frame(                     # ★ 別名。上書きしない
  mu = sapply(grid_cov_raw, mean), sd = sapply(grid_cov_raw, sd))
grid_cov <- as.data.frame(scale(grid_cov_raw))    # 平均0・分散1
say(sprintf("共変量: agri mean %.4f sd %.4f / wtr mean %.4f sd %.4f（標準化前）",
            grid_cov_scale["agri", "mu"], grid_cov_scale["agri", "sd"],
            grid_cov_scale["wtr", "mu"],  grid_cov_scale["wtr", "sd"]))
stopifnot(nrow(grid_cov) == ncell,
          all(abs(colMeans(grid_cov)) < 1e-10),
          all(abs(apply(grid_cov, 2, sd) - 1) < 1e-10))

# ---------------------------------------------------------------------------
# 2. 努力と検出
# ---------------------------------------------------------------------------
d <- readRDS(RDS)
effort <- as.data.frame(d$effort)
i_sp <- which(d$splist == SPECIES)
if (!length(i_sp)) stop("種が見つかりません: ", SPECIES)
if (i_sp > length(d$detect_list))
  stop(SPECIES, " は splist の ", i_sp, " 番目だが、detect_list は ",
       length(d$detect_list), " 種ぶんしかない")
detect <- d$detect_list[[i_sp]]

effort_loc <- as.integer(as.character(effort$meshcode))   # ★ meshcode は行番号
## トップレベルでは if(...) の次の行に else を置くと構文エラーになるので括弧で包む
effort_occ <- if (!is.null(effort$effort_occ)) {
  as.integer(effort$effort_occ)
} else {
  as.integer(factor(effort$YEAR))     # 2009〜2018 → 1〜10
}
effort_vec <- as.numeric(effort$effort)

## **向きと対応の確認。** ここを間違えると個体数が検出器数に化ける
stopifnot(nrow(detect) == nrow(effort),
          length(effort_loc) == nrow(effort),
          !anyNA(effort_loc), max(effort_loc) <= ncell,
          length(effort_occ) == nrow(effort))
say(sprintf("努力 %d 行 / 機会 %d（%s〜%s）/ セル %d",
            nrow(effort), length(unique(effort_occ)),
            min(effort$YEAR), max(effort$YEAR), length(unique(effort_loc))))

cnt <- Matrix::colSums(detect)
say(sprintf("%s: 個体 %d（複数回 %d / 単回 %d）/ 検出 %d",
            SPECIES, ncol(detect), sum(cnt > 1), sum(cnt == 1), sum(cnt)))

## --- 間引き（rate < 1）-------------------------------------------------------
## 複数回捕獲は全部使い、単回だけ抽出する（SGD と同じ設計）
if (RATE < 1) {
  set.seed(20260929)
  multi <- which(cnt > 1); single <- which(cnt == 1)
  keep <- sort(c(multi, sample(single, max(1L, floor(length(single) * RATE)))))
  detect <- detect[, keep, drop = FALSE]
  say(sprintf("間引き: %d → %d 個体（複数回 %d ＋ 単回 %d）",
              length(cnt), length(keep), length(multi), length(keep) - length(multi)))
}
nind <- ncol(detect)

## --- メモリの見積もり --------------------------------------------------------
## secrad.r:1174-1176 が ncell × nind × nobs の配列を2つ持つ
gb <- 2 * ncell * nind * 8 / 1e9
say(sprintf("★ 見込みメモリ: ncell %d × nind %d × 8バイト × 2 = **%.1f GB**",
            ncell, nind, gb))
say(sprintf("   （advdiff の ncell² もこれとは別に %.2f GB）", ncell^2 * 8 / 1e9))
if (gb > 40) say("   ⚠ 大きい。落ちたら --rate を下げること")

# ---------------------------------------------------------------------------
# 3. 組み立て
# ---------------------------------------------------------------------------
secrdata <- secrad_data$new(coords = coords, area = area,
                            grid_cov = grid_cov, resolution = resolution)
secrdata$add_obs(type = "poisson", effort = effort_vec,
                 effort_loc = effort_loc, effort_occ = effort_occ,
                 detect = as.matrix(detect))
obj <- secrad$new(secrdata = secrdata)
obj$set_model(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))
par0 <- generate_init(obj)
say("パラメータ: ", paste(names(par0), collapse = ", "))
par0[c("dens_0", "conn_0", "g0_1")] <- c(-1, -2, -5)     # far の初期値

# ---------------------------------------------------------------------------
# 4. 計測
# ---------------------------------------------------------------------------
say("loglf を測定中（1回目はキャッシュが無いので最も遅い）...")
secs <- numeric(REPS)
for (k in seq_len(REPS)) {
  tk <- system.time(ll <- obj$loglf(par0, loglfscale = 1))[["elapsed"]]
  secs[k] <- tk
  say(sprintf("  %d回目: %.1f 秒（%.1f 分） logL = %.4f", k, tk, tk / 60, ll))
}

# ---------------------------------------------------------------------------
# 5. 見積もり
# ---------------------------------------------------------------------------
s <- min(secs)
say("")
say("== 見積もり（最速の1回 ", sprintf("%.1f", s), " 秒を使う）==")
## 1反復 = 前進差分で 6評価。advdiff はキャッシュ共有で4回（adcrsgd/sgd_utils.R）
per_iter <- s * 6
say(sprintf("  1反復（loglf 6回）      : %.1f 分", per_iter / 60))
for (n in c(100, 200, 300)) {
  h <- per_iter * n / 3600
  say(sprintf("  %3d 反復                : %.1f 時間（%.1f 日）", n, h, h / 24))
}
say("")
say("  ＊ キャッシュ共有で advdiff は1反復4回なので、実際はこれより速い")
say("  ＊ クマは 1反復 15.6分 / 300反復 78時間 だった")
say("  ＊ 遅すぎる場合: (1) --rate を下げる (2) pcap の修正")
say("     （reports/pcap_fix_decision.md。nind 19,200 で 11.8倍の実測）")
say("")
say(sprintf("全体 %.1f 分", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
