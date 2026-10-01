# examples/identify_nind.R ---------------------------------------------------
#
# **つながりの本数を固定したまま個体数だけを振って、標準誤差がどう変わるかを測る。**
#
#   Rscript examples/identify_nind.R --dry-run
#   Rscript examples/identify_nind.R --pairs 4,12 --rates 1,0.4,0.15,0.05 --sims 3
#
# **実データは要らない。** 合成データのみ。書き出すのは results/identify_nind*.csv。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-30）
#
# `examples/identify_conn.R` で「つながり何本あれば透過性が推定できるか」を測り、
#
#   つながり 0〜1本 … 推定不能（SE 1.3〜16.6）
#   つながり 2本以上 … SE 0.3 前後（真値 0.6 に対して）
#   つながり 11〜20本 … SE 0.20
#
# と分かった。**ただしシミュレーションの個体数は 100〜290 で、実データは
# 関東 16,308 / 全国 33,550 と 100倍ある。**
#
# さらに対照実験（`--ablate`）で、**つながりを完全に壊しても SE は
# 2.65倍になるだけ**（無限大にならない）と分かった。
# つまり**同じ場所の再捕獲も情報を持っている**。
#
#   → **個体数が100倍なら、同じ4本でも SE はもっと小さいかもしれない。**
#     逆に頭打ちかもしれない。**測らないと分からない。**
#
# **関東（つながり4本・個体16,308）に29日投じる価値があるかは、これで決まる。**
#
# ---------------------------------------------------------------------------
# ★ 設計上の難所 — つながりと個体数は普通は連動する
#
# 密度や検出率を上げると、個体数も移動の本数も**同時に**増える。
# それでは「どちらが効いたか」が分からない（`identify_conn.R` の走査の弱点）。
#
# **切り離し方**:
#
#   1. 高密度で1回シミュレートする（移動個体がたくさん出る）
#   2. **移動個体のうち k 個体だけ繋がりを残し、残りは繋がりだけ壊す**
#      （検出器ごとに別個体へ分解。`--ablate` と同じ操作。
#        **総捕獲数も検出器ごとの捕獲数も変わらない**）
#      → つながりが **k 本に固定される**
#   3. **1か所でしか捕まっていない個体を間引いて個体数を振る**
#      （残した移動個体は絶対に間引かない）
#
# これで「つながり4本・個体2,000」のような、**実データに近い regime** を作れる。
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--nx", "--traps", "--sims", "--pairs", "--rates",
            "--g0", "--dens", "--maxit", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE), c(.known, "--dry-run"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--dry-run"), collapse = " "))

DRY    <- "--dry-run" %in% .args
NX     <- as.integer(.opt("--nx", "14"))
NTRAP1 <- as.integer(.opt("--traps", "6"))
SIMS   <- as.integer(.opt("--sims", "3"))
PAIRS  <- as.integer(strsplit(.opt("--pairs", "4,12"), ",")[[1]])
RATES  <- as.numeric(strsplit(.opt("--rates", "1,0.4,0.15,0.05"), ",")[[1]])
G0     <- as.numeric(.opt("--g0", "-2.0"))
DENS   <- as.numeric(.opt("--dens", "3.6"))   # 高密度。個体数を稼ぐ
MAXIT  <- as.integer(.opt("--maxit", "300"))
OUT    <- .opt("--out", "results/identify_nind")
stopifnot(SIMS >= 1, all(PAIRS >= 1), all(RATES > 0 & RATES <= 1))

PAR_NAMES <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")
TRUE_CONN <- c(conn_0 = -1.0, conn_agri = 0.6, conn_wtr = -0.5)
TRUE_ADV  <- -0.5
CELL_SIZE <- 1; CELL_AREA <- 1; EFFORT <- 10; N_OCCASION <- 1
STEPSPERTIME <- 1; STEPAD <- 200; TIMEBURNIN <- 20

say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
say("== つながりを固定して個体数だけを振る ==")
say(sprintf("  格子 %d×%d = %d セル / 検出器 %d 基", NX, NX, NX * NX, NTRAP1^2))
say(sprintf("  密度 %.1f / g0 %.1f / シミュレーション %d 回", DENS, G0, SIMS))
say("  固定するつながり: ", paste(PAIRS, collapse = ", "), " 本")
say("  単回個体の残し率: ", paste(RATES, collapse = ", "))

suppressMessages(suppressWarnings(source("adcrsgd/secrad.r", encoding = "UTF-8")))

# ===========================================================================
# 1. 景観と検出器（固定）
# ===========================================================================
ncell  <- NX * NX
xcoord <- rep(seq_len(NX), each = NX) * CELL_SIZE
ycoord <- rep(seq_len(NX), times = NX) * CELL_SIZE
grid_cov <- data.frame(
  agri = as.numeric(scale(sin(xcoord / 3) + cos(ycoord / 4))),
  wtr  = as.numeric(scale(cos(xcoord / 5) * sin(ycoord / 3))))
tc <- round(seq(2, NX - 1, length.out = NTRAP1))
traps <- expand.grid(x = tc * CELL_SIZE, y = tc * CELL_SIZE)
ntrap <- nrow(traps)
effort_loc <- match(paste(traps$x, traps$y), paste(xcoord, ycoord))
stopifnot(!anyNA(effort_loc), !anyDuplicated(effort_loc))

MODEL <- list(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))
TRUTH <- setNames(c(DENS, unname(TRUE_CONN), G0), PAR_NAMES)

# ===========================================================================
# 2. 部品
# ===========================================================================
simulate_once <- function() {
  sd <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                        area = rep(CELL_AREA, ncell), grid_cov = grid_cov,
                        resolution = c(x = CELL_SIZE, y = CELL_SIZE))
  sd$add_obs(type = "poisson", effort = rep(EFFORT, ntrap),
             effort_loc = effort_loc, effort_occ = rep(N_OCCASION, ntrap))
  sd$set_truemodel(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 1),
                   indmodel = c(A = FALSE, g0 = FALSE),
                   occmodel = c(A = FALSE, g0 = FALSE),
                   dpar = DENS, cpar = unname(TRUE_CONN),
                   adpar = TRUE_ADV, g0par = G0)
  sd$set_sim(EFFORT, STEPSPERTIME, STEPAD, TIMEBURNIN)
  sd$generate_ind(); sd$simulate(verbose = FALSE); sd$update_detect()
  as.matrix(sd$obs[[1]]$detect)       # 行 = 検出器 / 列 = 個体
}

## 個体を検出器ごとに分解する（繋がりだけを壊す）。
## **総捕獲数も検出器ごとの捕獲数も変わらない**
split_cols <- function(d, j) {
  k <- which(d[, j] > 0)
  lapply(k, function(kk) { v <- numeric(nrow(d)); v[kk] <- d[kk, j]; v })
}

## 2基以上で捕まった個体（＝繋がりを生む個体）
movers_of <- function(d) which(colSums(d > 0) >= 2L)

## 個体の集合が作る「つながり」（検出器の組、順不同・重複除去）の数
pairs_of <- function(d, cols) {
  if (!length(cols)) return(0L)
  pr <- unlist(lapply(cols, function(j) {
    k <- sort(which(d[, j] > 0))
    if (length(k) < 2L) return(NULL)
    cb <- utils::combn(k, 2); paste(cb[1, ], cb[2, ], sep = "-")
  }), use.names = FALSE)
  length(unique(pr))
}

## ★ 中核。つながりを `target` 本に固定し、単回個体を `rate` で間引く
##   - 残す移動個体は**絶対に間引かない**
##   - 残さない移動個体は**分解する**（捕獲データは保持）
make_variant <- function(d, target, rate, seed) {
  set.seed(seed)
  mv <- movers_of(d)
  ## 繋がりが target 本に届くまで移動個体を1つずつ採る
  keep <- integer(0)
  for (j in sample(mv)) {
    keep <- c(keep, j)
    if (pairs_of(d, keep) >= target) break
  }
  np <- pairs_of(d, keep)
  others <- setdiff(seq_len(ncol(d)), keep)
  ## 残りは分解 → すべて1検出器の個体になる
  singles <- unlist(lapply(others, function(j) split_cols(d, j)), recursive = FALSE)
  ## 間引き（残す移動個体は対象外）
  nkeep <- max(0L, floor(length(singles) * rate))
  if (nkeep < length(singles)) singles <- singles[sort(sample.int(length(singles), nkeep))]
  cols <- c(lapply(keep, function(j) d[, j]), singles)
  list(detect = matrix(unlist(cols), nrow = nrow(d)), n_pair = np,
       n_mover = length(keep), n_single = length(singles))
}

make_est_obj <- function(detect) {
  sd <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                        area = rep(CELL_AREA, ncell), grid_cov = grid_cov,
                        resolution = c(x = CELL_SIZE, y = CELL_SIZE))
  sd$add_obs(type = "poisson", effort = rep(EFFORT, ntrap),
             effort_loc = effort_loc, effort_occ = rep(N_OCCASION, ntrap),
             detect = detect)
  ob <- secrad$new(secrdata = sd)
  ob$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
               occmodel = MODEL$occmodel)
  ob
}

## **真値から出発**。最適化の失敗を排除し、情報量だけを見る
fit_one <- function(ob) {
  r <- tryCatch(optim(TRUTH, ob$loglf, method = "BFGS",
                      control = list(maxit = MAXIT), loglfscale = -1,
                      hessian = TRUE), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  se <- tryCatch(sqrt(diag(solve(r$hessian))), error = function(e) NULL)
  if (is.null(se) || !all(is.finite(se))) se <- rep(NA_real_, length(PAR_NAMES))
  list(par = setNames(r$par, PAR_NAMES), se = setNames(se, PAR_NAMES),
       logL = -r$value, conv = r$convergence)
}

# ===========================================================================
# 3. 見積もり
# ===========================================================================
if (DRY) {
  say("")
  t0 <- Sys.time(); d <- simulate_once()
  ts <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  mv <- movers_of(d)
  say(sprintf("  生成 %.0f秒 / 個体 %d / 移動個体 %d / つながり %d",
              ts, ncol(d), length(mv), pairs_of(d, mv)))
  for (tg in PAIRS) for (rt in RATES) {
    v <- make_variant(d, tg, rt, 1)
    say(sprintf("    つながり狙い %2d / 残し率 %.2f → 個体 %5d（移動 %d / 単回 %d）/ 実現 %d 本",
                tg, rt, ncol(v$detect), v$n_mover, v$n_single, v$n_pair))
  }
  t0 <- Sys.time(); invisible(make_est_obj(d)$loglf(TRUTH, loglfscale = -1))
  tl <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  n_fit <- SIMS * length(PAIRS) * length(RATES)
  say("")
  say(sprintf("  loglf 1回 %.2f秒（個体 %d）", tl, ncol(d)))
  say(sprintf("★ 見込み: 生成 %d回 ＋ 当てはめ %d本 ＝ **%.0f 分**",
              SIMS, n_fit, (ts * SIMS + tl * 6 * 50 * n_fit) / 60))
  say("  ＊ 間引いた条件はもっと速いので、これは上限寄りの見積もり")
  quit(status = 0)
}

# ===========================================================================
# 4. 本番
# ===========================================================================
rows <- list()
for (s in seq_len(SIMS)) {
  say("")
  say(sprintf("-- シミュレーション %d / %d --", s, SIMS))
  d <- simulate_once()
  mv <- movers_of(d)
  say(sprintf("   個体 %d / 移動個体 %d / つながり %d（この中から選ぶ）",
              ncol(d), length(mv), pairs_of(d, mv)))
  if (pairs_of(d, mv) < max(PAIRS)) {
    say("   ⚠ つながりが足りない。--dens か --g0 を上げること。この回は飛ばす")
    next
  }

  for (tg in PAIRS) for (rt in RATES) {
    v <- make_variant(d, tg, rt, s * 1000 + tg * 10 + round(rt * 100))
    t0 <- Sys.time()
    f <- fit_one(make_est_obj(v$detect))
    sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (is.null(f)) { say("   当てはめ失敗。飛ばす"); next }

    r <- data.frame(sim = s, target_pair = tg, rate = rt,
                    n_pair = v$n_pair, nind = ncol(v$detect),
                    n_mover = v$n_mover, n_single = v$n_single,
                    n_record = sum(v$detect), conv = f$conv, logL = f$logL, sec = sec)
    for (p in PAR_NAMES) {
      r[[paste0("est_", p)]] <- unname(f$par[p])
      r[[paste0("se_",  p)]] <- unname(f$se[p])
      r[[paste0("err_", p)]] <- unname(f$par[p] - TRUTH[p])
    }
    rows[[length(rows) + 1L]] <- r
    say(sprintf("   つながり %2d / 個体 %5d : conn_agri %+.3f (SE %.3f) / conn_wtr %+.3f (SE %.3f) / %.0f秒",
                v$n_pair, ncol(v$detect), f$par["conn_agri"], f$se["conn_agri"],
                f$par["conn_wtr"], f$se["conn_wtr"], sec))
    write.csv(do.call(rbind, rows), paste0(OUT, ".csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  }
}

res <- do.call(rbind, rows)
if (is.null(res) || !nrow(res)) stop("結果が得られなかった")

# ===========================================================================
# 5. まとめ
# ===========================================================================
say("")
say(strrep("=", 74))
say("  ★ つながりを固定して個体数を振った結果")
say(strrep("=", 74))
say("  真値: conn_agri +0.60 / conn_wtr -0.50")
say("")
for (tg in sort(unique(res$target_pair))) {
  s <- res[res$target_pair == tg, , drop = FALSE]
  say(sprintf("  ── つながり %d 本に固定 ──", tg))
  say(sprintf("    %8s %8s %12s %12s %10s %10s",
              "個体", "記録", "agri SE", "wtr SE", "agri 誤差", "wtr 誤差"))
  for (rt in sort(unique(s$rate), decreasing = TRUE)) {
    x <- s[s$rate == rt, , drop = FALSE]
    say(sprintf("    %8.0f %8.0f %12.3f %12.3f %10.3f %10.3f",
                median(x$nind), median(x$n_record),
                median(x$se_conn_agri, na.rm = TRUE),
                median(x$se_conn_wtr,  na.rm = TRUE),
                median(abs(x$err_conn_agri)), median(abs(x$err_conn_wtr))))
  }
  ## 個体数を何倍にすると SE が何倍になるか
  x <- s[is.finite(s$se_conn_agri), , drop = FALSE]
  if (nrow(x) >= 4 && length(unique(x$nind)) >= 3) {
    b <- stats::coef(stats::lm(log(se_conn_agri) ~ log(nind), data = x))[2]
    say(sprintf("    → **SE ∝ 個体数^%.2f**（0 なら個体数は効かない / −0.5 なら √n の改善）", b))
  }
  say("")
}

say("  ★ 読み方")
say("    **つながりの本数は固定してある。** 違うのは個体数だけ。")
say("    指数が 0 に近ければ、**個体数を増やしても透過性の精度は上がらない**")
say("      → 関東（つながり4本・個体16,308）に投じる価値は小さい")
say("    指数が負に大きければ、**個体数が効く**")
say("      → 実データの 16,308 個体は、この実験の100倍。期待できる")
say("")
say("  ★ 実データ")
say("    関東   つながり  4本 / 個体 16,308 / SGD 300反復 9.6日（×3点出発 29日）")
say("    W800   つながり 11本 / 個体 23,507 / SGD 300反復 32.7日（×3点出発 98日）")

write.csv(res, paste0(OUT, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, ".csv")
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
