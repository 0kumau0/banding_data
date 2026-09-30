# examples/identify_conn.R ---------------------------------------------------
#
# **「移動の観測が何本あれば透過性の係数が推定できるのか」を合成データで測る。**
#
#   Rscript examples/identify_conn.R --dry-run        # まず所要時間を見積もる
#   Rscript examples/identify_conn.R --reps 6
#   Rscript examples/identify_conn.R --nx 14 --reps 4 --g0 -4.5,-4,-3.5,-3,-2.5
#
# **実データは要らない。** このファイル1つで完結する（依存は adcrsgd/secrad.r）。
# 書き出すのは results/identify_conn*.csv だけ。
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-29）
#
# 足環データで透過性（`conn_0` / `conn_agri` / `conn_wtr` の3係数）を推定したいが、
# **その情報源は「別のメッシュで再捕獲された個体」だけ**で、実測は
#
#   シジュウカラ・全国 … 移動 17 件 / 独立なセルの組 14 本
#   シジュウカラ・関東 … 移動  5 件 / 独立なセルの組  4 本
#   ウグイス  ・全国 … 移動 26 件 / 独立なセルの組 16 本
#
# （`reports/20260929_moves_4species.md`、`reports/20260929_kanto_dataset.md`）
#
# **係数3つに対して観測が4〜16本。** 計算資源をどう手当てしても、
# そもそも推定できないなら意味がない。**真値が分かっている合成データで先に測る。**
#
# ---------------------------------------------------------------------------
# ★ 設計の考え方
#
# **これは最適化の検証ではない。** `examples/sgd_simulation.R` /
# `sgd_sim_conditions.R` は「Adam が BFGS 解に届くか」を見る台で、
# ここで問うのは**尤度に情報があるか**。最適化器の問題と混ぜてはいけない。
# したがって:
#
#   - **SGD は使わない。** BFGS だけ。速いし、問いに対して素直
#   - **真値から出発した当てはめを必ず1本入れる。** そこで得られる標準誤差が
#     「最適化の失敗を完全に排除したときの、データが持つ情報量の上限」。
#     ここが広ければ、どんな最適化器を使っても推定できない
#   - **遠い初期値からも当てはめる。** 真値出発と同じ対数尤度に着くかで、
#     「情報はあるが実務上たどり着けない」を切り分ける（多峰性。
#     `reports/20260924_multistart_report.md`）
#
# **`n_pair` を狙って作ることはしない。** 移動の本数は確率的に決まるので、
# 検出率 `g0` を振って**実現した `n_pair` を測り、それと推定誤差の関係を見る**。
# 「4本ちょうど」を人為的に作るより、この方が正直で情報量も多い。
# ---------------------------------------------------------------------------

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--nx", "--traps", "--reps", "--g0", "--dens", "--maxit", "--out")
.bad <- setdiff(grep("^--", .args, value = TRUE),
                c(.known, "--dry-run", "--calibrate", "--far", "--ablate"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "),
                       "\n使えるのは: ", paste(c(.known, "--dry-run"), collapse = " "))

DRY     <- "--dry-run" %in% .args
CALIB   <- "--calibrate" %in% .args   # 生成だけして n_pair の出方を見る（当てはめ無し）
## 遠方初期値からの当てはめ。**既定では行わない**（所要時間が2倍になる）。
## 主問題は「尤度に情報があるか」で、それは真値出発の当てはめで測れる。
## 多峰性（情報はあるが到達できない）は別問題なので、必要なときに --far で足す
DO_FAR  <- "--far" %in% .args
## 対照実験: 同じデータから「つながりだけ」を壊して当てはめ直す（下の ablate_detect）
ABLATE  <- "--ablate" %in% .args
NX      <- as.integer(.opt("--nx", "16"))          # 格子の1辺。ncell = NX^2
NTRAP1  <- as.integer(.opt("--traps", "6"))        # 検出器は NTRAP1^2 基
REPS    <- as.integer(.opt("--reps", "6"))         # 各水準の繰り返し
G0_SET  <- as.numeric(strsplit(.opt("--g0", "-4.5,-4,-3.5,-3,-2.5"), ",")[[1]])
DENS    <- as.numeric(.opt("--dens", "0.3"))       # log 密度（個体数を決める）
MAXIT   <- as.integer(.opt("--maxit", "300"))
OUT     <- .opt("--out", "results/identify_conn")
stopifnot(NX >= 8, NTRAP1 >= 3, REPS >= 1, MAXIT >= 10)

## --- 真値。**推定できるかを問うので、真値は「はっきりした効果」にする** ------
## ここを弱くすると「推定できない」が真値のせいなのか観測数のせいなのか
## 分からなくなる。まず強い効果で下限を測る
PAR_NAMES <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")
TRUE_CONN <- c(conn_0 = -1.0, conn_agri = 0.6, conn_wtr = -0.5)
TRUE_ADV  <- -0.5
INIT_FAR  <- c(dens_0 = -1.0, conn_0 = -2.0, conn_agri = 0.0,
               conn_wtr = 0.0, g0_1 = -5.0)

CELL_SIZE <- 1; CELL_AREA <- 1; EFFORT <- 10; N_OCCASION <- 1
STEPSPERTIME <- 1; STEPAD <- 200; TIMEBURNIN <- 20

say <- function(...) { cat(..., "\n", sep = ""); flush(stdout()) }
say("== 透過性の係数が推定できる観測数の下限を測る ==")
say(sprintf("  格子 %d×%d = %d セル / 検出器 %d 基 / 繰り返し %d",
            NX, NX, NX * NX, NTRAP1^2, REPS))
say("  g0 の水準: ", paste(G0_SET, collapse = ", "))
say(sprintf("  真値: conn_0 %.2f / conn_agri %.2f / conn_wtr %.2f",
            TRUE_CONN[1], TRUE_CONN[2], TRUE_CONN[3]))

suppressMessages(suppressWarnings(source("adcrsgd/secrad.r", encoding = "UTF-8")))

# ===========================================================================
# 1. 景観と検出器（全条件で固定）
# ===========================================================================
ncell  <- NX * NX
xcoord <- rep(seq_len(NX), each = NX) * CELL_SIZE
ycoord <- rep(seq_len(NX), times = NX) * CELL_SIZE
agri <- as.numeric(scale(sin(xcoord / 3) + cos(ycoord / 4)))
wtr  <- as.numeric(scale(cos(xcoord / 5) * sin(ycoord / 3)))
grid_cov <- data.frame(agri = agri, wtr = wtr)

## 検出器を格子状に置く。**各検出器は別のセル**なので、
## 「2基以上で捕まった」＝「2セル以上で捕まった」になる（実データの n_moved と同じ定義）
tc <- round(seq(2, NX - 1, length.out = NTRAP1))
traps <- expand.grid(x = tc * CELL_SIZE, y = tc * CELL_SIZE)
ntrap <- nrow(traps)
effort_loc <- match(paste(traps$x, traps$y), paste(xcoord, ycoord))
stopifnot(!anyNA(effort_loc), !anyDuplicated(effort_loc))

MODEL <- list(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

# ===========================================================================
# 2. 部品
# ===========================================================================

## 1回ぶんのデータ生成。検出行列（行=検出器 / 列=個体）を返す
simulate_once <- function(g0) {
  sd <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                        area = rep(CELL_AREA, ncell),
                        grid_cov = grid_cov,
                        resolution = c(x = CELL_SIZE, y = CELL_SIZE))
  sd$add_obs(type = "poisson", effort = rep(EFFORT, ntrap),
             effort_loc = effort_loc, effort_occ = rep(N_OCCASION, ntrap))
  sd$set_truemodel(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 1),
                   indmodel = c(A = FALSE, g0 = FALSE),
                   occmodel = c(A = FALSE, g0 = FALSE),
                   dpar = DENS, cpar = unname(TRUE_CONN),
                   adpar = TRUE_ADV, g0par = g0)
  sd$set_sim(EFFORT, STEPSPERTIME, STEPAD, TIMEBURNIN)
  sd$generate_ind(); sd$simulate(verbose = FALSE); sd$update_detect()
  sd
}

## 移動の数え方。**実データ（examples/band_moves.R）と同じ定義**にそろえる
##   n_moved = 2基以上で捕獲された個体
##   n_pair  = その個体が結んだ検出器の組（順不同・重複除去）の数
count_moves <- function(detect) {
  d <- as.matrix(detect)
  d[d < 0 | is.na(d)] <- 0
  hit <- d > 0                                   # 行=検出器 / 列=個体
  ncap_loc <- colSums(hit)
  moved <- which(ncap_loc >= 2L)
  pairs <- character(0)
  for (j in moved) {
    w <- which(hit[, j])
    cb <- utils::combn(sort(w), 2)
    pairs <- c(pairs, paste(cb[1, ], cb[2, ], sep = "-"))
  }
  list(n_moved = length(moved), n_pair = length(unique(pairs)),
       n_ind = ncol(d), n_multi = sum(colSums(d) > 1), n_rec = sum(d))
}

## 推定用のオブジェクトを検出行列から組む。
## **full 版と ablate 版で同じ手順を通す**ため関数にしておく
## （片方だけ simulate 由来のオブジェクトを使うと比較にならない）
make_est_obj <- function(detect) {
  sd <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                        area = rep(CELL_AREA, ncell),
                        grid_cov = grid_cov,
                        resolution = c(x = CELL_SIZE, y = CELL_SIZE))
  sd$add_obs(type = "poisson", effort = rep(EFFORT, ntrap),
             effort_loc = effort_loc, effort_occ = rep(N_OCCASION, ntrap),
             detect = as.matrix(detect))
  ob <- secrad$new(secrdata = sd)
  ob$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
               occmodel = MODEL$occmodel)
  ob
}

## ★★ **つながりだけを壊す**（対照実験）。
##
## 2026-09-29 のユーザの指摘: 「移動がない再捕獲が何度も検出されることにも
## 生態学的な意味がある。その例が多いことは計算の助けにならないのか」
##
## 尤度は捕獲履歴の**全体**（捕まらなかった検出器の 0 も含む）を使うので、
## 「同じ検出器で5回・隣では0回」も行動圏が小さいことの証拠になる。
## **同じ場所の再捕獲も情報を持っている。**
##
## ただし `g0` を振る走査では「同じ場所の再捕獲数」と「移動の本数」が
## **同時に増える**ので、どちらが効いたか分離できない。
##
## そこで**同じデータから、つながりだけを壊す**:
##
##   2基以上で捕まった個体を、**検出器ごとに別個体に分解する**
##   例: 個体 #7 が A で1回・B で1回 → 「A で1回の個体」と「B で1回の個体」
##
## **総捕獲数も、検出器ごとの捕獲数も、空間パターンも変わらない。**
## 消えるのは「同一個体である」という繋がりだけ。
## 標準誤差の差が、そのまま**移動の情報の価値**になる。
##
## ⚠ 分解するぶん検出個体数は増える（移動個体の数だけ）。
##   これは「個体識別ができなかったら何が見えるか」という対照そのものなので
##   意図した挙動だが、密度の項に効くことは承知しておく
ablate_detect <- function(m) {
  d <- as.matrix(m)
  out <- vector("list", 0)
  for (j in seq_len(ncol(d))) {
    k <- which(d[, j] > 0)
    if (length(k) <= 1L) { out[[length(out) + 1L]] <- d[, j]; next }
    for (kk in k) {
      v <- numeric(nrow(d)); v[kk] <- d[kk, j]
      out[[length(out) + 1L]] <- v
    }
  }
  matrix(unlist(out), nrow = nrow(d))
}

## 当てはめ。**loglfscale = -1 で optim に最小化させる**
fit_one <- function(obj, init, hessian = FALSE) {
  r <- tryCatch(optim(init, obj$loglf, method = "BFGS",
                      control = list(maxit = MAXIT),
                      loglfscale = -1, hessian = hessian),
                error = function(e) NULL)
  if (is.null(r)) return(NULL)
  se <- rep(NA_real_, length(PAR_NAMES))
  if (hessian) {
    ## loglfscale = -1 で最小化したので hessian は −logL のヘッセ（＝観測情報行列）。
    ## その逆行列の対角の平方根が標準誤差
    s <- tryCatch(sqrt(diag(solve(r$hessian))), error = function(e) NULL)
    if (!is.null(s) && all(is.finite(s))) se <- s
  }
  list(par = setNames(r$par, PAR_NAMES), logL = -r$value,
       conv = r$convergence, se = setNames(se, PAR_NAMES))
}

# ===========================================================================
# 3. 所要時間の見積もり（--dry-run）
# ===========================================================================
## --- 校正モード: 生成だけして n_pair の出方を見る ---------------------------
## **当てはめの前に必ずこれを通す。** 1時間の計算を流してから
## 「移動が0件でした」と分かるのは最悪の順序（2026-09-29 に一度やった）
if (CALIB) {
  say("")
  say("-- 校正: 各 g0 で生成だけして、つながりが何本出るかを見る --")
  say(sprintf("   %-6s %-5s %8s %8s %8s %8s %8s", "g0", "rep",
              "個体", "複数回", "検出", "移動", "つながり"))
  out <- list()
  for (g0 in G0_SET) for (r in seq_len(max(2L, REPS))) {
    sd <- simulate_once(g0); mv <- count_moves(sd$obs[[1]]$detect)
    say(sprintf("   %-6.1f %-5d %8d %8d %8d %8d %8d",
                g0, r, mv$n_ind, mv$n_multi, mv$n_rec, mv$n_moved, mv$n_pair))
    out[[length(out) + 1L]] <- data.frame(g0 = g0, rep = r, n_ind = mv$n_ind,
      n_multi = mv$n_multi, n_rec = mv$n_rec, n_moved = mv$n_moved, n_pair = mv$n_pair)
  }
  o <- do.call(rbind, out)
  say("")
  say("   g0 ごとの つながり の中央値:")
  for (g0 in G0_SET)
    say(sprintf("     %-6.1f → %4.0f 本（個体 %.0f）", g0,
                median(o$n_pair[o$g0 == g0]), median(o$n_ind[o$g0 == g0])))
  say("")
  say("   ★ 実データは 4 / 14 / 16 本。**その範囲をまたぐ水準を選ぶこと**")
  say("   本番は --calibrate を外し、--g0 で選んだ水準を渡す")
  quit(status = 0)
}

if (DRY) {
  say("")
  say("-- 試しに1回だけ生成して loglf を測る --")
  t0 <- Sys.time()
  sd <- simulate_once(median(G0_SET))
  t_sim <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  mv <- count_moves(sd$obs[[1]]$detect)
  say(sprintf("  生成 %.1f秒 / 個体 %d（複数回 %d）/ 移動 %d / つながり %d",
              t_sim, mv$n_ind, mv$n_multi, mv$n_moved, mv$n_pair))

  ob <- secrad$new(secrdata = sd)
  ob$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
               occmodel = MODEL$occmodel)
  tp <- c(DENS, unname(TRUE_CONN), median(G0_SET))
  t0 <- Sys.time(); invisible(ob$loglf(tp, loglfscale = -1))
  t_ll <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  say(sprintf("  loglf 1回 %.2f秒", t_ll))

  ## BFGS 1本 = 反復 × (1 + 5回の数値勾配)。実測では 40〜80 反復
  n_fit <- length(G0_SET) * REPS * (if (DO_FAR) 2 else 1)
  per   <- t_ll * 6 * 60                      # 60反復ぶんの見積もり
  hess  <- t_ll * 25 * length(G0_SET) * REPS  # ヘッセは真値出発のぶんだけ
  tot   <- (t_sim * length(G0_SET) * REPS) + (per * n_fit) + hess
  say("")
  say(sprintf("★ 全体の見積もり: 生成 %d回 ＋ 当てはめ %d本 ＝ **%.1f 分**",
              length(G0_SET) * REPS, n_fit, tot / 60))
  say("   （BFGS 1本を60反復と仮定。実際は40〜80で振れる）")
  if (tot > 3600) say("   ⚠ 1時間超。--nx を下げるか --reps を減らすこと")
  say("")
  say("  --dry-run なのでここで終了。本番は --dry-run を外す")
  quit(status = 0)
}

# ===========================================================================
# 4. 本番
# ===========================================================================
rows <- list()
for (g0 in G0_SET) {
  for (rep_i in seq_len(REPS)) {
    t_rep <- Sys.time()
    sd <- simulate_once(g0)
    det0 <- sd$obs[[1]]$detect
    mv <- count_moves(det0)

    ob <- make_est_obj(det0)
    truth <- setNames(c(DENS, unname(TRUE_CONN), g0), PAR_NAMES)
    ## (a) **真値から出発**。最適化の失敗を排除したときの情報量の上限
    fa <- fit_one(ob, truth, hessian = TRUE)
    ## (b) 遠方から出発。実務で到達できるか（--far のときだけ）
    fb <- if (DO_FAR) fit_one(ob, INIT_FAR, hessian = FALSE) else NULL

    if (is.null(fa)) { say("  当てはめ失敗（真値出発）。飛ばす"); next }

    r <- data.frame(
      g0_true = g0, rep = rep_i,
      n_ind = mv$n_ind, n_multi = mv$n_multi, n_rec = mv$n_rec,
      n_moved = mv$n_moved, n_pair = mv$n_pair,
      conv_a = fa$conv, logL_a = fa$logL,
      conv_b = if (is.null(fb)) NA_integer_ else fb$conv,
      logL_b = if (is.null(fb)) NA_real_ else fb$logL,
      ## 同じ峰に着いたか（遠方出発が真値出発より 0.01 以上低ければ別の峰）
      same_peak = if (is.null(fb)) NA else (fb$logL >= fa$logL - 0.01),
      sec = as.numeric(difftime(Sys.time(), t_rep, units = "secs")))
    for (p in PAR_NAMES) {
      r[[paste0("est_", p)]] <- unname(fa$par[p])
      r[[paste0("se_",  p)]] <- unname(fa$se[p])
      r[[paste0("err_", p)]] <- unname(fa$par[p] - truth[p])
    }

    ## --- 対照: つながりだけを壊した同じデータ -------------------------------
    r$abl_nind <- NA_integer_
    for (p in PAR_NAMES) { r[[paste0("abl_se_", p)]] <- NA_real_
                           r[[paste0("abl_est_", p)]] <- NA_real_ }
    if (ABLATE) {
      det1 <- ablate_detect(det0)
      ob1  <- make_est_obj(det1)
      fc   <- fit_one(ob1, truth, hessian = TRUE)
      if (!is.null(fc)) {
        r$abl_nind <- ncol(det1)
        for (p in PAR_NAMES) {
          r[[paste0("abl_se_",  p)]] <- unname(fc$se[p])
          r[[paste0("abl_est_", p)]] <- unname(fc$par[p])
        }
        say(sprintf("      [対照] つながりを壊すと 個体 %d→%d / conn_agri SE %s→%s / conn_wtr SE %s→%s",
                    ncol(det0), ncol(det1),
                    sprintf("%.3f", fa$se["conn_agri"]), sprintf("%.3f", fc$se["conn_agri"]),
                    sprintf("%.3f", fa$se["conn_wtr"]),  sprintf("%.3f", fc$se["conn_wtr"])))
      }
    }

    rows[[length(rows) + 1L]] <- r

    say(sprintf("  g0 %.1f rep%d: 個体 %4d / 移動 %3d / **つながり %3d** / %.0f秒",
                g0, rep_i, mv$n_ind, mv$n_moved, mv$n_pair, r$sec))
    say(sprintf("      conn_agri %+.3f (真 %+.2f, SE %s) / conn_wtr %+.3f (真 %+.2f, SE %s)%s",
                fa$par["conn_agri"], TRUE_CONN["conn_agri"],
                if (is.na(fa$se["conn_agri"])) "NA" else sprintf("%.3f", fa$se["conn_agri"]),
                fa$par["conn_wtr"], TRUE_CONN["conn_wtr"],
                if (is.na(fa$se["conn_wtr"])) "NA" else sprintf("%.3f", fa$se["conn_wtr"]),
                if (isFALSE(r$same_peak)) "  ⚠ 遠方出発は別の峰" else ""))

    ## 途中で止まっても結果が残るように、毎回書き出す
    write.csv(do.call(rbind, rows), paste0(OUT, ".csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  }
}

res <- do.call(rbind, rows)
if (is.null(res) || !nrow(res)) stop("結果が1件も得られなかった")

# ===========================================================================
# 5. まとめ
# ===========================================================================
say("")
say(strrep("=", 74))
say("  ★ つながりの本数と推定の質")
say(strrep("=", 74))

## つながりの本数で層に分ける。**実データの値（4 / 14 / 16）を含む区切り**
res$bin <- cut(res$n_pair, breaks = c(-1, 5, 10, 20, 40, Inf),
               labels = c("〜5", "6-10", "11-20", "21-40", "41-"))

summ <- do.call(rbind, lapply(split(res, res$bin), function(s) {
  if (!nrow(s)) return(NULL)
  ok <- function(p, tv) mean(sign(s[[paste0("est_", p)]]) == sign(tv))
  data.frame(
    つながり = as.character(s$bin[1]), n = nrow(s),
    n_pair中央 = median(s$n_pair), 個体中央 = median(s$n_ind),
    agri_誤差中央 = median(abs(s$err_conn_agri)),
    agri_SE中央   = median(s$se_conn_agri, na.rm = TRUE),
    agri_符号一致 = ok("conn_agri", TRUE_CONN["conn_agri"]),
    wtr_誤差中央  = median(abs(s$err_conn_wtr)),
    wtr_SE中央    = median(s$se_conn_wtr, na.rm = TRUE),
    wtr_符号一致  = ok("conn_wtr", TRUE_CONN["conn_wtr"]),
    同じ峰 = mean(s$same_peak, na.rm = TRUE))
}))
print(format(summ, digits = 3), row.names = FALSE)

say("")
say("  読み方:")
say("    誤差中央 … |推定値 − 真値| の中央値。**真値から出発した当てはめ**なので、")
say("               最適化の失敗は入っていない。**純粋にデータの情報量**")
say("    SE中央   … ヘッセ行列から出した標準誤差。真値(0.6 / -0.5)と比べる。")
say("               **SE が真値と同程度なら、その係数は推定できていない**")
say("    符号一致 … 効果の向きだけでも当てられた割合。1.0 でなければ")
say("               「農地が透過性を上げるか下げるか」すら言えない")
say("    同じ峰   … 遠方初期値が真値出発と同じ対数尤度に着いた割合。")
say("               低ければ**情報はあっても実務では多点出発が要る**")

## --- 対照実験のまとめ -------------------------------------------------------
if (ABLATE && any(is.finite(res$abl_se_conn_agri))) {
  say("")
  say(strrep("=", 74))
  say("  ★ 対照実験 — つながりだけを壊すと標準誤差はどうなるか")
  say(strrep("=", 74))
  say("  **総捕獲数も、検出器ごとの捕獲数も、空間パターンも同じ。**")
  say("  消えるのは「同一個体である」という繋がりだけ。")
  say("")
  s <- res[is.finite(res$abl_se_conn_agri), , drop = FALSE]
  say(sprintf("  %8s %6s %10s %10s %8s %10s %10s %8s",
              "つながり", "個体", "agri SE", "壊した後", "倍率", "wtr SE", "壊した後", "倍率"))
  o <- order(s$n_pair)
  for (i in o)
    say(sprintf("  %8d %6d %10.3f %10.3f %8.2f %10.3f %10.3f %8.2f",
                s$n_pair[i], s$n_ind[i],
                s$se_conn_agri[i], s$abl_se_conn_agri[i],
                s$abl_se_conn_agri[i] / s$se_conn_agri[i],
                s$se_conn_wtr[i], s$abl_se_conn_wtr[i],
                s$abl_se_conn_wtr[i] / s$se_conn_wtr[i]))
  ra <- stats::median(s$abl_se_conn_agri / s$se_conn_agri)
  rw <- stats::median(s$abl_se_conn_wtr  / s$se_conn_wtr)
  say("")
  say(sprintf("  **SE の倍率（中央値）: conn_agri %.2f 倍 / conn_wtr %.2f 倍**", ra, rw))
  say("")
  say("  ★ 読み方")
  say("    倍率が大きい … **移動の情報が透過性を支えている。**")
  say("                    同じ場所の再捕獲だけでは代わりにならない")
  say("    倍率が 1 に近い … **同じ場所の再捕獲が大半を担っている。**")
  say("                    移動の本数にこだわる前提そのものを見直す必要がある")
}

say("")
say("  ★ 実データの位置")
say("    シジュウカラ・関東 つながり **4 本**")
say("    シジュウカラ・全国 つながり **14 本**")
say("    ウグイス  ・全国 つながり **16 本**")

write.csv(res, paste0(OUT, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summ, paste0(OUT, "_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
say("")
say("  保存: ", OUT, ".csv / ", OUT, "_summary.csv")
say(sprintf("  所要 %.1f 分", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
