# examples/multistart_study.R ------------------------------------------------
#
# **単一の初期値からの最尤推定が、どのくらいの頻度で劣った峰に落ちるか。**
# 再捕獲の密度を振って、多数のデータセットで数える。
#
#   Rscript examples/multistart_study.R --max-hours 62
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-21）
#
# 2026-09-17 に、疎な再捕獲のデータで **BFGS の参照解が最大点でなかった**ことが
# 分かった（reports/20260916_bracket_report.md §8）。原因は最適化器ではなく
# **尤度面の多峰性**で、多点出発のうち2/3が別の（より良い）峰に着いた。
#
# **しかしこれは1データセットでの1例にすぎない。**
# 「疎な再捕獲では単一初期値の最尤推定は信頼できない」と言うには、
# **どのくらいの頻度で起きるのか**、**どのくらい疎だと起きるのか**が要る。
#
# 原著（Fukasawa & Higashide 2025）は "For all iterations, estimations of
# ADCR converged successfully" と書いている。ただしそのシミュレーションは
# 1個体あたり約10.9検出（疎シナリオでも1.66）で、**足環データの見込み
# （約1.09）よりはるかに密**。矛盾ではなく、**探索されていない領域**にあたる。
#
# この研究が埋めるのはそこ。
#
# ---------------------------------------------------------------------------
# 設計
#
#   再捕獲の密度を3水準（TRUE_G0 と TRUE_DENS で作る）
#     × 各水準でデータセットを多数生成
#     × 各データセットで BFGS を N_START 点から回す
#
#   記録するのは「遠方の既定初期値から出発した1本が、最良の峰を見つけたか」。
#   これが**利用者が実際にやること**だから。
#
#   ADAM_EVERY 個に1つ、Adam も1本回す（勝った設定 beta2=0.9 / alpha x1）。
#   SGD が同じ頻度で外すのかを見るため。rate = 1.0 なので間引きの機構は使わず、
#   全データの対数尤度に直接 Adam をかける（両者は rate=1.0 で同一）。
#
# ---------------------------------------------------------------------------
# 無人で数十時間走らせる前提の作り
#
#   - **1データセットごとに CSV へ追記する。** 途中で落ちてもそこまでは残る
#   - **各データセットを tryCatch で包む。** 1つ失敗しても止まらない
#   - **--max-hours で自分から止まる。** 放置しても暴走しない
#   - **再開できる。** 既存の CSV があれば読み、続きから番号を振る
# ---------------------------------------------------------------------------

## --- 引数 -------------------------------------------------------------------
.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.bad <- setdiff(grep("^--", .args, value = TRUE),
                c("--max-hours", "--n-start", "--adam-every", "--out", "--no-hessian"))
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

MAX_HOURS  <- as.numeric(.opt("--max-hours",  "62"))
N_START    <- as.integer(.opt("--n-start",    "5"))
ADAM_EVERY <- as.integer(.opt("--adam-every", "10"))
OUTFILE    <- .opt("--out", "results/multistart_study.csv")
DO_HESSIAN <- !("--no-hessian" %in% .args)
stopifnot(MAX_HOURS > 0, N_START >= 2L, ADAM_EVERY >= 1L)

SOURCEPATH <- "adcrsgd/secrad.r"
UTILPATH   <- "adcrsgd/sgd_utils.R"
PAR_NAMES  <- c("dens_0", "conn_0", "conn_agri", "conn_wtr", "g0_1")

## --- 再捕獲の密度の3水準 ----------------------------------------------------
# TRUE_G0 が検出率、TRUE_DENS が個体数。検出個体数が極端にならないよう
# 両方を動かす。実際の「1個体あたり検出数」はデータごとに記録するので、
# 解析では水準名ではなくその実測値を説明変数にすること。
LEVELS <- data.frame(
  level     = c("dense", "mid", "sparse"),
  true_g0   = c(-3.0,    -4.0,  -5.0),
  true_dens = c(-0.5,     0.75,  2.0),
  stringsAsFactors = FALSE)

## --- 固定の設定 -------------------------------------------------------------
NX <- 20; CELL_SIZE <- 1; CELL_AREA <- 1
TRAP_COORD <- c(3, 7, 11, 15, 18)
EFFORT <- 200; N_OCCASION <- 1
TRUE_CONN <- c(conn_0 = -1.0, conn_agri = 0.6, conn_wtr = -0.5)
TRUE_ADV  <- -0.5
STEPSPERTIME <- 1; STEPAD <- 200; TIMEBURNIN <- 20
INIT <- c(dens_0 = -1.0, conn_0 = -2.0, conn_agri = 0.0, conn_wtr = 0.0, g0_1 = -5.0)
START_SD    <- 0.5     # 追加の初期値の散らし幅
OPTIM_MAXIT <- 1000
LL_TOL      <- 0.01    # 同じ峰とみなす対数尤度の差
## Adam（勝った設定。reports/20260916_bracket_report.md §3.3）
ADAM_ITER  <- 400
ADAM_BETA2 <- 0.9
ALPHA <- c(dens_0 = 0.02, conn_0 = 0.03, conn_agri = 0.03, conn_wtr = 0.03, g0_1 = 0.02)
MAX_STEP <- c(dens_0 = 0.05, conn_0 = 0.10, conn_agri = 0.05, conn_wtr = 0.05, g0_1 = 0.05)
BETA1 <- 0.9; EPS_ADAM <- 1e-8; GRAD_EPS <- 1e-4

ncell  <- NX * NX
xcoord <- rep(seq_len(NX), each = NX) * CELL_SIZE
ycoord <- rep(seq_len(NX), times = NX) * CELL_SIZE
agri <- as.numeric(scale(sin(xcoord / 3) + cos(ycoord / 4)))
wtr  <- as.numeric(scale(cos(xcoord / 5) * sin(ycoord / 3)))
grid_cov <- data.frame(agri = agri, wtr = wtr)
traps <- expand.grid(x = TRAP_COORD * CELL_SIZE, y = TRAP_COORD * CELL_SIZE)
ntrap <- nrow(traps)
effort_loc <- match(paste(traps$x, traps$y), paste(xcoord, ycoord))
MODEL <- list(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

t_start <- Sys.time()
elapsed_h <- function() as.numeric(difftime(Sys.time(), t_start, units = "hours"))

cat("== 多点出発の系統的研究 ==\n")
cat(sprintf("  初期値 %d点 / Adam は %d データに1回 / 上限 %.1f時間\n",
            N_START, ADAM_EVERY, MAX_HOURS))
cat("  出力: ", OUTFILE, "（1データセットごとに追記）\n\n", sep = "")

cat("== 尤度エンジン ==\n")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))
source(UTILPATH, encoding = "UTF-8")

dir.create("results", showWarnings = FALSE)

## 再開: 既存の CSV があれば続きから
done <- 0L
if (file.exists(OUTFILE)) {
  prev <- tryCatch(read.csv(OUTFILE, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(prev) && nrow(prev)) {
    done <- nrow(prev)
    cat(sprintf("既存の %d 件に追記します\n\n", done))
  }
}

## --- 1データセット分 --------------------------------------------------------
one_dataset <- function(idx) {
  lv <- LEVELS[((idx - 1L) %% nrow(LEVELS)) + 1L, ]

  ## --- データ生成 -----------------------------------------------------------
  simdata <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                             area = rep(CELL_AREA, ncell), grid_cov = grid_cov,
                             resolution = c(x = CELL_SIZE, y = CELL_SIZE))
  simdata$add_obs(type = "poisson", effort = rep(EFFORT, ntrap),
                  effort_loc = effort_loc, effort_occ = rep(N_OCCASION, ntrap))
  simdata$set_truemodel(envmodel = list(D ~ 1, C ~ agri + wtr, A ~ 1),
                        indmodel = c(A = FALSE, g0 = FALSE),
                        occmodel = c(A = FALSE, g0 = FALSE),
                        dpar = lv$true_dens, cpar = unname(TRUE_CONN),
                        adpar = TRUE_ADV, g0par = lv$true_g0)
  simdata$set_sim(EFFORT, STEPSPERTIME, STEPAD, TIMEBURNIN)
  simdata$generate_ind()
  simdata$simulate(verbose = FALSE)
  simdata$update_detect()

  detect <- simdata$obs[[1]]$detect
  cnt    <- colSums(detect)
  n_det  <- simdata$nind
  n_mult <- sum(cnt > 1); n_sing <- sum(cnt == 1)
  if (n_mult < 2) stop("複数回検出が少なすぎます")

  obj <- secrad$new(secrdata = simdata)
  obj$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
                occmodel = MODEL$occmodel)

  ## --- BFGS を N_START 点から -----------------------------------------------
  ## 1点目は**必ず既定の遠方初期値**。これが利用者が実際にやること。
  starts <- list(INIT)
  for (k in seq_len(N_START - 1L)) {
    s <- INIT + rnorm(length(PAR_NAMES), 0, START_SD); names(s) <- PAR_NAMES
    starts[[k + 1L]] <- s
  }

  fits <- lapply(starts, function(s)
    tryCatch(optim(s, obj$loglf, method = "BFGS",
                   control = list(maxit = OPTIM_MAXIT), loglfscale = -1),
             error = function(e) NULL))
  ok <- !sapply(fits, is.null)
  if (!any(ok)) stop("BFGS が全点で失敗")

  lls <- sapply(fits, function(f) if (is.null(f)) NA_real_ else -f$value)
  best_i   <- which.max(lls)
  best_ll  <- lls[best_i]
  best_par <- setNames(fits[[best_i]]$par, PAR_NAMES)
  far_ll   <- lls[1]                      # 既定の遠方初期値からの結果
  far_gap  <- best_ll - far_ll            # 正なら既定初期値は最良を逃した

  ## 峰の数: 対数尤度を LL_TOL で丸めて数える
  n_peak <- length(unique(round(lls[ok] / LL_TOL)))

  ## --- 最良点での標準誤差（面の平坦さの指標）--------------------------------
  max_se <- NA_real_
  if (DO_HESSIAN) {
    h <- tryCatch(optim(best_par, obj$loglf, method = "BFGS",
                        control = list(maxit = 1), loglfscale = -1, hessian = TRUE),
                  error = function(e) NULL)
    if (!is.null(h) && !is.null(h$hessian)) {
      se <- tryCatch(sqrt(diag(solve(h$hessian))), error = function(e) NULL)
      if (!is.null(se) && all(is.finite(se))) max_se <- max(se)
    }
  }

  ## --- Adam（ADAM_EVERY 個に1回）-------------------------------------------
  ## rate = 1.0 なので間引きの機構は要らない。全データの対数尤度に直接かける。
  adam_ll <- NA_real_; adam_gap <- NA_real_
  if (idx %% ADAM_EVERY == 0L) {
    adcache <- share_advdiff_cache(list(obj))
    objfun <- function(p) { names(p) <- PAR_NAMES; obj$loglf(p, loglfscale = 1) }
    cur <- INIT; m <- v <- setNames(rep(0, length(PAR_NAMES)), PAR_NAMES)
    okA <- TRUE
    for (it in seq_len(ADAM_ITER)) {
      g <- tryCatch(grad_cachewise(func = objfun, x = cur, eps = GRAD_EPS),
                    error = function(e) rep(NA_real_, length(PAR_NAMES)))
      if (any(!is.finite(g))) { okA <- FALSE; break }
      names(g) <- PAR_NAMES
      m <- BETA1 * m + (1 - BETA1) * g
      v <- ADAM_BETA2 * v + (1 - ADAM_BETA2) * g^2
      st <- ALPHA * (m / (1 - BETA1^it)) / (sqrt(v / (1 - ADAM_BETA2^it)) + EPS_ADAM)
      cur <- cur + pmax(pmin(st, MAX_STEP), -MAX_STEP)
    }
    if (okA) {
      adam_ll  <- tryCatch(objfun(cur), error = function(e) NA_real_)
      adam_gap <- best_ll - adam_ll
    }
  }

  data.frame(
    idx = idx, level = lv$level, true_g0 = lv$true_g0, true_dens = lv$true_dens,
    n_true = nrow(simdata$trueind), n_detected = n_det,
    n_multi = n_mult, n_single = n_sing,
    det_per_ind = sum(detect) / max(1L, n_det),
    n_start_ok = sum(ok), n_peak = n_peak,
    best_ll = best_ll, far_ll = far_ll, far_gap = far_gap,
    far_is_best = far_gap < LL_TOL,
    max_se = max_se,
    adam_ll = adam_ll, adam_gap = adam_gap,
    best_dens_0 = best_par[["dens_0"]], best_conn_0 = best_par[["conn_0"]],
    best_conn_agri = best_par[["conn_agri"]], best_conn_wtr = best_par[["conn_wtr"]],
    best_g0_1 = best_par[["g0_1"]],
    stringsAsFactors = FALSE)
}

## --- 本体 -------------------------------------------------------------------
idx <- done
n_fail <- 0L
repeat {
  if (elapsed_h() > MAX_HOURS) { cat("\n時間の上限に達したので終了します。\n"); break }
  idx <- idx + 1L
  t0 <- Sys.time()
  row <- tryCatch(one_dataset(idx), error = function(e) {
    cat(sprintf("[%4d] 失敗: %s\n", idx, conditionMessage(e))); NULL })
  if (is.null(row)) {
    n_fail <- n_fail + 1L
    if (n_fail >= 20L) { cat("失敗が20件に達したので終了します。\n"); break }
    next
  }
  row$mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  write.table(row, OUTFILE, sep = ",", row.names = FALSE,
              col.names = !file.exists(OUTFILE) || file.size(OUTFILE) == 0,
              append = file.exists(OUTFILE) && file.size(OUTFILE) > 0)
  cat(sprintf("[%4d] %-6s 検出 %4d（複数回 %3d）/個体 %.2f  峰 %d  既定初期値の遅れ %7.3f%s  最大SE %6.2f  %.1f分  経過 %.1fh\n",
              idx, row$level, row$n_detected, row$n_multi, row$det_per_ind,
              row$n_peak, row$far_gap, if (row$far_is_best) "" else " ★",
              row$max_se, row$mins, elapsed_h()))
}

cat(sprintf("\n完了: %d 件（失敗 %d 件）/ %.1f 時間\n", idx - done - n_fail, n_fail, elapsed_h()))
cat("解析の例:\n")
cat("  d <- read.csv(\"", OUTFILE, "\")\n", sep = "")
cat("  aggregate(cbind(far_is_best, n_peak, max_se) ~ level, d, mean)\n")
cat("  plot(d$det_per_ind, d$far_gap, log = \"y\")\n")
