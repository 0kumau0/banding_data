# examples/nind_scaling.R ----------------------------------------------------
#
# `loglf` のコストは個体数にどう効くか。合成データで実測する。
#
#   Rscript examples/nind_scaling.R
#   Rscript examples/nind_scaling.R --nx 20 --nind 300,1200,4800,19200 --rep 5
#
# 実データ不要。読み書きするのは results/nind_scaling.csv だけ。
#
# ---------------------------------------------------------------------------
# なぜ測るのか（2026-09-14）
#
# CLAUDE.md には「loglf のコストは個体数ではなく ncell で決まる。SGD 化は
# 速度対策にならない」とある。しかしこれは**クマ規模（ncell 8497 / 個体109、
# ncell >> nind）で取った測定**で、足環データには外挿できない。
# 個体依存の配列は ncell x nind なので、nind が大きくなればそちらが支配的に
# なりうる。そのとき sampling_rate は直接効く。
#
# 足環データで BFGS が落ちるために SGD を選んだ、というのがこの track の前提。
# **その SGD が実際のボトルネックに効くのかどうか**を、ここで確かめる。
#
# ---------------------------------------------------------------------------
# 測り方
#
# 1. secrad は advdiff の結果を self$adtemp にキャッシュする。同じ par で
#    loglf を2回呼ぶと、2回目はキャッシュに当たる。
#
#      cold = 初回（キャッシュなし）の総コスト  ← 実際に効くのはこちら
#      warm = キャッシュが当たったときのコスト
#
#    ⚠ 当初これを「cold - warm = advdiff（ncell 由来）、warm = 個体部（nind 由来）」
#    と解釈したが、**誤り**。ncell を固定したまま nind を増やすと cold - warm が
#    0.83秒 → 1249秒 まで伸びた（2026-09-14 の実測）。キャッシュ後にスキップされる
#    処理の中に、個体数に強く依存する重い部分がある。
#    **cold - warm を「advdiff 由来」と読んではいけない。**
#    信用できるのは cold と warm の実測値そのもの。
#
# 2. 個体数を増やすのにシミュレーションを回すと重い（1649個体で58秒）。
#    **検出行列の列を複製して nind だけを増やす。** 個体は重複するので
#    尤度の値には意味がないが、**計算量の測定としては等価**
#    （loglf は個体ごとに同じ処理をするだけで、重複を検知しない）。
#
# 3. 初回の loglf にはウォームアップのぶんが乗るので、捨ててから測る。
#    warm は分解能に対して短いので --rep 回繰り返して中央値を取る。
# ---------------------------------------------------------------------------

SOURCEPATH <- "adcrsgd/secrad.r"
OUT_CSV    <- "results/nind_scaling.csv"

## --- 引数 -------------------------------------------------------------------
.args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default = NULL) {
  i <- match(flag, .args)
  if (is.na(i) || i == length(.args)) default else .args[i + 1L]
}
.known <- c("--nx", "--nind", "--rep", "--nt", "--g0", "--ntrue")
.bad <- setdiff(grep("^--", .args, value = TRUE), .known)
if (length(.bad)) stop("知らない引数: ", paste(.bad, collapse = " "))

NX       <- as.integer(.opt("--nx", "20"))
NIND_SET <- as.integer(strsplit(.opt("--nind", "300,1200,4800,19200"), ",")[[1]])
NREP     <- as.integer(.opt("--rep", "5"))
NT       <- as.integer(.opt("--nt", "200"))
G0PAR    <- as.numeric(.opt("--g0", "-4"))
NTRUE    <- as.integer(.opt("--ntrue", "1600"))

SEED         <- 20260914
TRAP_AT      <- c(2, 6, 9, 13, 17)
STEPSPERTIME <- 1
STEPAD       <- 100
TIMEBURNIN   <- 20
CPAR  <- c(-1.0, 0.3)
ADPAR <- -0.5

ncell <- NX * NX
cat(sprintf("== nind スケーリング測定 ==\n  ncell = %d (%dx%d) / nind = %s / 繰り返し %d\n",
            ncell, NX, NX, paste(NIND_SET, collapse = ", "), NREP))

cat("== secrad.r の読み込み（C++ のコンパイルに約25秒）==\n")
suppressMessages(suppressWarnings(source(SOURCEPATH, encoding = "UTF-8")))

## --- 景観と調査デザイン -----------------------------------------------------
xcoord <- rep(1:NX, each = NX)
ycoord <- rep(1:NX, NX)
trapx  <- rep(TRAP_AT, length(TRAP_AT))
trapy  <- rep(TRAP_AT, each = length(TRAP_AT))
effort_loc <- match(paste(trapx, trapy), paste(xcoord, ycoord))
effort_loc <- effort_loc[!is.na(effort_loc)]
ntrap <- length(effort_loc)
X <- as.numeric(scale(sin(xcoord / 2) + cos(ycoord / 2)))

MODEL <- list(envmodel = list(D ~ 1, C ~ X, A ~ 0),
              indmodel = c(A = FALSE, g0 = FALSE),
              occmodel = c(A = FALSE, g0 = FALSE))

## --- 1回だけシミュレートする ------------------------------------------------
cat("== 合成データを1回生成 ==\n")
set.seed(SEED)
simdata <- secrad_data$new(coords = cbind(x = xcoord, y = ycoord),
                           area = rep(1, ncell),
                           grid_cov = data.frame(X = X),
                           resolution = c(x = 1, y = 1))
simdata$add_obs(type = "poisson", effort = rep(NT, ntrap),
                effort_loc = effort_loc, effort_occ = rep(1, ntrap))
simdata$set_truemodel(envmodel = list(D ~ 1, C ~ X, A ~ 1),
                      indmodel = c(A = FALSE, g0 = FALSE),
                      occmodel = c(A = FALSE, g0 = FALSE),
                      dpar = log(NTRUE / ncell), cpar = CPAR,
                      adpar = ADPAR, g0par = G0PAR)
simdata$set_sim(NT, STEPSPERTIME, STEPAD, TIMEBURNIN)
simdata$generate_ind()
t_sim <- system.time(simdata$simulate(verbose = FALSE))[["elapsed"]]
simdata$update_detect()
base_detect <- as.matrix(simdata$obs[[1]]$detect)
cat(sprintf("  真の個体 %d / 検出 %d / 検出器 %d / %.1f秒\n",
            nrow(simdata$trueind), ncol(base_detect), ntrap, t_sim))

## --- nind を指定した secrad オブジェクトを作る（列の複製で増やす）----------
make_obj <- function(n) {
  idx <- rep_len(seq_len(ncol(base_detect)), n)
  d   <- base_detect[, idx, drop = FALSE]
  sub <- secrad_data$new(coords = simdata$coords, area = simdata$area,
                         grid_cov = simdata$grid_cov, resolution = simdata$resolution)
  o <- simdata$obs[[1]]
  sub$add_obs(type = o$type, effort = o$effort, effort_loc = o$effort_loc,
              effort_occ = o$effort_occ, detect = d)
  sub$ind_cov <- rep(1, n)
  obj <- secrad$new(secrdata = sub)
  obj$set_model(envmodel = MODEL$envmodel, indmodel = MODEL$indmodel,
                occmodel = MODEL$occmodel)
  list(obj = obj, detect_mb = as.numeric(object.size(d)) / 1024^2)
}

## ウォームアップ（初回呼び出しの余分を測定から外す）
cat("== ウォームアップ ==\n")
{
  w <- make_obj(min(NIND_SET))
  invisible(w$obj$loglf(generate_init(w$obj), loglfscale = 1))
  rm(w); gc(full = TRUE)
}

rows <- list()
for (n in NIND_SET) {
  cat(sprintf("\n---- nind = %d ----\n", n))
  built <- tryCatch(make_obj(n), error = function(e) {
    cat("  オブジェクト作成に失敗: ", conditionMessage(e), "\n"); NULL })
  if (is.null(built)) next
  obj <- built$obj
  par <- generate_init(obj)

  gc(reset = TRUE, full = TRUE)
  t_cold <- tryCatch(system.time(ll1 <- obj$loglf(par, loglfscale = 1))[["elapsed"]],
                     error = function(e) { cat("  cold 失敗: ", conditionMessage(e), "\n"); NA_real_ })
  if (is.na(t_cold)) next
  peak <- sum(gc(full = TRUE)[, 6])

  warms <- replicate(NREP, system.time(obj$loglf(par, loglfscale = 1))[["elapsed"]])
  t_warm <- median(warms)

  ## cold - warm は「キャッシュが当たったときに省かれるぶん」でしかない。
  ## ncell 由来とは限らない（実際 nind とともに伸びる）ので、そう名付けない。
  cat(sprintf("  cold %.3f秒 / warm %.3f秒（中央値, n=%d）→ 差 %.3f秒\n",
              t_cold, t_warm, NREP, t_cold - t_warm))
  cat(sprintf("  detect %.1f MB / R ヒープ峰 %.0f MB / logL %.2f\n",
              built$detect_mb, peak, ll1))

  rows[[length(rows) + 1L]] <- data.frame(
    ncell = ncell, nind = n, cold = t_cold, warm = t_warm,
    advdiff = t_cold - t_warm, warm_share = t_warm / t_cold,
    peak_mb = peak, detect_mb = built$detect_mb)

  rm(obj, built); gc(full = TRUE)
}

if (!length(rows)) stop("測定できた条件がありません。")
res <- do.call(rbind, rows)

cat("\n==================== まとめ ====================\n")
print(format(res, digits = 3), row.names = FALSE)

cat("\n## 個体部は nind に比例するか\n")
if (nrow(res) >= 2) {
  base <- res[1, ]
  tab <- data.frame(nind = res$nind,
                    `nind倍率`   = round(res$nind / base$nind, 1),
                    `個体部倍率` = round(res$warm / base$warm, 1),
                    `個体部の割合(%)` = round(100 * res$warm_share, 1),
                    check.names = FALSE)
  print(tab, row.names = FALSE)
  ## 両対数の傾き。1 なら線形、0 なら横ばい。
  if (nrow(res) >= 3 && all(res$warm > 0)) {
    b <- coef(lm(log(res$warm) ~ log(res$nind)))[2]
    cat(sprintf("\n  log-log の傾き: %.2f （1.0 = nind に線形、0 = 個体数に無関係）\n", b))
  }
  cat("\n  ＊ 個体部が線形に伸び、割合が大きくなるほど、\n")
  cat("    個体を間引く SGD はボトルネックに直接効く。\n")
  cat("    横ばいなら SGD では減らない（＝クマ規模での結論がそのまま当てはまる）。\n")
}

dir.create("results", showWarnings = FALSE)
write.csv(res, OUT_CSV, row.names = FALSE)
cat("\n保存: ", OUT_CSV, "\n", sep = "")
