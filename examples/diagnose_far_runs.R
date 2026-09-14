# examples/diagnose_far_runs.R -----------------------------------------------
#
# 週末の `--init far` 実行（A2 / B4、各200反復）が参照解に届かなかった原因を、
# result ファイルに残った trace から切り分ける。
#
#   Rscript examples/diagnose_far_runs.R [<result.RData> ...]
#
# 読むだけ。実データは要らない（trace しか見ない）。
#
# 切り分けたい3仮説（docs/20260914_作業記録.md）:
#   1. 平坦な尾根 / 別の最適点  … 終点の logL が参照解と同等なら該当。
#                                 ログの時点で棄却済み（-766 対 -685.5）だが数値で残す
#   2. 勾配が間違っている        … conn_0 と conn_agri が同一の更新を受けていないか。
#                                 勾配が参照解の方向を向いているか
#   3. 失速                      … |step| / alpha が 1 から大きく落ちていないか。
#                                 Adam の step = alpha * m/sqrt(v) なので、
#                                 この比がそのまま m/sqrt(v) の推定になる
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

files <- if (length(args)) args else
  list.files(".", pattern = "^SGD_bear_.*_far_.*_result\\.RData$")

if (!length(files)) {
  cat("far 実行の result ファイルが見つかりません。作業ディレクトリ: ", getwd(), "\n", sep = "")
  quit(status = 0)
}

## SGD_bear_20260909.R と同じ定義。result には alpha が保存されていないので再構成する。
ALPHA_DEFAULT    <- 0.02
MAX_STEP_DEFAULT <- 0.05
ALPHA_BY_PAR     <- c(conn_0 = 0.03)
MAX_STEP_BY_PAR  <- c(conn_0 = 0.10)
INIT_FAR <- c(dens_0 = -1, conn_0 = -2, conn_agri = 0, conn_wtr = 0, g0_1 = -5)

by_par <- function(default, overrides, nm) {
  v <- setNames(rep(default, length(nm)), nm)
  hit <- intersect(names(overrides), nm)
  v[hit] <- overrides[hit]
  v
}

hr <- function(ch = "-") cat(strrep(ch, 78), "\n")

for (f in files) {
  hr("=")
  e <- new.env(); load(f, envir = e)
  nm    <- colnames(e$trace_par)
  n     <- e$iter_done
  mult  <- if (is.null(e$ALPHA_MULT)) 1 else e$ALPHA_MULT
  alpha <- by_par(ALPHA_DEFAULT,    ALPHA_BY_PAR,    nm) * mult
  mstep <- by_par(MAX_STEP_DEFAULT, MAX_STEP_BY_PAR, nm) * mult

  cat(f, "\n  tag=", e$RUN_TAG, " alpha_mult=", mult, " beta2=", e$BETA2,
      " 反復=", n, "\n\n", sep = "")

  P <- e$trace_par [seq_len(n), , drop = FALSE]
  G <- e$trace_grad[seq_len(n), , drop = FALSE]
  S <- e$trace_step[seq_len(n), , drop = FALSE]
  L <- e$trace_ll  [seq_len(n)]
  ref <- e$ref_par[nm]

  ## --- 仮説1: そもそもどれだけ低いのか -------------------------------------
  cat("## 対数尤度\n")
  cat(sprintf("  参照解          : %.6f\n", -685.5202223))
  cat(sprintf("  iter 1          : %.6f\n", L[1]))
  cat(sprintf("  iter %d (最終)  : %.6f   （参照解との差 %.3f）\n",
              n, L[n], L[n] - (-685.5202223)))
  cat(sprintf("  最大            : %.6f  (iter %d)\n", max(L), which.max(L)))
  dL <- diff(L)
  cat(sprintf("  単調増加でない反復: %d 件 / %d\n", sum(dL < 0), length(dL)))
  gap <- -685.5202223 - L[n]
  for (w in c(50, 10)) {
    r <- mean(tail(dL, w))
    cat(sprintf("  直近%3d反復の平均上昇: %.5f / 反復  → 差を埋めるのに %.0f 反復（%.0f 日 @24.1分）\n",
                w, r, gap / r, gap / r * 24.1 / 60 / 24))
  }
  cat("  ＊ 直近10 が 直近50 より小さければ減速しており、上の日数はどれも楽観値\n\n")

  ## --- 仮説3: 失速。|step|/alpha が Adam の m/sqrt(v) にあたる --------------
  cat("## |step| / alpha （1 に近いほど本来の歩幅。小さいほど失速）\n")
  ratio <- sweep(abs(S), 2, alpha, "/")
  blk <- unique(round(seq(1, n, length.out = min(8, n))))
  print(round(ratio[blk, , drop = FALSE], 4))
  cat("\n  max_step に当たった回数:\n  ")
  print(colSums(abs(S) >= matrix(mstep, n, length(nm), byrow = TRUE) - 1e-12))

  ## --- 仮説2a: 勾配は参照解の方向を向いているか ----------------------------
  ## sign(g) と sign(ref - 現在値) が一致していれば「参照解へ向かっている」。
  cat("\n## 勾配は参照解の方を向いているか（一致率 %）\n")
  ## sweep(P, 2, ref, "-") は P - ref。向きが欲しいので符号を反転して ref - P にする。
  to_ref  <- -sweep(P, 2, ref, "-")
  toward  <- sign(G) == sign(to_ref)
  cat("  全反復  : "); print(round(100 * colMeans(toward), 1))
  cat("  直近50  : "); print(round(100 * colMeans(tail(toward, 50)), 1))
  ## 全体としての向き。勾配ベクトルと「参照解へ向かう直線」のなす角の cos。
  cosang <- rowSums(G * to_ref) /
            (sqrt(rowSums(G^2)) * sqrt(rowSums(to_ref^2)))
  cat(sprintf("  勾配と参照解方向の cos: 全体 %+.3f / 直近50 %+.3f\n",
              mean(cosang), mean(tail(cosang, 50))))

  ## --- 仮説2b: conn_0 と conn_agri が同一の更新を受けていないか ------------
  if (all(c("conn_0", "conn_agri") %in% nm)) {
    cat("\n## conn_0 と conn_agri は別々に動いているか\n")
    cat(sprintf("  勾配の相関        : %+.4f\n", cor(G[, "conn_0"], G[, "conn_agri"])))
    cat(sprintf("  勾配の最大絶対差  : %.6e\n",
                max(abs(G[, "conn_0"] - G[, "conn_agri"]))))
    cat(sprintf("  ステップの相関    : %+.4f\n", cor(S[, "conn_0"], S[, "conn_agri"])))
    cat(sprintf("  ステップの最大絶対差: %.6e\n",
                max(abs(S[, "conn_0"] - S[, "conn_agri"]))))
    cat("  ＊ 差が 0 に近ければ同じ摂動を見ている疑い（勾配のバグ）\n")
  }

  ## --- 終点の内訳 -----------------------------------------------------------
  cat("\n## 最終反復の内訳\n")
  tab <- rbind(初期値 = INIT_FAR[nm], 最終値 = P[n, ], 参照解 = ref,
               `差(最終-参照)` = P[n, ] - ref,
               勾配 = G[n, ], ステップ = S[n, ],
               alpha = alpha, `|step|/alpha` = abs(S[n, ]) / alpha)
  print(round(tab, 5))

  ## --- 各係数がいつ参照解に最も近づいたか ----------------------------------
  cat("\n## 各係数の |誤差| が最小になった反復\n")
  err <- abs(sweep(P, 2, ref, "-"))
  best <- apply(err, 2, which.min)
  print(rbind(`最良の反復` = best,
              `そのときの誤差` = round(err[cbind(best, seq_along(best))], 4),
              `最終の誤差` = round(err[n, ], 4)))
  cat("\n")
}
hr("=")
