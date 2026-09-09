# adcrsgd/sgd_utils.R --------------------------------------------------------
#
# SGD の1反復を安くするための小道具。secrad.r 本体には手を入れない。
#
#   source("adcrsgd/secrad.r")
#   source("adcrsgd/sgd_utils.R")
#
# 背景（2026-09-09 の profiling。docs/2026-09-09_作業記録.md）:
#
# loglf の支配的なコストは advdiff.eigen() が作る ncell x ncell 行列で、
# 個体数とは無関係。クマデータは ncell=8497 で advdiff 1回が約150秒。
# 1反復あたり10回呼ばれていたので、ここを削るのが唯一効く手。
#
# 10回の内訳:
#
#   - obj_multi と obj_single が別オブジェクトで、キャッシュ self$adtemp を
#     共有しない。同じ格子・同じ cpar に対する advdiff を2回計算していた
#   - numDeriv の前進差分は par を先頭から順に揺らす。adtemp は直前の cpar を
#     1件しか持たないので、パラメータ順 (dens_0, conn_*, g0_1) だと
#     最後の g0_1 の評価で cpar が cpar0 に戻り、キャッシュが conn の値で
#     汚れているためミスになる。6回の評価のうち5回がミス
#
# ここで用意する2つを併用すると 10回 -> 4回 になる（約2.5倍）。
# 副次的に、advdiff の結果を1つ分しか持たなくなるのでメモリも半分になる
# （ncell=8497 なら 578MB x 2 -> 578MB）。
# ---------------------------------------------------------------------------


# --- 1. advdiff キャッシュの共有 --------------------------------------------
#
# secrad の adtemp は公開フィールドで、loglf は self$adtemp$adpar /
# self$adtemp$adout を読み書きするだけ。リストの代わりに環境を入れておけば、
# 複数のオブジェクトが同じ実体を指せる（環境は参照で共有される）。
# loglf 側のコードは1行も変えなくてよい。
#
# 共有してよいのは advdiff の結果が格子とパラメータだけで決まり、
# どの個体を持っているかに依存しないから。逆に chtemp は個体ごとの量
# （ncell x nind）なので共有してはいけない。

advdiff_cache <- function() {
  e <- new.env(parent = emptyenv())
  e$adpar <- NA
  e$adout <- NA
  e
}

## 格子が同一かを確かめる。
## キャッシュキー c(cpar, apar, apar_ind, apar_occ) には格子が入っていないので、
## 違う格子のオブジェクト同士で共有すると黙って誤った値を返す。ここで止める。
same_grid <- function(a, b) {
  da <- a$secrdata; db <- b$secrdata
  isTRUE(all.equal(da$coords,     db$coords))     &&
  isTRUE(all.equal(da$resolution, db$resolution)) &&
  isTRUE(all.equal(da$area,       db$area))       &&
  isTRUE(all.equal(da$grid_cov,   db$grid_cov))   &&
  isTRUE(all.equal(a$envmodel,    b$envmodel))    &&
  isTRUE(all.equal(a$indmodel,    b$indmodel))    &&
  isTRUE(all.equal(a$occmodel,    b$occmodel))
}

## objs（secrad オブジェクトのリスト）に同じキャッシュを持たせる。
## cache を渡さなければ新規に作る。戻り値はそのキャッシュ。
share_advdiff_cache <- function(objs, cache = NULL) {
  stopifnot(length(objs) >= 1L)
  for (i in seq_along(objs)) {
    if (i > 1L && !same_grid(objs[[1]], objs[[i]])) {
      stop("格子またはモデルが一致しないオブジェクト同士で advdiff キャッシュを",
           "共有しようとしています（", i, " 番目）。",
           "キャッシュキーに格子が入っていないため、共有すると誤った値になります。")
    }
  }
  if (is.null(cache)) cache <- advdiff_cache()
  for (o in objs) o$adtemp <- cache
  invisible(cache)
}


# --- 2. キャッシュに沿った順序で有限差分を取る ------------------------------
#
# numDeriv::grad(method="simple") と同じ前進差分だが、評価の順序だけを変える。
#
#   f(x) を1回        … cpar0 で計算（ミス）
#   cpar に効かない係数を先に … cpar0 のままなのでヒット
#   cpar に効く係数を後に     … 1つずつミス
#
# ミスは 1 + (cpar に効く係数の数) になる。パラメータ順のままだと
# 最後に cpar0 へ戻る評価が余分なミスを生む。
#
# 注意: ミニバッチの1反復では、この関数に渡す func が同じ部分集合を見ていること
# （共通乱数）。評価ごとに部分集合を引き直すと差分の分子がノイズに埋もれる。

## advdiff のキャッシュキーに効く係数はどれか。
## secrad.r の generate_init の命名規則に従う（conn_*, adv_*, adv_ind_*, adv_occ_*）。
## キャッシュキーは c(cpar, apar, apar_ind, apar_occ)。
cache_par_idx <- function(par_names) {
  which(grepl("^(conn|adv)_", par_names))
}

grad_cachewise <- function(func, x, eps = 1e-4, cache_idx = NULL) {
  if (is.null(cache_idx)) cache_idx <- cache_par_idx(names(x))
  n  <- length(x)
  f0 <- func(x)
  if (!is.finite(f0)) return(rep(NA_real_, n))

  g <- rep(NA_real_, n)
  # キャッシュに効かない係数を先に、効く係数を後に
  for (j in c(setdiff(seq_len(n), cache_idx), cache_idx)) {
    xp <- x; xp[j] <- xp[j] + eps
    g[j] <- (func(xp) - f0) / eps
  }
  names(g) <- names(x)
  g
}
