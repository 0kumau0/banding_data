# tests/band_env_check.R ------------------------------------------------------
#
# **Segmentation fault がどの部品で起きるかを特定する。**
#
#   Rscript tests/band_env_check.R
#   Rscript tests/band_env_check.R --log results/env_check.log
#
# ---------------------------------------------------------------------------
# なぜこれが要るのか（2026-09-29）
#
# 別PCで `examples/band_timing.R` が Segmentation fault になったが、
# **コンソールに1行も出なかった**。`--rate 0.01` のヤマガラ（個体数は桁違いに
# 少ない）でも同じなので、**規模の問題ではない**。
#
# segfault は R のエラーと違って後始末をしないので、
#
#   - `cat` の出力はバッファに残ったまま捨てられる
#   - `sink()` の中身も失われる
#   - MINGW64 のパイプ越しだと `flush(stdout())` も当てにならない
#
# **唯一確実に残るのは「書いて閉じたファイル」。**
# だから `step()` は毎回 open → write → close する。効率は悪いが、
# ここで欲しいのは速度ではなく**落ちる直前の1行**。
#
# 各段は「前の段の結果に依存しない」ように並べてあるので、
# どこで落ちてもそこまでの情報は全部ログに残る。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
.opt <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1L]
}
LOG <- .opt("--log", "results/band_env_check.log")
dir.create(dirname(LOG), showWarnings = FALSE, recursive = TRUE)
if (file.exists(LOG)) file.remove(LOG)

## ★ 毎回 open → write → close する。これだけが segfault を生き延びる
step <- function(...) {
  line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste0(..., collapse = ""))
  con <- file(LOG, open = "at", encoding = "UTF-8")
  writeLines(line, con); close(con)
  cat(line, "\n", sep = ""); try(flush(stdout()), silent = TRUE)
}
## 段が落ちても R のエラーなら続ける（segfault なら続かない。それが知りたい）
attempt <- function(label, expr) {
  step("→ ", label)
  r <- try(force(expr), silent = TRUE)
  if (inherits(r, "try-error")) {
    step("   ✗ R のエラー: ", conditionMessage(attr(r, "condition")))
    return(invisible(NULL))
  }
  step("   ✓ ", label)
  invisible(r)
}

step("=== band_env_check 開始 ===")
step("ログ: ", normalizePath(LOG, winslash = "/", mustWork = FALSE))

# --- 1. R が動くか -----------------------------------------------------------
step("1. R ", R.version.string)
step("   platform: ", R.version$platform)
step("   作業ディレクトリ: ", getwd())
step("   OMP_NUM_THREADS = ", Sys.getenv("OMP_NUM_THREADS", "(未設定)"))
## memory.limit() は R 4.2 以降 defunct なので使わない
step("   R が確保済みのメモリ: ",
     tryCatch(sprintf("%.2f GB", sum(gc()[, "used"] * c(8, 8)) / 1e9),
              error = function(e) "(取得不可)"))

# --- 2. パッケージ -----------------------------------------------------------
## **sf の読み込み自体が落ちることがある**（GDAL/GEOS/PROJ の DLL の取り違え）。
## だから1つずつ、読み込んだ直後にバージョンを書き出す。
attempt("library(Matrix)", { library(Matrix); step("   Matrix ", as.character(packageVersion("Matrix"))) })
attempt("library(sf)",     { library(sf);     step("   sf ", as.character(packageVersion("sf"))) })
## GDAL / GEOS / PROJ の版。**DLL の取り違えはここに出る**
attempt("sf の外部ライブラリ", {
  v <- sf::sf_extSoftVersion()
  step("   ", paste(names(v), v, sep = " ", collapse = " / "))
})
attempt("library(Rcpp)",   step("   Rcpp ", as.character(packageVersion("Rcpp"))))
attempt("library(RcppEigen)", step("   RcppEigen ", as.character(packageVersion("RcppEigen"))))

# --- 3. config.R -------------------------------------------------------------
attempt("source(config.R)", {
  if (!file.exists("config.R"))
    stop("config.R が無い。リポジトリルートで実行すること: ", getwd())
  source("config.R", encoding = "UTF-8")
  for (v in c("BAND_MESH", "BAND_RDS")) {
    p <- if (exists(v)) get(v) else "(未定義)"
    step("   ", v, " = ", p,
         if (exists(v) && file.exists(p))
           sprintf("  [%.2f GB]", file.size(p) / 1e9) else "  [★見つからない]")
  }
})

# --- 4. メッシュ（GDAL / GEOS）----------------------------------------------
## ここで落ちるなら sf まわり。データの規模とは無関係
##
## `--no-data` は §4-5 を飛ばす。**編集機（このPC）で動作確認するため**。
## 生データ（griddata\ と git\ 直下）は編集機の Claude の活動範囲の外なので、
## 許可なく読まない（CLAUDE.md「触ってよい範囲」）。
NO_DATA <- "--no-data" %in% args
if (NO_DATA) step("※ --no-data: §4（メッシュ）と §5（rds）を飛ばす")

mesh <- if (NO_DATA) NULL else attempt("st_read(BAND_MESH)", {
  m <- sf::st_read(BAND_MESH, quiet = TRUE)
  step("   ", nrow(m), " 行 × ", ncol(m), " 列")
  step("   列: ", paste(names(m), collapse = ", "))
  m
})
if (!is.null(mesh)) {
  attempt("st_centroid（GEOS）", {
    xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(mesh)))
    step("   重心 ", nrow(xy), " 点 / NA ", sum(is.na(xy)))
  })
  attempt("st_area", {
    a <- as.numeric(sf::st_area(mesh)) / 1e6
    step(sprintf("   面積 中央 %.1f km²（最小 %.1f 最大 %.1f）", median(a), min(a), max(a)))
  })
}

# --- 5. rds ------------------------------------------------------------------
d <- if (NO_DATA) NULL else attempt("readRDS(BAND_RDS)", {
  x <- readRDS(BAND_RDS)
  step("   要素: ", paste(names(x), collapse = ", "))
  x
})
if (!is.null(d)) {
  attempt("effort の中身", {
    e <- as.data.frame(d$effort)
    step("   effort ", nrow(e), " 行 × ", ncol(e), " 列: ", paste(names(e), collapse = ", "))
    if (!is.null(e$effort_occ)) {
      o <- as.integer(e$effort_occ)
      step("   ★ effort_occ: ", length(unique(o)), " 水準 / 範囲 ",
           min(o, na.rm = TRUE), "〜", max(o, na.rm = TRUE),
           " / NA ", sum(is.na(o)))
      step("      → secrad.r:1275 が max(effort_occ) で srv を確保する。",
           "水準数と最大値が食い違っていたら振り直しが要る")
    } else step("   ★ effort_occ の列が無い")
    if (!is.null(e$meshcode)) {
      mc <- as.integer(as.character(e$meshcode))
      step("   ★ meshcode（= メッシュの行番号）: 範囲 ",
           min(mc, na.rm = TRUE), "〜", max(mc, na.rm = TRUE), " / NA ", sum(is.na(mc)))
    }
  })
  attempt("detect_list の中身", {
    step("   splist ", length(d$splist), " 種 / detect_list ", length(d$detect_list), " 件")
    for (sp in c("ヤマガラ", "シジュウカラ")) {
      i <- which(d$splist == sp)
      if (!length(i) || i > length(d$detect_list)) { step("   ", sp, ": 無し"); next }
      m <- d$detect_list[[i]]
      step("   ", sp, ": ", nrow(m), " × ", ncol(m), " (", class(m)[1], ")")
    }
  })
}

# --- 6. C++（ここが本命の容疑者）--------------------------------------------
## `secrad.r` は毎回 sourceCpp で OpenMP 付きの C++ をコンパイルする。
## **ツールチェーンの不一致ならここで落ちる**（データとは無関係）。
attempt("source(adcrsgd/secrad.r) — C++ のコンパイル", {
  suppressMessages(suppressWarnings(source("adcrsgd/secrad.r", encoding = "UTF-8")))
  step("   コンパイル完了")
})
attempt("source(adcrsgd/sgd_utils.R)", source("adcrsgd/sgd_utils.R", encoding = "UTF-8"))

step("=== 完了。ここまで出ていれば部品は全部生きている ===")
step("")
step("次: エンジンの実行経路（C++ の中）を確かめる。**別プロセスで**:")
step("    Rscript tests/smoke_test.R")
step("  合成データで loglf まで通す既存のテスト。")
step("  これが落ちるなら実データとは無関係にエンジン側の問題。")
step("ログ: ", LOG)
