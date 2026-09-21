# tests/parse_check.R --------------------------------------------------------
#
# **リポジトリ内のすべての R スクリプトが構文的に正しいかを確認する。**
#
#   Rscript tests/parse_check.R
#
# ---------------------------------------------------------------------------
# なぜ要るのか（2026-09-21）
#
# このPC（編集機）では **SGD_bear_20260909.R を実行できない**。
# クマデータが活動範囲の外にあり、スクリプト自体も allow list に無いため。
# つまり**編集機で書き換えたコードを、編集機では一度も走らせずに別PCへ渡す**
# ことになる。別PCではそれが数十時間無人で走る。
#
# **構文エラーなら1秒で落ちるが、それが分かるのは数十時間後**という事故が
# ありうる。R はファイル全体を構文解析してから実行するので、
# **parse() だけなら実データもパッケージも要らずに検査できる。**
#
# 意味の正しさは分からない。**「走り出しはする」ことだけを保証する。**
# ---------------------------------------------------------------------------

DIRS <- c(".", "adcrsgd", "examples", "tests", "reports")

files <- unlist(lapply(DIRS, function(d)
  list.files(d, pattern = "[.][Rr]$", full.names = TRUE)))
files <- sort(unique(files[file.exists(files)]))

if (!length(files)) stop("R スクリプトが見つかりません。作業ディレクトリを確認してください: ", getwd())

cat("== 構文チェック ==\n")
cat("  作業ディレクトリ: ", getwd(), "\n", sep = "")
cat("  対象: ", length(files), " ファイル\n\n", sep = "")

ng <- character(0)
for (f in files) {
  res <- tryCatch({ parse(f, encoding = "UTF-8"); NULL },
                  error = function(e) conditionMessage(e))
  if (is.null(res)) {
    cat(sprintf("  OK   %s\n", f))
  } else {
    ng <- c(ng, f)
    cat(sprintf("  NG   %s\n", f))
    cat("       ", gsub("\n", "\n       ", trimws(res)), "\n", sep = "")
  }
}

cat(sprintf("\n%d 件中 %d 件が構文 OK\n", length(files), length(files) - length(ng)))
if (length(ng)) {
  cat("構文エラー:\n")
  cat(paste0("  ", ng, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("すべて構文 OK。**意味の正しさは別問題**（smoke_test.R を通すこと）。\n")
