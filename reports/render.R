# reports/render.R -----------------------------------------------------------
#
# レポートを Obsidian で読める Markdown として書き出す。
#
#   Rscript reports/render.R                    # 既定のレポート（adcr_sgd_report）
#   Rscript reports/render.R sampling_report    # レポート名を指定
#   Rscript reports/render.R --html             # HTML も併せて出す
#
# 素の rmarkdown::render() だと .md の YAML に output: ブロックがそのまま残り、
# Obsidian のプロパティ欄が rmarkdown の設定で埋まってしまう。
# ここではレンダー後にその部分だけを取り除いて、title / subtitle / date / tags
# だけが残るようにしている。
# ---------------------------------------------------------------------------

args      <- commandArgs(trailingOnly = TRUE)
want_html <- "--html" %in% args
target    <- setdiff(args, "--html")
target    <- if (length(target)) target[1] else "adcr_sgd_report"

RMD <- file.path("reports", paste0(target, ".Rmd"))
MD  <- file.path("reports", paste0(target, ".md"))

if (!file.exists(RMD)) {
  stop("見つかりません: ", RMD,
       "\nリポジトリルート（Claude/banding_data）で実行してください。現在: ", getwd())
}

cat("Markdown を生成中 ...\n")
rmarkdown::render(RMD, output_format = "md_document", quiet = TRUE)

# --- YAML から output: ブロックを取り除く -----------------------------------

strip_output_key <- function(path) {
  x <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(x) < 2L || x[1] != "---") return(invisible(FALSE))
  close_at <- which(x == "---")[2]
  if (is.na(close_at)) return(invisible(FALSE))

  head_lines <- x[2:(close_at - 1)]
  start <- grep("^output:\\s*$", head_lines)
  if (!length(start)) return(invisible(FALSE))

  # output: の次のトップレベルキー（行頭が空白でない行）まで削る
  rest  <- head_lines[(start[1] + 1):length(head_lines)]
  nextk <- which(grepl("^[^[:space:]]", rest))
  drop  <- if (length(nextk)) start[1]:(start[1] + nextk[1] - 1) else start[1]:length(head_lines)

  writeLines(c("---", head_lines[-drop], "---", x[(close_at + 1):length(x)]),
             path, useBytes = TRUE)
  invisible(TRUE)
}

if (strip_output_key(MD)) cat("  YAML から output: を除去\n")

figs <- list.files("reports/figures", pattern = "[.]png$", full.names = TRUE)
cat(sprintf("\n出力: %s (%.1f KB)\n", MD, file.size(MD) / 1024))
cat(sprintf("図  : %d 枚 / reports/figures/\n", length(figs)))

if (want_html) {
  cat("\nHTML を生成中 ...\n")
  rmarkdown::render(RMD, output_format = "html_document", quiet = TRUE)
  cat(sprintf("出力: reports/%s.html\n", target))
}
