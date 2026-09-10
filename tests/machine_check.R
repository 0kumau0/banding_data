# tests/machine_check.R ------------------------------------------------------
#
# 解析マシンの健康状態と、実行中／実行済みの SGD の健全性を1つの Markdown に
# まとめる。別PCで走らせて、出来たファイルを commit / push すれば、
# こちらのPCから状態を読める。
#
#   Rscript tests/machine_check.R
#   Rscript tests/machine_check.R --memtest      # メモリ自己検査も行う（数分）
#   Rscript tests/machine_check.R --memtest-gb 16
#
# 作業ディレクトリはリポジトリルート（banding_data）。
# 出力: results/machine_check_<ホスト名>_<日時>.md
#
# ---------------------------------------------------------------------------
# なぜ必要か
# ---------------------------------------------------------------------------
#
# 2026-09-10、解析マシン（HP、16GB x 8 = 128GB）で CPU1-DIMM6 のメモリ初期化
# 警告が出て、認識容量が 120GB に減った状態が見つかった。片ランクが BIOS で
# マップアウトされたものと思われる。この状態でも解析は進められるが、
#
#   - ECC が効いているか（1ビット誤りが黙って結果を壊さないか）
#   - 訂正エラーが増え続けていないか
#   - 途中で再起動していないか、その原因は何か
#   - SGD の検算（対数尤度が旧記録と一致するか）が通ったか
#
# を毎回手で確認するのは面倒なので、まとめて取れるようにした。
# ---------------------------------------------------------------------------

args        <- commandArgs(trailingOnly = TRUE)
DO_MEMTEST  <- "--memtest" %in% args
MEMTEST_GB  <- {
  i <- which(args == "--memtest-gb")
  if (length(i) && length(args) > i[1]) as.numeric(args[i[1] + 1]) else 8
}
if (any(args == "--memtest-gb")) DO_MEMTEST <- TRUE

# 検算の基準値（SGD_bear_20260909.R と同じ）
OLD_LOGLIK <- -685.5202223

out <- character(0)
add <- function(...) out <<- c(out, paste0(...))
sec <- function(title) { add(""); add("## ", title); add("") }
code_block <- function(txt) { add("```"); add(txt); add("```"); add("") }

judgments <- list()
judge <- function(ok, label, detail = "") {
  judgments[[length(judgments) + 1L]] <<-
    list(ok = ok, label = label, detail = detail)
}

# --- PowerShell を1回だけ呼んで、必要な情報をまとめて取る -------------------
# 引用符の扱いで嵌りやすいので、一時ファイルに書いて -File で実行する。

run_powershell <- function(script_text) {
  if (.Platform$OS.type != "windows") return(NULL)
  f <- tempfile(fileext = ".ps1")
  # PowerShell は BOM 無し UTF-8 だと日本語環境で化けることがあるため ASCII で書く
  writeLines(script_text, f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)
  res <- tryCatch(
    system2("powershell", c("-NoProfile", "-NonInteractive", "-ExecutionPolicy",
                            "Bypass", "-File", f),
            stdout = TRUE, stderr = TRUE),
    warning = function(e) NULL, error = function(e) NULL)
  res
}

PS <- c(
  '$ErrorActionPreference = "SilentlyContinue"',
  '',
  'Write-Output "===SYSTEM==="',
  '$cs = Get-CimInstance Win32_ComputerSystem',
  '$os = Get-CimInstance Win32_OperatingSystem',
  'Write-Output ("Host`t" + $env:COMPUTERNAME)',
  'Write-Output ("Model`t" + $cs.Manufacturer + " " + $cs.Model)',
  'Write-Output ("OS`t" + $os.Caption + " " + $os.Version)',
  'Write-Output ("TotalPhysicalMemoryGB`t" + [math]::Round($cs.TotalPhysicalMemory/1GB,1))',
  'Write-Output ("LastBootUpTime`t" + $os.LastBootUpTime)',
  'Write-Output ("LogicalProcessors`t" + $cs.NumberOfLogicalProcessors)',
  '',
  'Write-Output "===ECC==="',
  'Get-CimInstance Win32_PhysicalMemoryArray | ForEach-Object {',
  '  $m = switch ($_.MemoryErrorCorrection) {',
  '    0 {"Reserved"} 1 {"Other"} 2 {"Unknown"} 3 {"None"}',
  '    4 {"Parity"} 5 {"Single-bit ECC"} 6 {"Multi-bit ECC"} 7 {"CRC"}',
  '    default {"code " + $_.MemoryErrorCorrection} }',
  '  Write-Output ("MemoryErrorCorrection`t" + $m)',
  '}',
  '',
  'Write-Output "===DIMMS==="',
  'Get-CimInstance Win32_PhysicalMemory |',
  '  Sort-Object DeviceLocator |',
  '  ForEach-Object {',
  '    Write-Output ($_.DeviceLocator + "`t" + [math]::Round($_.Capacity/1GB,0) + "GB`t" +',
  '                  $_.Speed + "MHz`t" + $_.Manufacturer + "`t" + $_.PartNumber)',
  '  }',
  '',
  'Write-Output "===WHEA_COUNT==="',
  '$whea = Get-WinEvent -FilterHashtable @{LogName="System"; ProviderName="Microsoft-Windows-WHEA-Logger"} -MaxEvents 2000',
  'if ($whea) {',
  '  Write-Output ("Total`t" + $whea.Count)',
  '  Write-Output ("Last7Days`t" + ($whea | Where-Object {$_.TimeCreated -gt (Get-Date).AddDays(-7)}).Count)',
  '  Write-Output ("Last24Hours`t" + ($whea | Where-Object {$_.TimeCreated -gt (Get-Date).AddDays(-1)}).Count)',
  '} else { Write-Output "Total`t0" }',
  '',
  'Write-Output "===WHEA_RECENT==="',
  'if ($whea) {',
  '  $whea | Select-Object -First 10 | ForEach-Object {',
  '    Write-Output ($_.TimeCreated.ToString("yyyy-MM-dd HH:mm:ss") + "`tID" + $_.Id + "`t" +',
  '                  ($_.Message -split "`r?`n")[0])',
  '  }',
  '}',
  '',
  'Write-Output "===RESTARTS==="',
  '# 41 = 予期しない停止 / 6008 = 前回のシャットダウンが異常 / 1074 = 計画的な再起動（要求元が分かる）',
  '$ev = Get-WinEvent -FilterHashtable @{LogName="System"; Id=41,1074,6008} -MaxEvents 40',
  'if ($ev) {',
  '  $ev | ForEach-Object {',
  '    Write-Output ($_.TimeCreated.ToString("yyyy-MM-dd HH:mm:ss") + "`tID" + $_.Id + "`t" +',
  '                  (($_.Message -split "`r?`n")[0]))',
  '  }',
  '} else { Write-Output "(該当なし)" }',
  '',
  'Write-Output "===END==="'
)

ps_out <- run_powershell(PS)

split_sections <- function(lines) {
  if (is.null(lines)) return(list())
  marks <- grep("^===[A-Z_]+===$", lines)
  if (!length(marks)) return(list())
  res <- list()
  for (i in seq_along(marks)) {
    nm <- gsub("=", "", lines[marks[i]])
    from <- marks[i] + 1L
    to <- if (i < length(marks)) marks[i + 1L] - 1L else length(lines)
    res[[nm]] <- if (to >= from) lines[from:to] else character(0)
  }
  res
}
S <- split_sections(ps_out)

kv <- function(section, key) {
  x <- S[[section]]
  if (is.null(x)) return(NA_character_)
  hit <- grep(paste0("^", key, "\t"), x, value = TRUE)
  if (!length(hit)) return(NA_character_)
  sub(paste0("^", key, "\t"), "", hit[1])
}

# ===========================================================================
# 報告の組み立て
# ===========================================================================

host <- kv("SYSTEM", "Host")
if (is.na(host) || !nzchar(host)) host <- Sys.info()[["nodename"]]
stamp <- format(Sys.time(), "%Y%m%d_%H%M")

add("---")
add("title: \"解析マシンの状態チェック\"")
add("subtitle: \"", host, " / ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\"")
add("date: ", format(Sys.Date()))
add("tags:")
add("  - ADCR")
add("  - machine-check")
add("---")
add("")
add("`Rscript tests/machine_check.R` の出力。**このファイルを commit / push すれば、",
    "編集機側から状態を確認できる。**")

# --- 1. システム ------------------------------------------------------------
sec("1. システム")
if (length(S[["SYSTEM"]])) {
  add("| 項目 | 値 |"); add("|---|---|")
  for (l in S[["SYSTEM"]]) {
    p <- strsplit(l, "\t", fixed = TRUE)[[1]]
    if (length(p) >= 2) add("| ", p[1], " | ", paste(p[-1], collapse = " "), " |")
  }
  add("")
} else {
  add("（PowerShell から情報を取得できませんでした。Windows 以外の環境か、",
      "実行がブロックされています。）")
  add("")
}
add("- R: ", R.version.string)
add("- 作業ディレクトリ: `", getwd(), "`")
add("")

# --- 2. メモリ --------------------------------------------------------------
sec("2. メモリ構成")

ecc <- kv("ECC", "MemoryErrorCorrection")
total_gb <- suppressWarnings(as.numeric(kv("SYSTEM", "TotalPhysicalMemoryGB")))

if (!is.na(ecc)) {
  add("**ECC: ", ecc, "**")
  add("")
  judge(grepl("ECC", ecc),
        paste0("ECC = ", ecc),
        if (grepl("ECC", ecc))
          "1ビット誤りは訂正・記録されるので、計算結果が黙って壊れるリスクは低い。"
        else
          "ECC が無い（または不明）。メモリ由来の誤りが検出されずに結果を壊しうる。")
}

if (length(S[["DIMMS"]])) {
  add("| スロット | 容量 | 速度 | 製造元 | 型番 |")
  add("|---|---|---|---|---|")
  for (l in S[["DIMMS"]]) {
    p <- strsplit(l, "\t", fixed = TRUE)[[1]]
    while (length(p) < 5) p <- c(p, "")
    add("| ", paste(p[1:5], collapse = " | "), " |")
  }
  add("")
  n_dimm <- length(S[["DIMMS"]])
  add("認識しているモジュール数: **", n_dimm, "**")
  if (!is.na(total_gb)) add("OS が認識している総容量: **", total_gb, " GB**")
  add("")
}

# この解析に必要なメモリの見積もり。ncell が分かれば具体的に出す。
add("### この解析に必要なメモリの見積もり")
add("")
add("支配的なのは advdiff の `ncell x ncell` 行列。`loglf` 1回の実行中に",
    "同時に存在するのは3〜4枚程度で、個体依存の配列（`ncell x nind`）は桁違いに小さい。")
add("")
add("| ncell | 行列1枚 | ピーク目安(4枚) |")
add("|---|---|---|")
for (nc in c(400, 2500, 8497)) {
  one <- nc^2 * 8 / 1024^3
  add(sprintf("| %d | %s | %s |", nc,
              if (one < 0.01) sprintf("%.0f MB", one * 1024) else sprintf("%.2f GB", one),
              if (one * 4 < 0.01) sprintf("%.0f MB", one * 4096) else sprintf("%.1f GB", one * 4)))
}
add("")
add("**クマデータ（ncell = 8497）でもピークは数 GB。容量面の制約は無い。**")
add("")

# --- 3. WHEA ---------------------------------------------------------------
sec("3. ハードウェアエラー（WHEA）")

w_total <- suppressWarnings(as.integer(kv("WHEA_COUNT", "Total")))
w_7d    <- suppressWarnings(as.integer(kv("WHEA_COUNT", "Last7Days")))
w_24h   <- suppressWarnings(as.integer(kv("WHEA_COUNT", "Last24Hours")))

if (!is.na(w_total)) {
  add("| 期間 | 件数 |"); add("|---|---|")
  add("| 記録全体（最大2000件） | ", w_total, " |")
  if (!is.na(w_7d))  add("| 直近7日 | ", w_7d, " |")
  if (!is.na(w_24h)) add("| 直近24時間 | ", w_24h, " |")
  add("")
  n24 <- if (is.na(w_24h)) 0L else w_24h
  judge(n24 <= 5L,
        paste0("直近24時間の WHEA イベント ", n24, " 件"),
        if (n24 <= 5L) "散発的。解析を進めてよい水準。"
        else "頻発している。解析より先にメモリ交換を検討したほうがよい。")
  if (length(S[["WHEA_RECENT"]])) {
    add("直近のイベント:"); add("")
    code_block(paste(S[["WHEA_RECENT"]], collapse = "\n"))
  }
} else {
  add("（取得できませんでした）"); add("")
}

# --- 4. 再起動履歴 ----------------------------------------------------------
sec("4. 再起動の履歴")
add("`41` = 予期しない停止 / `6008` = 前回のシャットダウンが異常 /",
    "`1074` = 計画的な再起動（要求したプロセスが分かる）")
add("")
if (length(S[["RESTARTS"]])) {
  code_block(paste(S[["RESTARTS"]], collapse = "\n"))
  n41 <- sum(grepl("\tID41\t", S[["RESTARTS"]]))
  judge(n41 == 0L,
        paste0("予期しない停止（ID 41）", n41, " 件"),
        if (n41 == 0L) "記録の範囲では予期しない停止は無い。"
        else "予期しない停止がある。長時間実行の前に原因を潰しておきたい。")
} else {
  add("（取得できませんでした）"); add("")
}
add("**長時間実行の前に、Windows Update の自動再起動を止めておくこと。**",
    "数日かかる解析では、これが最も多い中断原因。")
add("")

# --- 5. SGD 実行の状態 ------------------------------------------------------
sec("5. SGD 実行の状態")

logf <- list.files(".", pattern = "^SGD_bear_.*_log\\.txt$")
ckpt <- list.files(".", pattern = "^SGD_bear_.*_checkpoint\\.rds$")
optf <- list.files(".", pattern = "^SGD_bear_.*_optim\\.rds$")

if (!length(logf) && !length(ckpt) && !length(optf)) {
  add("実行の痕跡が見つかりません（`SGD_bear_*` のログ・チェックポイント・optim）。")
  add("")
} else {

  ## 5-1 検算
  add("### 対数尤度の検算")
  add("")
  add("旧解を新しいパラメータ化へ座標変換した点での対数尤度が、旧記録 **",
      sprintf("%.7f", OLD_LOGLIK), "** と一致するか。")
  add("これは 8497 x 8497 の行列演算を含む計算全体が正しく行われたことの実証になる。",
      "**メモリ由来の破損があれば、まず一致しない。**")
  add("")
  if (length(logf)) {
    for (f in logf) {
      x <- readLines(f, warn = FALSE, encoding = "UTF-8")
      hit <- grep("loglf = |一致|CHECK_TOL|旧記録", x, value = TRUE)
      add("`", f, "`（", length(x), " 行、更新 ",
          format(file.mtime(f), "%Y-%m-%d %H:%M"), "）")
      add("")
      if (length(hit)) {
        code_block(paste(utils::head(hit, 12), collapse = "\n"))
        ok <- any(grepl("一致", hit)) && !any(grepl("一致しません", hit))
        judge(ok, paste0("検算（", f, "）"),
              if (ok) "対数尤度が旧記録と一致。計算は健全。"
              else "一致の記録が見当たらない。ログを直接確認すること。")
      } else {
        add("検算の行が見つかりません。まだそこまで進んでいないか、",
            "`RUN_CHECK <- FALSE` で実行されています。")
        add("")
        judge(FALSE, paste0("検算（", f, "）"), "検算の記録が無い。")
      }

      ## 5-2 進捗と速度
      it <- grep("^\\[..:..:..\\] iter ", x, value = TRUE)
      if (length(it)) {
        add("進捗（最後の5件）:"); add("")
        code_block(paste(utils::tail(it, 5), collapse = "\n"))
        s <- suppressWarnings(as.numeric(sub(".*  ([0-9.]+)秒  .*", "\\1", it)))
        s <- s[is.finite(s)]
        if (length(s)) {
          add("1反復あたり: 中央値 **", round(median(s)), " 秒**",
              "（", round(median(s) / 60, 1), " 分）、記録 ", length(s), " 件")
          add("")
          add("**300反復に換算すると約 ", round(median(s) * 300 / 3600 / 24, 1),
              " 日。**")
          add("")
        }
      }
    }
  } else {
    add("ログファイルがありません。"); add("")
  }

  ## 5-3 チェックポイント
  if (length(ckpt)) {
    add("### チェックポイント")
    add("")
    for (f in ckpt) {
      ck <- tryCatch(readRDS(f), error = function(e) NULL)
      if (is.null(ck)) { add("`", f, "` … 読み込めません"); next }
      add("`", f, "`（更新 ", format(file.mtime(f), "%Y-%m-%d %H:%M"), "）")
      add("")
      add("- 完了した反復: **", ck$iter_done, "**")
      add("- INIT_MODE: ", if (is.null(ck$init_mode)) "(記録なし)" else ck$init_mode)
      add("- sampling_rate: ", ck$sampling_rate)
      add("")
      if (!is.null(ck$current_par) && !is.null(ck$ref_par)) {
        add("| 係数 | 現在値 | 参照解 | 差 |"); add("|---|---|---|---|")
        for (p in names(ck$current_par))
          add(sprintf("| %s | %.5f | %.5f | %+.5f |", p, ck$current_par[[p]],
                      ck$ref_par[[p]], ck$current_par[[p]] - ck$ref_par[[p]]))
        add("")
        add("最大絶対誤差: **",
            sprintf("%.5f", max(abs(ck$current_par - ck$ref_par))), "**")
        add("")
      }
    }
  }

  ## 5-4 optim
  if (length(optf)) {
    add("### BFGS 参照解")
    add("")
    for (f in optf) {
      o <- tryCatch(readRDS(f), error = function(e) NULL)
      if (is.null(o)) { add("`", f, "` … 読み込めません"); next }
      add("`", f, "`: convergence = **", o$convergence, "**",
          "（0 なら収束）、logL = **", sprintf("%.6f", -o$value), "**")
      add("")
      judge(identical(as.integer(o$convergence), 0L),
            paste0("BFGS 収束（", f, "）"),
            if (identical(as.integer(o$convergence), 0L)) "収束している。"
            else "収束していない。maxit を増やすか初期値を見直すこと。")
    }
  }
}

# --- 6. メモリ自己検査 ------------------------------------------------------
sec("6. メモリの自己検査")
if (!DO_MEMTEST) {
  add("実施していません。行うには `--memtest`（既定 ", MEMTEST_GB,
      " GB、数分）を付けて実行してください。")
  add("")
} else {
  add("確保したメモリに既知のパターンを書き込み、読み戻して一致するかを見る。",
      "memtest86 ほど網羅的ではないが、**OS を止めずに、実際に使う領域**を検査できる。")
  add("")
  gb <- MEMTEST_GB
  n_per  <- 16 * 1024 * 1024                                  # 1ブロック 128MB (double)
  nblock <- max(1L, round(gb * 1024^3 / (n_per * 8)))
  add("- 検査量: ", gb, " GB（128MB × ", nblock, " ブロック）")
  add("")

  # 書いてすぐ読み返すだけでは、値がキャッシュに載ったままで主記憶を検査できない。
  # 全ブロックを確保しきってから検証することで、
  #   (a) 実際に N GB を常駐させ、
  #   (b) 最初のブロックは検証までに大量の他アクセスを挟む
  # ようにしている。
  pattern <- function(b) as.numeric(b) * 1e6 + seq_len(n_per) * 1.000000001

  t0 <- Sys.time()
  blocks <- vector("list", nblock)
  alloc_ok <- TRUE
  for (b in seq_len(nblock)) {
    blocks[[b]] <- tryCatch(pattern(b), error = function(e) NULL)
    if (is.null(blocks[[b]])) {
      alloc_ok <- FALSE
      add("- **", b - 1L, " ブロック目で確保に失敗**（メモリ不足）。以降は検査していない。")
      add("")
      nblock <- b - 1L
      break
    }
  }
  t_alloc <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  bad <- 0L; bad_blocks <- integer(0)
  if (nblock >= 1L) {
    for (b in seq_len(nblock)) {
      # 期待値をその場で作り直して突き合わせる（別の領域に確保される）
      if (!identical(blocks[[b]], pattern(b))) {
        bad <- bad + 1L; bad_blocks <- c(bad_blocks, b)
      }
    }
  }
  rm(blocks); gc(FALSE)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  add("- 確保: ", round(t_alloc), " 秒 / 全体: ", round(el), " 秒")
  add("- 検証したブロック: ", nblock)
  add("- 不一致ブロック: **", bad, "**",
      if (bad > 0L) paste0("（", paste(bad_blocks, collapse = ", "), "）") else "")
  if (!alloc_ok)
    add("- 確保に失敗したので、`--memtest-gb` を小さくして再実行すること。")
  add("")
  judge(bad == 0L, paste0("メモリ自己検査（", gb, " GB）"),
        if (bad == 0L) "検査した範囲では不一致なし。"
        else "不一致が出た。解析を止めてメモリを交換すること。")
  add("**注意: これは軽量な検査で、memtest86 の代わりにはならない。**",
      "不良ランクが BIOS でマップアウトされていれば、そもそも OS からは見えない。")
  add("")
}

# --- 7. 判定 ---------------------------------------------------------------
sec("7. 判定")
if (!length(judgments)) {
  add("判定できる材料がありませんでした。")
} else {
  add("| | 項目 | 所見 |"); add("|---|---|---|")
  for (j in judgments)
    add("| ", if (j$ok) "OK" else "**要確認**", " | ", j$label, " | ", j$detail, " |")
  add("")
  nbad <- sum(!vapply(judgments, `[[`, logical(1), "ok"))
  if (nbad == 0L) add("**すべて問題なし。解析を進めてよい。**")
  else add("**要確認が ", nbad, " 件。上の表を確認すること。**")
}
add("")
add("---")
add("")
add("*`tests/machine_check.R` が生成。数値はすべて実行時に取得したもの。*")

# --- 保存 -------------------------------------------------------------------
dir.create("results", showWarnings = FALSE)
outfile <- sprintf("results/machine_check_%s_%s.md", host, stamp)
writeLines(out, outfile, useBytes = TRUE)

cat(paste(out, collapse = "\n"), "\n\n")
cat("保存:", outfile, "\n")
cat("このファイルを commit / push すれば編集機側から確認できます。\n")
