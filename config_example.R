# config_example.R -----------------------------------------------------------
#
# マシンごとに違うパスをここ1箇所に集約する。
#
#   使い方: このファイルを config.R にコピーし、環境に合わせて書き換える。
#           config.R は .gitignore 済み（マシンごとに内容が違うため）。
#
#   スクリプト側:  source("config.R")
#
# 作業ディレクトリはリポジトリルート（git/banding_data）を前提にしている。
# 改変版 ADCR エンジンはリポジトリ内の adcrsgd/secrad.r にあるため、
# ここでは扱わない（sourcepath <- "adcrsgd/secrad.r"）。
# ---------------------------------------------------------------------------

# --- ルート -----------------------------------------------------------------

# banding data フォルダ。このリポジトリの2階層上。
DATA_ROOT <- "../.."

# ADCR 関連。ADCR_ROOT/adcrtest2 は本家 kfukasawa37/adcrtest2 のクローン（素の状態）。
ADCR_ROOT <- file.path(DATA_ROOT, "ADCR")

# 検証用のクマデータ（Fukasawa & Higashide 2025 の Dryad データ）
BEAR_ROOT <- file.path(ADCR_ROOT, "doi_10_5061_dryad_ksn02v7bq__v20250117")

# 中間生成物の置き場（band_data_*.csv / band_data_list_*.rds がある git/ 直下）
WORK_ROOT <- ".."

# グリッド・メッシュ関連
GRID_ROOT <- file.path(DATA_ROOT, "griddata")

# --- 個別ファイル -----------------------------------------------------------

BEAR_EFFORT <- file.path(BEAR_ROOT, "effort_231225.csv")
BEAR_DETECT <- file.path(BEAR_ROOT, "detectmat_231225.csv")
BEAR_MESH   <- "meshutm_0.5km_buff_land"        # st_read(BEAR_ROOT, BEAR_MESH)

# 足環データの中間生成物。段階によって差し替わるので、手元にあるものを指すこと。
# （このPCには 20251205 / 20251217 / kanto / 素の4種類がある）
BAND_RDS    <- file.path(WORK_ROOT, "band_data_list_30sp_20251217.rds")
BAND_CSV    <- file.path(WORK_ROOT, "band_data_20251204.csv")

# --- 存在確認 ---------------------------------------------------------------
#
# パスがずれていたとき、スクリプトの奥で「file not found」になるのではなく
# ここで「config.R のどの変数が違うか」を明示して止める。

config_check <- function(vars = c("DATA_ROOT", "ADCR_ROOT", "BEAR_ROOT",
                                  "BEAR_EFFORT", "BEAR_DETECT"),
                         fatal = TRUE) {
  missing <- character(0)
  for (v in vars) {
    if (!exists(v)) { missing <- c(missing, paste0(v, " (未定義)")); next }
    p <- get(v)
    if (!file.exists(p)) missing <- c(missing, sprintf("%s = %s", v, p))
  }
  if (length(missing) == 0L) {
    cat("config: OK (", length(vars), "件のパスを確認)\n", sep = "")
    return(invisible(TRUE))
  }
  msg <- paste0("config.R のパスが見つかりません:\n  ",
                paste(missing, collapse = "\n  "),
                "\n作業ディレクトリ: ", getwd(),
                "\nリポジトリルート（git/banding_data）で実行しているか確認してください。")
  if (fatal) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  invisible(FALSE)
}
