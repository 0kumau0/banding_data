### Make data for analyse and plot 
library(foreign)
library(tidyverse)
library(sf)
library(dplyr)
library(purrr)
library(jpmesh)
Sys.setlocale("LC_ALL", "Japanese_Japan.932")
source("functions.R", encoding = "UTF-8")

# Clean data ---------------------------------------------------------------
#read.data
data <- read.dbf("../../6118LANDBIRD.DBF", as.is = T)
#PCODE910053の地点は110099に書き換え
data <- data %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))
splist <- read.dbf("../../Splist.DBF", as.is = T)
place　<- read.dbf("../../PLACE.DBF", as.is = T)
place <- place %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))
place <- place %>% filter(PCODE!=380036) #石鎚山のおかしなデータ除去　2025年12月4日　データ作成もう一度やりなおし
migratory <- read.csv("../../migratory.csv") #made from Javian database


# Make band_data
#convert the location coordinates in place data from degrees and minutes to decimal degrees
place$Lat <- sapply(place$LAT, convert_to_decimal)
place$Lon <- sapply(place$LONG, convert_to_decimal)

# #type conversion
# data <- data %>%
#   mutate(PCODE = as.character(PCODE),
#          GUID = as.character(GUID),
#          RING = as.character(RING),
#          SPC = as.character(SPC))
# 
# #Extract YEAR
# data$YEAR <- substr(as.character(data$DAY),1, 4) %>% as.numeric()
# 
# #add katakana names
# data <- data %>% left_join(splist %>% dplyr::select(SPC,SPNAMK,SPNAME), by = "SPC")
# 
# #filter data for the past 10 years
# R.data <- data %>% filter(YEAR > 2008)
# 
# # Remove duplication
# # filter STAT only "N"
# stat_n_data <- data %>% filter(STAT == "N")
# 
# # extract groups that have same GUID & RING but more than 2 rows of "N" in STAT
# duplicate_n_groups <- stat_n_data %>%
#   group_by(GUID, RING) %>%
#   filter(n() > 1) %>%
#   ungroup()
# 
# # extract groups that have same GUID & RING & SPC but more than 2 rows of "N" in STAT
# duplicate_n_samesp_groups <- stat_n_data %>%
#   group_by(GUID, RING,SPC) %>%
#   filter(n() > 1) %>%
#   ungroup()
# 
# # extract groups that have same GUID & RING and different SPCs
# differentsp_group <- data %>%
#   group_by(GUID, RING) %>%
#   filter(n_distinct(SPNAMK) > 1) %>%  # 異なる種名が含まれているグループだけを抽出
#   ungroup()
# 
# #remove duplication from data
# dup_N_list <- duplicate_n_samesp_groups %>% mutate(indID = as.character(paste0(PCODE,NO,DAY,GUID,RING)),
#                                                    indID = trimws(indID))
# dup_dsp_list <- differentsp_group %>% mutate(indID = as.character(paste0(PCODE,NO,DAY,GUID,RING)),
#                                              indID = trimws(indID))
# 
# R.data <- R.data %>%
#   mutate(indID = as.character(paste0(PCODE,NO,DAY,GUID,RING)),
#          indID = trimws(indID)) %>%
#   anti_join(dup_N_list, by = "indID") %>%
#   anti_join(dup_dsp_list, by = "indID") %>%
#   dplyr::select(-indID)
# 
# # #save R.data
# R.data %>%
#   write.csv("../band_data_20251204.csv", row.names = FALSE)

band_data <- read_csv("../band_data_20251204.csv", 
                      col_types = cols(PCODE = col_character(),
                                       RING = "character",
                                       GUID = "character",
                                       SPC = "character"),
                      locale = locale(encoding = "shift_jis")
)
colname <- names(band_data)
band_place <- band_data %>% left_join(place, by = "PCODE")
#band_place$mesh <- coords_to_mesh(band_place$Lon, band_place$Lat, mesh_size = 2) #めちゃくちゃ時間かかる。やらない
band_place <- band_place %>% st_as_sf(coords = c("Lon","Lat"), crs=4326)
band_place <- band_place %>% st_transform(crs=3100)

#band_place %>% filter(grepl("^高知県", ADDRESS)) #文字列でのデータの取り出し

#remove islands data using mesh polygon range
mesh <- st_read("../../griddata/mesh2_convex7.gpkg") %>% 
  mutate(meshcode2 = row_number())
poly <- st_read("../../griddata/QGIS/poly_20251210.shp") #3.ポリゴンの～.shpと同じ
band_place <- 
  st_join(band_place, mesh, join = st_within, left = FALSE) 

#ggplot () + geom_sf(data=poly) +geom_sf(data= band_data) #"../plots/pointplot_20251210.png"
band_data <- band_place %>% 
  st_drop_geometry() %>% 
  dplyr::select(colname, meshcode2) %>% 
  rename(meshcode = meshcode2)


# Make data for ADCR model ------------------------------------------------

# /make effort data --------------------------------------------------------
effort <- make.effort2(band_data)

# /make detection data -----------------------------------------------------
splist <- count_individuals(band_data)
#splist_full <- count_individuals(data)

#各種ごとのデータセットを作成してリストに格納
Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")
library(stringi)
library(Matrix)

detect_list <- make.detect2(band_data, num=30, effort)

#Make lists
band_data_list <- list()
band_data_list$splist <- splist %>% 
  stri_trans_general("Halfwidth-Fullwidth")
band_data_list$effort <- effort
band_data_list$detect_list <- detect_list

#saveRDS(band_data_list, "../band_data_list_30sp_20251205.rds")
#band_data_list <- readRDS("../band_data_list_30sp_20251205.rds") #10年分データ、30種のデータ 変なデータ除去20251205

#saveRDS(band_data_list, "../band_data_list_30sp_20251217.rds") #各メッシュ、年毎にデータを集計
#saveRDS(band_data_list, "../band_data_list_30sp_20251218.rds")

#band_data_list <- readRDS("../band_data_list_30sp_20251205.rds") #10年分データ、30種のデータ 変なデータ除去20251205
#band_data_list <- readRDS("../band_data_list_30sp_20251218.rds") 


#呼び出したい種のリスト番号の取り出し
which(band_data_list$splist == "シジュウカラ")

# ADCR model parameter -------------------------------------------------------------
library(cowplot)
library(lubridate)
library(raster)
library(gdistance)
library(viridis)
library(secr)

sourcepath<-"secrad_SGD.r"
source(sourcepath, encoding = "UTF-8")

# place <- place %>% dplyr::select(PCODE,Lat,Lon)
# effort <- band_data_list$effort %>% left_join(place %>% mutate(PCODE = as.character(PCODE)), by = "PCODE")
# 
# #sf変換＆UTM変換
# effort_st <- st_as_sf(effort, coords = c("Lon", "Lat"), crs = 4326)
# effort_utm <- st_transform(effort_st, crs = 3100)


# # 座標を抽出して元のデータに追加
# coords <- st_coordinates(effort_utm)
# effort$x <- coords[, "X"]
# effort$y <- coords[, "Y"]
# effort_st<-effort%>%st_as_sf(coords=c("x","y"),crs=3100)

#read griddata
ngrid<-1
dataset<-list()
#3次メッシュ ↓とどちらか
#dataset$griddata <- st_read("C:\\Users\\Kumada\\banding data\\griddata\\mesh3_2.shp")
#2次メッシュ
dataset$griddata <- st_read("../../griddata/mesh2_convex7.gpkg") %>% 
  mutate(meshcode2 = row_number()) %>% 
  rename(oldmeshcode2 = meshcode) %>% 
  rename(meshcode = meshcode2) 

#中心点
dataset$griddata <- st_transform(dataset$griddata, crs = 3100)
cent <- st_centroid(dataset$griddata)
coords <- st_coordinates(cent)
dataset$griddata <- dataset$griddata %>%
  mutate(x = coords[, "X"],
         y = coords[, "Y"])

#dataset
coords<-dataset$griddata[,c("x","y")]/1000	#km
coords<-coords%>%as_tibble%>%dplyr::select(-geometry)
dataset$coords<-coords%>%as.matrix

# 各グリッドセル間の距離を求める準備
ncell <- nrow(coords)  # グリッドセルの数

# 各方向のペアワイズ距離行列（絶対値をとることで距離に）
xdist <- abs(outer(coords$x, rep(1, ncell)) - outer(rep(1, ncell), coords$x)) 
ydist <- abs(outer(coords$y, rep(1, ncell)) - outer(rep(1, ncell), coords$y))

#最小の各方向距離（隣接セル間距離?）
dx <- min(xdist[xdist != 0])
dy <- min(ydist[ydist != 0])

# グリッドセル面積（km²）
area <- rep(100, nrow(dataset$griddata)) #2次メッシュの時
dataset$area <- area

# 土地被覆情報の取得と整形
grid_cov <- dataset$griddata %>%
  as_tibble() %>%
  mutate(
    agri = cultivated,  # 農地面積
    wtr = openwater     # 水域面積
  ) %>%
  dplyr::select(agri, wtr, -geom)  # geometry列を除外

# 各変数（agri, wtr）の平均と標準偏差を計算（標準化のため）
mu_agri <- mean(grid_cov$agri)
sd_agri <- sd(grid_cov$agri)
mu_wtr  <- mean(grid_cov$wtr)
sd_wtr  <- sd(grid_cov$wtr)

# agri,wtr列を標準化　
grid_cov_std <- grid_cov %>%
  mutate(agri = (agri - mu_agri) / sd_agri)
grid_cov_std <- grid_cov_std %>%
  mutate(wtr = (wtr - mu_wtr) / sd_wtr)

# 標準化後の土地被覆情報を保存
dataset$grid_cov_std <- grid_cov_std

# 標準化の際に使用した平均と標準偏差を保存
dataset$grid_cov_musd <- data.frame(mu = c(mu_agri, mu_wtr),
                                    sd = c(sd_agri, sd_wtr))

# 最小隣接セル間距離をリストとしてまとめる
resolution <- c(x = dx, y = dy)
dataset$resolution<-resolution

#effortをデータに追加
dataset$effort <- effort$effort #bandデータのeffortは1調査あたりすべて同じととりあえずする

#dataset$griddataのどのグリッドに各effortの点が含まれるかをgriddataの行番号として取得
#格子ライン上に乗った点について、両方のセル情報が加わってしまうので、最初の一つだけ採択
#effort_loc_list <- st_intersects(effort_st, dataset$griddata)
# gtable <- dataset$griddata %>% 
#   group_by(meshcode) %>%
#   slice_head(n = 1) %>% 
#   ungroup() %>% 
#   mutate(rownumber = row_number(.))
gtable <- dataset$griddata %>% 
  mutate(rownumber = row_number()) %>% 
  mutate(meshcode = as.character(meshcode))
  
effort_loc <- left_join(effort, gtable, by = "meshcode") %>% 
  dplyr::select(rownumber) %>% 
  unlist() %>% unname() 
dataset$effort_loc<-effort_loc

# # 1地点に複数グリッドが対応している場合は、最初の1つだけを使う
# effort_loc <- sapply(effort_loc_list, function(x) if(length(x) > 0) x[1] else NA)
# dataset$effort_loc<-effort_loc

#データに追加
detect <- band_data_list$detect_list[[which(band_data_list$splist == "シジュウカラ")]]
dataset$detect<-detect
dataset$effort_occ<-effort$effort_occ

#状態空間格子の設定
secrdata<-secrad_data$new(coords=dataset$coords,
                          area=dataset$area,
                          grid_cov=dataset$grid_cov_std,
                          resolution=dataset$resolution)

#観測情報の追加
secrdata$add_obs(type="poisson",
                 effort=dataset$effort,
                 effort_loc=dataset$effort_loc,
                 effort_occ=dataset$effort_occ,
                 detect=dataset$detect)

#プロット
secrdata$ggsecraddata(covname="agri")
secrdata$ggsecraddata(covname="wtr")

secrad_obj<-secrad$new(secrdata=secrdata)
envmodel<-list(D~1,C~agri+wtr,A~0)
indmodel<-c(A=FALSE,g0=FALSE)
occmodel<-c(A=FALSE,g0=FALSE)
secrad_obj$set_model(envmodel=envmodel,indmodel=indmodel,occmodel=occmodel)

initpar<-generate_init(secrad_obj)
initpar["dens_0"]<--1
initpar["conn_0"]<--2
initpar["g0_1"]<--5

save.image("data_20260202_SGD.RData")

# --- SGD実装 (ADCRモデル実装ノート準拠) ---
library(numDeriv)

# 1. データの層別化とサンプリング設定
# dataset$detect は行が個体、列がトラップ/機会と仮定して rowSums で捕獲回数を計算
capture_counts <- rowSums(dataset$detect) 

multi_ids <- which(capture_counts > 1)   # 複数回捕獲個体 (全数使用)
single_ids <- which(capture_counts == 1)  # 1回のみ捕獲個体 (サンプリング対象)

# サンプリング設定 (実装ノート Step 1)
sampling_rate <- 0.1  # 10%
sample_size <- floor(length(single_ids) * sampling_rate)
weight_single <- 1 / sampling_rate # 重み

# 2. 重み付き尤度関数の定義 (実装ノート Step 2)
# ※注意: secrad.r 側の loglf が ids 引数を受け取れるよう修正されている必要があります
weighted_loglf <- function(par, ids_multi, ids_single, weight, obj) {
  
  # 複数回捕獲個体の対数尤度 (loglfscale=1 で正の対数尤度を取得)
  ll_multi <- obj$loglf(par, ids = ids_multi, loglfscale = 1)
  
  # サンプリングされた1回捕獲個体の対数尤度
  ll_single_sample <- obj$loglf(par, ids = ids_single, loglfscale = 1)
  
  # 全体の近似対数尤度: Total LL ≈ LL(Multi) + LL(Single_sampled) * Weight
  total_ll <- ll_multi + (ll_single_sample * weight)
  
  return(total_ll)
}

# 3. 最適化ループ (SGD) (実装ノート Step 3)
current_par <- initpar
learning_rate <- 0.01  # 学習率
max_iter <- 1000       # 最大反復回数

cat("Starting SGD Optimization...\n")

for(iter in 1:max_iter) {
  
  # ステップA: 1回捕獲個体の再サンプリング (Stochasticity)
  current_single_sample <- sample(single_ids, size = sample_size)
  
  # ステップB: 勾配計算 (Gradient Ascent for Maximum Likelihood)
  # weighted_loglf の勾配を求める
  g <- numDeriv::grad(func = weighted_loglf, 
                      x = current_par, 
                      ids_multi = multi_ids,
                      ids_single = current_single_sample, 
                      weight = weight_single, 
                      obj = secrad_obj)
  
  # ステップC: パラメータ更新 (勾配方向へ加算)
  current_par <- current_par + g * learning_rate
  
  # ログ出力 (適宜間引いてください)
  if(iter %% 10 == 0 || iter == 1) {
    # 現在の推定尤度を計算（モニタリング用）
    current_ll <- weighted_loglf(current_par, multi_ids, current_single_sample, weight_single, secrad_obj)
    cat(sprintf("Iter: %d, LL: %.2f, Pars: %s\n", iter, current_ll, paste(round(current_par, 4), collapse=", ")))
  }
}

# 4. 結果の格納 (optimの出力形式に合わせて整形)
secrad_res <- list(
  par = current_par,
  value = weighted_loglf(current_par, multi_ids, sample(single_ids, sample_size), weight_single, secrad_obj), # 最終的な近似尤度
  counts = c(function = max_iter, gradient = max_iter),
  convergence = 0,
  message = "SGD optimization completed"
)

cat("SGD Optimization Finished.\n")

# Plotting data -----------------------------------------------------------
Japan <- st_read("S:\\common\\personal_backup\\kumada\\Virbsagi\\R\\Japan_merge2.shp")

#Count the number of ind for each location
Total <- band_data %>% 
  group_by(PCODE) %>% 
  tally()

Total <- Total %>% left_join(place %>% dplyr::select(PCODE,LAT,LONG))

#Remove PCODE910053
Total <- Total %>% filter(PCODE!="910053")

Total_sf <- st_as_sf(Total, coords = c("LONG", "LAT"), crs = 4326)
place_sf <- st_as_sf(place, coords = c("Lon", "Lat"), crs = 4326)

#distribution map
ggplot() +
  geom_sf(data = Japan) +
  geom_sf(data =Total_sf, aes(cex = n)) +
  theme_minimal()

#bargraph of num of catch per individual
folder <- "../plots/" #select save folder
Num_capture_plot_all(R.data, folder)




