#2次メッシュの作成
# 必要パッケージ
library(sf)
library(dplyr)
library(purrr)
library(jpgrid)   # JISメッシュ対応（海上もOK）
library(ggplot2)
library(concaveman)
library(foreign)
library(readr)
source("functions.R", encoding = "UTF-8")


Japan <- st_read("S:\\common\\personal_backup\\kumada\\Virbsagi\\R\\Japan_merge2.shp")
#"C:\Users\Kumada\Documents\banding data\git\banding_data\make_data&plot.R"の186まで適宜実行し、effort_stを作成しておく
place　<- read.dbf("../../PLACE.DBF", as.is = T)
place <- place %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))
#Count the number of ind for each location
Total <- band_data %>% 
  group_by(PCODE) %>% 
  tally()

Total <- Total %>% left_join(place %>% dplyr::select(PCODE,Lat,Lon))
Total_sf <- st_as_sf(Total, coords = c("Lon", "Lat"), crs = 4326)

# Total_sf: tibble兼sfのPOINT (columns: PCODE, n, geometry)
# すでに CRS が WGS84 (EPSG:4326) である前提

# # 外接矩形をsfcポリゴン化（少しバッファを足すと周辺海域も含めやすい）
# bbox_poly <- Total_sf %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_buffer(dist = 0.2)  # 必要なら度単位で少し拡張。例: dist = 0.2
# 
# # 2次メッシュ（約10km）を範囲に交差するものだけ抽出
# mesh10km <- geometry_to_grid(bbox_poly, "10km") %>%   # "10km" = 2次メッシュ
#   first() %>%                                         # 単一範囲のため先頭要素
#   grid_as_sf(crs = st_crs(Total_sf))                  # sfポリゴン化
# 
# # 可視化
# ggplot() +
#   geom_sf(data = mesh10km, fill = NA, color = "grey50", size = 0.2) +
#   geom_sf(data = Total_sf, aes(size = n), color = "black", alpha = 0.6) +
#   scale_size_continuous(name = "n") +
#   coord_sf() 


convex_poly <- effort_st %>%
  st_union() %>%
  st_convex_hull()             # 凸包（海側を含む外郭の簡易近似）

mesh10km_convex <- geometry_to_grid(convex_poly, "10km") %>%
  first() %>%
  grid_as_sf(crs = st_crs(effort_st))

ggplot() +
  geom_sf(data = mesh10km_convex, fill = NA, color = "grey40") +
  geom_sf(data = effort_st, color = "black", size = 0.8) +
  geom_sf(data = convex_poly, fill = NA, color = "red") 

mesh10km_convex_gpkg <- mesh10km_convex %>%
  mutate(meshcode = as.character(grid)) %>%
  dplyr::select(meshcode, everything(), -grid)

st_write(mesh10km_convex_gpkg,
         dsn = "../../griddata/mesh2_convex2.gpkg",
         layer = "mesh2_convex",
         delete_layer = TRUE)

#環境情報付与
landuse2 <- read.csv("S:\\common\\personal_backup\\kumada\\国土標準土地利用データ\\landuse2.csv")
landuse2 <- landuse2 %>% 
  mutate(id = as.character(MESH2_ID))

mesh10km_convex_gpkg <- mesh10km_convex_gpkg %>%
  mutate(meshcode = as.character(meshcode))

mesh_joined <- mesh10km_convex_gpkg %>%
  st_drop_geometry() %>%  # geometryを外す
  left_join(
    landuse2 %>% dplyr::select(id, cultivated, openwater),
    by = c("meshcode" = "id")
  ) %>%
  mutate(
    cultivated = ifelse(is.na(cultivated), 0, cultivated),
    openwater = ifelse(is.na(openwater), 100000000, openwater)
  ) %>%
  bind_cols(geometry = st_geometry(mesh10km_convex_gpkg)) %>%  # geometryを戻す
  st_as_sf()

st_write(mesh_joined,
         dsn = "../../griddata/mesh2_convex3.gpkg",
         layer = "mesh2_convex",
         delete_layer = TRUE)

# 
# # concaveman で凹包（alphaを小さくすると細かく追随、計算時間↑）
# concave_poly <- concaveman::concaveman(Total_sf, length_threshold = 0, concavity = 2)
# 
# mesh10km_concave <- geometry_to_grid(concave_poly, "10km") %>%
#   first() %>%
#   grid_as_sf(crs = st_crs(Total_sf))
# 
# ggplot() +
#   geom_sf(data = mesh10km_concave, fill = NA, color = "grey40") +
#   geom_sf(data = Total_sf, color = "black", size = 0.8) +
#   geom_sf(data = concave_poly, fill = NA, color = "blue") +
#   coord_sf() + theme_bw()

  