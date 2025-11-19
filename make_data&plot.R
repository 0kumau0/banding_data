### Make data for analyse and plot 
library(foreign)
library(tidyverse)
library(sf)
library(dplyr)
library(purrr)
Sys.setlocale("LC_ALL", "Japanese_Japan.932")
source("functions.R", encoding = "UTF-8")

# Clean data ---------------------------------------------------------------
#read.data
data <- read.dbf("../../6118LANDBIRD.DBF", as.is = T)
splist <- read.dbf("../../Splist.DBF", as.is = T)
place　<- read.dbf("../../PLACE.DBF", as.is = T)
migratory <- read.csv("../../migratory.csv") #made from Javian database

#convert the location coordinates in place data from degrees and minutes to decimal degrees
place$Lat <- sapply(place$LAT, convert_to_decimal)
place$Lon <- sapply(place$LONG, convert_to_decimal)

#type conversion
data <- data %>%
  mutate(PCODE = as.character(PCODE),
         GUID = as.character(GUID),
         RING = as.character(RING),
         SPC = as.character(SPC))

#Extract YEAR
data$YEAR <- substr(as.character(data$DAY),1, 4) %>% as.numeric() 

#add katakana names
data <- data %>% left_join(splist %>% dplyr::select(SPC,SPNAMK,SPNAME), by = "SPC")　

#filter data for the past 10 years
R.data <- data %>% filter(YEAR > 2008)

# Remove duplication --------------------------------------------------
# filter STAT only "N"
stat_n_data <- data %>% filter(STAT == "N")

# extract groups that have same GUID & RING but more than 2 rows of "N" in STAT
duplicate_n_groups <- stat_n_data %>%
  group_by(GUID, RING) %>%
  filter(n() > 1) %>%
  ungroup()

# extract groups that have same GUID & RING & SPC but more than 2 rows of "N" in STAT
duplicate_n_samesp_groups <- stat_n_data %>%
  group_by(GUID, RING,SPC) %>%
  filter(n() > 1) %>%
  ungroup()

# extract groups that have same GUID & RING and different SPCs
differentsp_group <- data %>% 
  group_by(GUID, RING) %>% 
  filter(n_distinct(SPNAMK) > 1) %>%  # 異なる種名が含まれているグループだけを抽出
  ungroup()

#remove duplication from data
dup_N_list <- duplicate_n_samesp_groups %>% mutate(indID = as.character(paste0(PCODE,NO,DAY,GUID,RING)),
                                                   indID = trimws(indID)) 
dup_dsp_list <- differentsp_group %>% mutate(indID = as.character(paste0(PCODE,NO,DAY,GUID,RING)),
                                             indID = trimws(indID))

R.data <- R.data %>%
  mutate(indID = as.character(paste0(PCODE,NO,DAY,GUID,RING)),
         indID = trimws(indID)) %>% 
  anti_join(dup_N_list, by = "indID") %>% 
  anti_join(dup_dsp_list, by = "indID") %>% 
  dplyr::select(-indID)

R.data <- R.data %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))

#save R.data
# R.data %>% 
#   write.csv("../band_data.csv", row.names = FALSE)


# Make data for ADCR model ------------------------------------------------

# make effort data --------------------------------------------------------
#effortは地点×日付数の行、effortはすべて1
effort <- R.data %>% 
  mutate(YEAR = substr(DAY, 1, 4)) %>% 
  #filter(YEAR > 2013) %>% 
  filter(YEAR > 2008) %>% #10年分データ
  distinct(PCODE, DAY)
effort <- effort %>% mutate(PCODE = as.character(.$PCODE))
effort <- effort %>%  
  group_by(PCODE) %>%
  arrange(DAY, .by_group = TRUE) %>% 
  mutate(effort_occ = row_number()) %>% 
  ungroup() %>% 
  mutate(effortID = row_number())
effort$effort <- rep(1, nrow(effort))
effort <- effort %>% mutate(PCODE = if_else(PCODE == "910053", "110099", PCODE))

# make detection data -----------------------------------------------------
splist <- count_individuals(R.data)
splist_full <- count_individuals(data)

#make list of dataset by sp 
detect_list <- effort_list <- list()
for (i in 1:length(splist)){ 
  #種を選択
  spp <- R.data %>% filter(SPNAMK == splist[i])
  
  #調査ID付与
  spp <- spp %>% mutate(kaiID = paste0(PCODE,DAY))
  
  #effortのほうのeffortIDと結合 effortのほうのeffortIDをすべて保持
  spp_1 <- spp %>% dplyr::select(PCODE, kaiID)
  
  #出現マトリクスの作成
  presense_matrix <- spp %>% 
    mutate(individualID = paste0(GUID,RING),
           present = 1) %>% 
    full_join(
      effort %>% mutate(kaiID = paste0(PCODE,DAY)) %>% 
        dplyr::select(effortID, kaiID) ,
      by = "kaiID") %>% 
    dplyr::select(effortID, individualID, present) %>% 
    pivot_wider(names_from = individualID, values_from = present, values_fill = 0) %>% 
    dplyr::select(where(~ !anyNA(.)))
  
  #effort_list[[i]] <- spp_effort
  detect_list[[i]] <- presense_matrix
}


# Plotting data -----------------------------------------------------------
Japan <- st_read("S:\\common\\personal_backup\\kumada\\Virbsagi\\R\\Japan_merge2.shp")

#Count the number of ind for each location
Total <- R.data %>% 
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




