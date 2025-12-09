#codes of functions for banding data

#convert the location coordinates in place data from degrees and minutes to decimal degrees--------------------
# function to convert
convert_to_decimal <- function(coord) {
  deg <- floor(coord)
  min <- (coord - deg) * 100
  return(deg + min / 60)
}


# plotting num of catch per ind -------------------------------------------
#bargraph of num of catch per individual (all spcies)
Num_capture_plot_all <- function(df,folder){
  # 個体ごとの捕獲回数（GUID + RING で個体を識別）
  individual_counts <- df %>%
    mutate(indiv_id = paste(GUID, RING, sep = "_")) %>%
    group_by(SPNAMK, indiv_id) %>%
    summarise(capture_count = n(), .groups = "drop")
  
  # 捕獲回数ごとの個体数（各種 × 回数ごと）
  capture_freq <- individual_counts %>%
    group_by(SPNAMK, capture_count) %>%
    summarise(n_individuals = n(), .groups = "drop")
  
  # 1回と2回以上に分ける
  once_counts_top <- capture_freq %>%
    filter(capture_count == 1)
  
  # グラフ表示の順番を総個体数順にする
  species_order <- capture_freq %>%
    group_by(SPNAMK) %>%
    summarise(total = sum(n_individuals), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(SPNAMK)
  
  # factor化して順序を指定
  once_counts_top <- once_counts_top %>%
    mutate(SPNAMK = factor(SPNAMK, levels = species_order))
  
  repeated_counts_top <- capture_freq %>%
    filter(capture_count >= 2)
  
  repeated_counts_top <- repeated_counts_top %>%
    mutate(SPNAMK = factor(SPNAMK, levels = species_order))
  
  # 最大捕獲回数を取得
  max_capture <- max(capture_freq$capture_count)
  
  # x軸を factor にして水準を統一
  repeated_counts_top <- repeated_counts_top %>%
    mutate(capture_count = factor(capture_count, levels = 2:max_capture))
  
  #総個体数順に20ずつ分割
  species_groups <- split(species_order, ceiling(seq_along(species_order) / 20))
  
  # グラフ描画
  capture_plot <- list()
  
  for (i in seq_along(species_groups)) {
    species_subset <- species_groups[[i]]
    
    repeated_subset <- repeated_counts_top %>%
      filter(SPNAMK %in% species_subset) %>%
      mutate(SPNAMK = factor(SPNAMK, levels = species_subset))  # 並び順を固定
    
    once_subset <- once_counts_top %>%
      filter(SPNAMK %in% species_subset) %>%
      mutate(SPNAMK = factor(SPNAMK, levels = species_subset))
    
    p <- ggplot(repeated_subset, aes(x = capture_count, y = n_individuals)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = n_individuals), vjust = 0.1, size = 2.5) +
      geom_text(
        data = once_subset,
        aes(label = paste0("1回: ", n_individuals), y = 0),
        color = "red", vjust = -3, hjust = -0.8
      ) +
      facet_wrap(~ SPNAMK, scales = "free_y") +
      theme_bw() +
      labs(
        title = paste("個体ごとの捕獲回数分布（上位種） - グループ", i),
        x = "捕獲回数（2回以上）",
        y = "個体数"
      )
    
    capture_plot[[i]] <- p
  }
  
  for (i in seq_along(capture_plot)) {
    #make a forder for saving plot
    ggsave(filename = paste0(folder, "capture_plot_group_", i, ".png"), plot = capture_plot[[i]], width = 10, height = 8)
  }
}  


# Make effort data --------------------------------------------------------
make.effort <- function(df){
  effort <- df %>% 
    distinct(PCODE, DAY) %>% 
    mutate(PCODE = as.character(.$PCODE))
  effort <- effort %>%  
    group_by(PCODE) %>%
    arrange(DAY, .by_group = TRUE) %>% 
    mutate(effort_occ = row_number()) %>% #sequential No with in each location(PCODE)
    ungroup() %>% 
    mutate(effortID = row_number())  # sequential No. across the dataset
  effort$effort <- rep(1, nrow(effort))
  effort
}


# Make detect data -----------------------------------------------------
make.detect <- function(df, num = 30, effort){ #detect data を作る種数,　デフォルト出現上位30
  detect_list <- list()
  for (i in 1:num){
    spp <- df %>% filter(SPNAMK == splist[i])
    spp <- spp %>% mutate(kaiID = paste0(PCODE,DAY)) #調査ID付与
    
    #出現matrixの作成
    spp.df <- spp %>% 
      mutate(individualID = paste0(GUID,RING),
             present = 1) %>%
      full_join(
        effort %>% mutate(kaiID = paste0(PCODE,DAY)) %>%
          dplyr::select(effortID, kaiID) ,
        by = "kaiID") %>%
      dplyr::select(effortID, individualID, present)
    spp.df$effortID <- factor(spp.df$effortID, exclude = NULL)
    spp.df$individualID <- factor(spp.df$individualID, exclude = NULL)
    spp.df <- spp.df %>%
      mutate(present = ifelse(is.na(present), 0, present))
    # 疎行列を作成 
    mat <- sparseMatrix(
      i = as.integer(spp.df$effortID),
      j = as.integer(spp.df$individualID),
      x = spp.df$present,
      dims = c(length(levels(spp.df$effortID)), length(levels(spp.df$individualID))),
      dimnames = list(levels(spp.df$effortID), levels(spp.df$individualID))
    )
    detect_list[[i]] <- mat
  }
  detect_list
}


# SortSpbyCapturedNumber --------------------------------------------------
count_individuals <- function(df){
  # 個体ごとの捕獲回数（GUID + RING で個体を識別）
  individual_counts <- df %>%
    mutate(indiv_id = paste(GUID, RING, sep = "_")) %>%
    group_by(SPNAMK, indiv_id) %>%
    summarise(capture_count = n(), .groups = "drop")
  
  # 捕獲回数ごとの個体数（各種 × 回数ごと）
  capture_freq <- individual_counts %>%
    group_by(SPNAMK, capture_count) %>%
    summarise(n_individuals = n(), .groups = "drop")
  
  # グラフ表示の順番を総個体数順にする
  species_order <- capture_freq %>%
    group_by(SPNAMK) %>%
    summarise(total = sum(n_individuals), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(SPNAMK)
}
