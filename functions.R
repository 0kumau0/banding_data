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