#Function to generate a grid based in the original coordinates of the umap
umap_2_grid <- function(data_coordinates){
  min_x <- min(data_coordinates$UMAP_1)+0.1
  min_y <- min(data_coordinates$UMAP_2)+0.1
  
  aggregated_data <- data_coordinates %>% 
    group_by(cluster) %>%
    summarise(
      Mean_X = mean(UMAP_1)+min_x,
      Mean_Y = mean(UMAP_2)+min_y
    ) %>%
    arrange(-Mean_Y, Mean_X)  # Sort primarily by Y then X
  
  # Divide Y-axis into ranges
  # Here we use quantile to create ranges, adjust the number of cuts as needed
  num_ranges <- ceiling(sqrt(n_distinct(aggregated_data$cluster)))  # Example to determine number of ranges
  cuts <- quantile(aggregated_data$Mean_Y, probs = seq(0, 1, length.out = num_ranges + 1))
  aggregated_data$Y_Range <- cut(aggregated_data$Mean_Y, breaks = cuts, include.lowest = TRUE, labels = FALSE)
  
  # Assign grid positions within each Y range
  aggregated_data <- aggregated_data %>%
    group_by(Y_Range) %>%
    mutate(
      col = rank(Mean_X),  # Rank by Median_X within each Y range
      row = as.integer(Y_Range)  # Use Y_Range as row identifier
    ) %>%
    ungroup()%>%
    select(cluster,row,col) %>% 
    arrange(row,col) %>% mutate(cluster = factor(cluster,
                                                 levels = unique(cluster)))
  
  return(aggregated_data)
}


#Function to check the levels of clusters and range of fc for coloring
assign_colors <- function(data_grid, data_markers){
  levels_clusters <- unique(data_grid$cluster)
  range_fc <- range(data_markers$avg_log2FC, na.rm = TRUE)
  return(list(clusters=levels_clusters, fc=range_fc))
}


#Function to plot the umap with the original data
plot_umap <- function(data_coord, levels_clusters){
  data_coord <- data_coord %>% mutate(cluster = factor(cluster,
                                                       levels = levels_clusters))
  n_clusters <- length(levels_clusters)
  label.df <- data.frame(cluster=levels(data_coord$cluster), label = paste0(levels(data_coord$cluster)))
  label.df_2 <- data_coord %>% 
    group_by(cluster) %>% 
    summarize(UMAP_1 = median(UMAP_1), UMAP_2 = median(UMAP_2)) %>% 
    left_join(label.df) %>% 
    mutate(cluster = factor(cluster,
                                  levels = levels_clusters))
  
  plot_umap <- ggplot(data_coord,aes(x=UMAP_1,y=UMAP_2,color=cluster))+geom_point(size=2,
                                                                                                  shape=18,stroke=0)+
    geom_label(size=(12*5/14),data=label.df_2,aes(x=UMAP_1,y=UMAP_2, label=cluster, fill=cluster),
               color="black",label.padding = unit(0.05, "lines"),label.r = unit(0.05, "lines"),label.size = 0.1)+
    scale_color_manual(values=hue_pal()(n_clusters))+ 
    scale_fill_manual(values=hue_pal()(n_clusters))+
    theme_void()+theme(legend.position = "none")
  
  return(plot_umap)
  
  
}

#Function to plot the abstract representation of the umap with the information of the marker genes

plot_abstract <- function(data_grid, markers_grid, gene, n_clusters, range_fc) {

  abstract_plot <-ggplot() +
    geom_tile(data = data_grid, aes(x = col, y = row, fill = cluster), alpha =1, colour = "black") +
    scale_fill_manual(values = hcl.colors(n_clusters, palette = "Dynamic")) +
    new_scale_fill() +
    geom_circle(data = markers_grid[markers_grid$gene==gene,], aes(x0 = col, y0 = row, r = pct.1 / 2, fill = avg_log2FC)) +
    scale_fill_gradient(low = "grey90", high = "black", limits = range_fc) +
    coord_fixed() +
    theme(panel.background=element_rect(fill="transparent"),
      plot.background = element_rect(fill="transparent"),
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.spacing = unit(0, "lines"),
      legend.position = "none",
      text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
      
  return(abstract_plot)
}



