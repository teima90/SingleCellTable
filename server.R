source("functions.R")
#libraries
library(tidyverse)
library(ggplot2)
library(ggforce)  # for geom_circle
library(ggnewscale)
library(scales)



# Increase the maximum file upload size to 100MB
options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB


#Helper function to detect the separator
detectSeparator <- function(filePath) {
  firstLine <- readLines(filePath, n = 1)
  if (grepl(",", firstLine)) {
    return(",")
  } else if (grepl("\t", firstLine)) {
    return("\t")
  } else if (grepl(";", firstLine)) {
    return(";")
  } else {
    return(" ")
  }
}


#Data processing functions
checkData <- function(dat){  TRUE  }

modifUmap <- function(dataUmap){
  if(checkData(dataUmap)){
    dataUmap_modif <- dataUmap
    dataUmap_modif$cluster <- as.factor(dataUmap_modif$cluster)
    return(dataUmap_modif)}
  else{ return(NULL)  }  
}

modifMarkers <- function(dataMarkers){
  if(checkData(dataMarkers)){
    dataMarkers_modif <- dataMarkers[order(dataMarkers$gene), c(2:7)]
    dataMarkers_modif$gene <- as.character(dataMarkers_modif$gene)
    return(dataMarkers_modif)}
  else{ return(NULL)  }  
}


#Plotting function (substitute for Maite's)
createPlotForGene <- function(data,genename) {
  ggplot(data = subset(data, gene == genename), aes(x = pct.1, y = pct.2)) + geom_point()
}




#------#
#SERVER#
#------# 
function(input, output, session) {
  # Define reactive expressions for handling file input
  
  # Load default data
  default_umap_data <- read.delim("umap_root.txt", header = TRUE, check.names = FALSE, row.names = 1, sep=detectSeparator("umap_root.txt"))
  default_markers_data <- read.delim("markers_root_mini_2.txt", header = TRUE, check.names = FALSE, row.names = 1,sep=detectSeparator("markers_root_mini_2.txt"))
  
  # Reactive expressions for UMAP and markers data
  reactive_UMAP <- reactive({
    if (is.null(input$data_UMAP)) {
      return(default_umap_data)
    } else {
      req(input$data_UMAP)
      sep <- detectSeparator(input$data_UMAP$datapath)
      read.delim(input$data_UMAP$datapath, sep = sep, header = TRUE, check.names = FALSE, row.names = 1)
    }
  })
  
  reactive_MKRS <- reactive({
    if (is.null(input$data_markers)) {
      return(default_markers_data)
    } else {
      req(input$data_markers)
      sep <- detectSeparator(input$data_markers$datapath)
      read.delim(input$data_markers$datapath, sep = sep, header = TRUE, check.names = FALSE, row.names = 1)
    }
  })
  
  # 
  # reactive_UMAP <- reactive({
  #   req(input$data_UMAP)
  #   validate(need(tools::file_ext(input$data_UMAP$datapath) == "txt", "Please upload a umap.txt file"))
  #   sep <- detectSeparator(input$data_UMAP$datapath)
  #   read.delim(input$data_UMAP$datapath, sep = sep, header = TRUE, check.names = FALSE, row.names = 1)
  # })
  # 
  # reactive_MKRS <- reactive({
  #   req(input$data_markers)
  #   validate(need(tools::file_ext(input$data_markers$datapath) == "txt", "Please upload a markers.txt file"))
  #   sep <- detectSeparator(input$data_markers$datapath)
  #   read.delim(input$data_markers$datapath, sep = sep, header = TRUE, check.names = FALSE, row.names = 1)
  # })
  
  
  # Define choices for the Cluster input
  output$clusterChoices <- renderUI({
    checkboxGroupInput("Clusters", "Clusters",
                       choices = unique(reactive_MKRS()$cluster),
                       selected = NULL,
                       inline = FALSE,
                       width = "100%")
  })
  
  

  
  #Plot the big umap
  output$UMAPplot <- renderPlot ({
    umap_coord <- modifUmap(reactive_UMAP())
    markers <- modifMarkers(reactive_MKRS())
    root_grid <- umap_2_grid(umap_coord)
    root_colors <- assign_colors(root_grid, markers)
    markers_grid <- markers %>%
      left_join(root_grid,by="cluster")
    plot_umap(umap_coord, root_colors$clusters)
    
  })

  
  #Display Data Table
  output$dataViz <- renderDT({
    #Load data
    umap_coord <- modifUmap(reactive_UMAP())
    markers <- modifMarkers(reactive_MKRS())
    markers <- markers %>%
      relocate(gene, .before = avg_log2FC) %>%
      mutate(gene= as.character(gene),
             avg_log2FC = round(avg_log2FC,2)) %>%
      select(-p_val_adj)
    
    
    #Generate the list of abstract plots for the table
    root_grid <- umap_2_grid(umap_coord)
    root_colors <- assign_colors(root_grid, markers)
    markers_grid <- markers %>%
      left_join(root_grid,by="cluster")
    plots_list <- lapply(unique(markers_grid$gene), function(gene) {
      plot_abstract(data_grid=root_grid, markers_grid, gene, n_clusters=length(root_colors$clusters), root_colors$fc)
    })
    
    names(plots_list) <- unique(markers_grid$gene)

  
  # Save each plot as an image
  # plot_paths <- sapply(seq_along(plots_list), function(i) {
  #   file_path <- file.path("www",sprintf("%s.png", names(plots_list)[[i]]))
  #   #file_path <- file.path(sprintf("plot_%d.png", i))
  #   ggsave(file_path, plot = plots_list[[i]], width = 5, height = 4)
  #   file_path
  # })
  
  plot_paths <- sapply(seq_along(plots_list), function(i) {
    file_path <- file.path("www", sprintf("%s.png", names(plots_list)[[i]]))
    if (!file.exists(file_path)) {  # Check if the file already exists
      ggsave(file_path, plot = plots_list[[i]], width = 5, height = 4)
    }
    file_path
  })
  
  
  #Create dataframe with HTML images
  dataMarkers_display <- markers %>%
    mutate(Plot = sapply(markers$gene, function(x) sprintf('<img src="%s.png" style="height:200px;"></img>', x))
           )
  

    #FINAL DISPLAY depending on filters

    #Radio buttons option 0: All genes
    if (input$radbut == "All genes"){
      datatable(dataMarkers_display,
                escape = FALSE,
                options = list(
                  columnDefs = list(list(visible = FALSE, targets = 0)),
                  searching = FALSE,
                  paging = FALSE
                ))
    }else{

    #Radio buttons option 1: Only genes expressed in
    if (input$radbut == "Only genes expressed in"){

      # Check if any clusters are selected; if not, show all data

      if (is.null(input$Clusters) || length(input$Clusters) == 0) {
        datatable(dataMarkers_display,
                  escape = FALSE,
                  options = list(
                    columnDefs = list(
                      list(visible = FALSE, targets = 0)),
                    searching = FALSE,
                    paging = FALSE
                  ))
      } else {

        filtered_dataMarkers_display <- dataMarkers_display %>%
          filter(cluster %in% input$Clusters) 

        datatable(filtered_dataMarkers_display,
                  escape = FALSE,
                  options = list(
                     columnDefs = list(
                       list(visible = FALSE, targets = 0)),
                    searching = FALSE,
                    paging = FALSE
                  ))
      }
    }

      }

  })
  }



