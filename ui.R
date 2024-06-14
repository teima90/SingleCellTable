library(DT)
library(tidyverse)
library(shiny)
library(reactable)
library(shinydashboard)
library(fresh)
library(sparkline)

create_theme(
  theme = "default",
  bs_vars_navbar(
    default_bg = "#3f2d54",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF"
  ),
  bs_vars_color(
    gray_base = "#354e5c",
    brand_primary = "#75b8d1",
    brand_success = "#c9d175",
    brand_info = "#758bd1",
    brand_warning = "#d1ab75",
    brand_danger = "#d175b8"
  ),
  bs_vars_state(
    success_text = "#FFF",
    success_bg = "#c9d175",
    success_border = "#c9d175",
    info_text = "#FFF",
    info_bg = "#3f2d54",
    info_border = "#3f2d54",
    danger_text = "#FFF",
    danger_bg = "#d175b8",
    danger_border = "#d175b8"
  ),
  bs_vars_wells(
    bg = "#FFF",
    border = "#3f2d54"
  ),
  output_file = "www/mytheme.css"
)




navbarPage(
  theme = "mytheme.css",
  title = "UMAP Navigation",
  selected="Table Visualization",
  tabPanel("Input files",
           fluidPage(
             fileInput("data_UMAP", label = "UMAP file", buttonLabel = "Upload...",
                       accept = c(".csv", ".txt")),
             fileInput("data_markers", label = "Markers file", buttonLabel = "Upload...",
                       accept = c(".csv", ".txt"))
           )
  ),
  tabPanel("Table Visualization",
           dashboardPage(
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             
             dashboardBody(
              fluidRow(
                 box(title = "Filters",
                     solidHeader = TRUE,
                     width = 2,
                     background = 'yellow',
                     radioButtons("radbut", label = "", 
                                  choices = c("All genes",
                                              "Only genes expressed in")),
                     
                     conditionalPanel(
                       condition = "input.radbut !== 'All genes'",
                       uiOutput("clusterChoices")
                     )
                 ),
                 box(title= "Gene expression",
                     status = "warning",
                     solidHeader = FALSE,
                     width = 6,
                     DTOutput("dataViz")
                     ),
                 
                 box(title= "UMAP plot",
                     status = "warning", 
                     solidHeader = FALSE,
                     width = 4,
                     plotOutput("UMAPplot"))
               )
           )
             
           )
  )
)



