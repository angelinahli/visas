## Filename:    app.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Main app page

###### Load in Packages ######

library(shiny)
library(shinyBS)
library(ggplot2)
library(data.table)
library(dplyr)

###### Defining Paths and Helpers ######

data_path <- file.path("..", "data")

source(file.path("panels", "home.R"), local=TRUE)$value
source(file.path("panels", "summary.R"), local=TRUE)$value
source(file.path("panels", "issuances.R"), local=TRUE)$value
source(file.path("panels", "workload.R"), local=TRUE)$value
source(file.path("panels", "regional.R"), local=TRUE)$value

###### Defining UI ######
ui <- fluidPage(
  titlePanel("US Non-immigrant Visa Data"),
  
  tabsetPanel(
    type="pills",
    
    home_ui(),
    summary_ui(),
    issuances_ui(),
    workload_ui(),
    regional_ui()

  )
  
  # source_local_value(file.path(ui_path, "pathname.R")),
)

###### Defining Server ######

server <- function(input, output, session) {
  
  home_server(input, output, session)
  summary_server(input, output, session)
  issuances_server(input, output, session)
  workload_server(input, output, session)
  regional_server(input, output, session)
  
}

###### Run the app ######

shinyApp(ui = ui, server = server)