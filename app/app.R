## Filename:    app.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Main app page

###### Load in Packages ######

library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(data.table)
library(DT)
library(dplyr)
library(stringr)
library(tidyr)
library(emo)

###### Defining Paths and Importing Data ######

data_path <- file.path("data", "output")

label_sources <- data.table(readRDS(file.path(data_path, "label_sources.rds")))
labels <- data.table(readRDS(file.path(data_path, "labels.rds")))
regional <- data.table(readRDS(file.path(data_path, "regional.rds")))
workload <- data.table(readRDS(file.path(data_path, "workload.rds")))

###### Sourcing Helper Functions ######

source(file.path("helpers.R"), local=TRUE)$value
source(file.path("panels", "home.R"), local=TRUE)$value
source(file.path("panels", "summary.R"), local=TRUE)$value
source(file.path("panels", "issuances.R"), local=TRUE)$value
source(file.path("panels", "workload.R"), local=TRUE)$value
source(file.path("panels", "regional.R"), local=TRUE)$value

###### Defining UI ######

ui <- fluidPage(
  theme = shinytheme("simplex"),
  
  fluidRow(
    column(1),
    column(10,
           
           ## heading
           h2(emo::ji("red_heart"), "US Non-Immigrant Visa Data Explorer", emo::ji("blue_heart")),
           p("Created by ", a(href="https://angelinahli.com/", "Angelina Li"), " in Jan 2020", 
             emo::ji("diamond_shape_with_a_dot_inside"),
             a(href="https://github.com/angelinahli/visas/", "Source code")),
           hr(),
           
           ## tabs
           div(
             tabsetPanel(
               type="pills",
               
               home_ui(),
               summary_ui(),
               issuances_ui(),
               workload_ui(),
               regional_ui()
               
             )
           )
           
    )
  )
    
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