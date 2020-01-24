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
source(file.path("panels", "analysis.R"), local=TRUE)$value
source(file.path("panels", "info.R"), local=TRUE)$value

###### Defining UI ######

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans:400,700|Merriweather&display=swap")
  ),
  
  fluidRow(
    column(1),
    column(10,
           
           ## heading
           h2(strong("US Non-Immigrant Visa Explorer"), class="text-title"),
           p("Created by ", a(href="https://angelinahli.com/", "Angelina Li"), " in Jan 2020 - ", 
             a(href="https://github.com/angelinahli/visas/", "Source code"),
             class="lead text-grey"),
           hr(),
           
           ## tabs
           div(
             tabsetPanel(
               type="tabs",
               
               analysis_ui(),
               info_ui()
               
             )
           )
           
    ),
    class="text-black"
  )
    
)

###### Defining Server ######

server <- function(input, output, session) {
  
  analysis_server(input, output, session)
  info_server(input, output, session)
  
}

###### Run the app ######

shinyApp(ui = ui, server = server)