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
library(shinyglide)
library(plotly)
library(data.table)
library(DT)
library(RColorBrewer)
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
source(file.path("panels", "faqs.R"), local=TRUE)$value
source(file.path("panels", "visas.R"), local=TRUE)$value
source(file.path("panels", "downloads.R"), local=TRUE)$value

###### Defining UI ######

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans&display=swap"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "img/favicon-16.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "img/favicon-32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "96x96", href = "img/favicon-96.png")
  ),
  
  div(
    ## heading
    h1(img(src="img/statue-of-liberty.svg", style="height: 60px; vertical-align: bottom;"),
       strong("US Non-Immigrant Visa Explorer"), 
       class="text-title"),
    get_spacer(15),
   
    ## tabs
    tabsetPanel(
      type="pills",
      analysis_ui(),
      faqs_ui(),
      visas_ui(),
      downloads_ui()
    ),
    class="text-black container"
  ),
  
  tags$footer(
    div("Created by ", a(href="https://angelinahli.com/", "Angelina Li"), " - Jan 2020 - ", 
      "Icons by ",
      a(href="https://www.flaticon.com/authors/freepik", title="Freepik", "Freepik"),
      " - ",
      a(href="https://github.com/angelinahli/visas/", "Source code"),
      class="text-grey", style="font-size: 15px"))
  )

###### Defining Server ######

server <- function(input, output, session) {
  
  analysis_server(input, output, session)
  faqs_server(input, output, session)
  visas_server(input, output, session)
  downloads_server(input, output, session)

}

###### Run the app ######

shinyApp(ui = ui, server = server)