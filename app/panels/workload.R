## Filename:    workload.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing workload page
##              Contains top-level workload_ui and workload_server functions
##              All internal functions are prepended with 'workload'

###### UI ######

workload_ui <- function() {
  tabPanel("Issuances and Refusals",
    tabsetPanel(
      type = "tabs",
      workload_ui_evolution(),
      workload_ui_breakdown()
    )
  )
}

workload_ui_evolution <- function() {
  tabPanel("Evolution")
}

workload_ui_breakdown <- function() {
  tabPanel("Detailed Breakdown")
}

###### Server ######

workload_server <- function(input, output, session) {
  workload_server_evolution(input, output, session)
  workload_server_breakdown(input, output, session)
}

workload_server_evolution <- function(input, output, session) {
  
}

workload_server_breakdown <- function(input, output, session) {
  
}