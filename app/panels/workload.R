## Filename:    workload.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing workload page
##              Contains top-level workload_ui and workload_server functions
##              All internal functions are prepended with 'workload'

###### UI ######

workload_ui <- function() {
  tabPanel(
    
    title = "Issuances and Refusals", 
    value = "Issuances and Refusals",
    
    tabsetPanel(
      
      type = "tabs",
      
      tabPanel("Evolution"),
      tabPanel("Detailed Breakdown")
      
    )
    
  )
}

###### Server ######

workload_server <- function(input, output, session) {
  
}