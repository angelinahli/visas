## Filename:    issuances.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing issuances page
##              Contains top-level issuances_ui and issuances_server functions

###### UI ######

issuances_ui <- function() {
  tabPanel(
    
    title = "Overall Issuances", 
    value = "Overall Issuances",
    
    tabsetPanel(
      
      type = "tabs",
      
      tabPanel("Overall"),
      tabPanel("Evolution"),
      tabPanel("Detailed Breakdown")
      
    )
    
  )
}

###### Server ######

issuances_server <- function(input, output, session) {
  
}