## Filename:    regional.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing regional page
##              Contains top-level regional_ui and regional_server functions
##              All internal functions are prepended with 'regional'

###### UI ######

regional_ui <- function() {
  tabPanel(
    
    title = "Issuances Per Country", 
    value = "Issuances Per Country",
    
    tabsetPanel(
      
      type = "tabs",
      
      tabPanel("Evolution"),
      tabPanel("Detailed Breakdown")
      
    )
    
  )
}

###### Server ######

regional_server <- function(input, output, session) {
  
}