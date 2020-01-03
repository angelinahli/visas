## Filename:    summary.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing summary page
##              Contains top-level summary_ui and summary_server functions

###### UI ######

summary_ui <- function() {
  tabPanel(
    
    title = "Summary",
    value = "Summary",
    
    tabsetPanel(
      
      type = "tabs",
      
      tabPanel("Types of Visas"),
      tabPanel("Download Data")
      
    )
    
  )
}

###### Server ######

summary_server <- function(input, output, session) {
  
}
