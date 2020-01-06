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
    "Issuances Per Country", 
    tabsetPanel(
      type = "tabs",
      regional_ui_evolution(),
      regional_ui_breakdown()
    )
  )
}

regional_ui_evolution <- function() {
  tabPanel("Evolution")
}

regional_ui_breakdown <- function() {
  tabPanel("Detailed Breakdown")
}

###### Server ######

regional_server <- function(input, output, session) {
  regional_server_evolution(input, output, session)
  regional_server_breakdown(input, output, session)
}

regional_server_evolution <- function(input, output, session) {
  
}

regional_server_breakdown <- function(input, output, session) {
  
}