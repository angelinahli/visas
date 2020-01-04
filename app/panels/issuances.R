## Filename:    issuances.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing issuances page
##              Contains top-level issuances_ui and issuances_server functions
##              All internal functions are prepended with 'issuances'

###### UI ######

issuances_ui <- function() {
  tabPanel(
    "Overall Issuances", 
    tabsetPanel(
      type = "tabs",
      issuances_ui_overall(),
      issuances_ui_evolution(),
      issuances_ui_breakdown()
    )
  )
}

issuances_ui_overall <- function() {
  tabPanel("Overall")
}

issuances_ui_evolution <- function() {
  tabPanel("Evolution")
}

issuances_ui_breakdown <- function() {
  tabPanel("Detailed Breakdown")
}

###### Server ######

issuances_server <- function(input, output, session) {
  
}