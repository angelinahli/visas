## Filename:    home.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing home page
##              Contains top-level home_ui and home_server functions
##              All internal functions are prepended with 'home'

###### UI ######

home_ui <- function() {
  tabPanel(
    
    title = "Home", 
    value = "Home",
    h3("Welcome to the Non-Immigrant Visa Data Explorer!")

  )
}

###### Server ######

home_server <- function(input, output, session) {
  
}