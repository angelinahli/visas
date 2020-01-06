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
      regional_ui_overall(),
      regional_ui_breakdown()
    )
  )
}

regional_ui_overall <- function() {
  tabPanel(
    "Overall",
    
    h4("Visa Issuances Per Country and Year"),
    p("This tab explores some data on the number of visa issuances per country, ",
      "category and year."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        get_year_slider(dt=regional, slider_id="regional_overall_year", multiple=F),
        get_visa_select(dt=regional, select_id="regional_overall_categories", multiple=F),
        materialSwitch("regional_overall_is_animated", "Show Animated", value = F, status = "default")
      ),
      mainPanel(
        plotlyOutput("regional_overall_map"),
        uiOutput("regional_overall_notes")
      )
    )
    
  )
}

regional_ui_breakdown <- function() {
  tabPanel("Detailed Breakdown")
}

###### Server ######

regional_server <- function(input, output, session) {
  regional_server_overall(input, output, session)
  regional_server_breakdown(input, output, session)
}

regional_server_overall <- function(input, output, session) {
  
  output$regional_overall_notes <- renderUI({
    get_notes_html(visa_categories = input$regional_overall_categories)
  })
  
}

regional_server_breakdown <- function(input, output, session) {
  
}