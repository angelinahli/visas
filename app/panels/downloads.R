## Filename:    downloads.R
## Author:      Angelina Li
## Date:        2020/01/09
## R version:   3.3.3
## Description: Contains UI and server code managing info page
##              Contains top-level downloads_ui and downloads_server functions
##              All internal functions are prepended with 'downloads'

###### UI ######

downloads_ui <- function() {
  tabPanel(
    "Get the Data",
    
    h4("Download Datasets"),
  
    ## select dataset
    selectInput("downloads_dataset",
                "Choose a dataset:",
                width = "400px",
                choices = c("Issuances by category, year and nationality", 
                            "Issuances and refusals by category and year", 
                            "Visa category descriptions")),
    
    ## button
    downloadButton("downloads_button", "Download"),
    
    hr(),
    
    uiOutput("downloads_title"),
    br(),
    DT::dataTableOutput("downloads_table")
  )
}

###### Server ######

downloads_server <- function(input, output, session) {
  num_rows <- 20
  
  dataset_input <- reactive({
    switch(input$downloads_dataset,
           "Issuances by category, year and nationality" = regional,
           "Issuances and refusals by category and year" = workload,
           "Visa category descriptions" = labels)
  })
  
  output$downloads_table <- DT::renderDataTable({
    head(dataset_input(), n = num_rows)
  }, rownames = FALSE)
  
  output$downloads_button <- downloadHandler(
    filename = function() {
      paste0(input$downloads_dataset, ".csv")
    },
    content = function(file) {
      write.csv(dataset_input(), file)
    }
  )
  
  output$downloads_title <- renderUI({
    HTML(paste0("<h4>", input$downloads_dataset, " (First ", num_rows, " rows)</h4>"))
  })
}
