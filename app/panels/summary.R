## Filename:    summary.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing summary page
##              Contains top-level summary_ui and summary_server functions
##              All internal functions are prepended with 'summary'

###### UI ######

summary_ui <- function() {
  tabPanel("Summary",
    tabsetPanel(
      type = "tabs",
      summary_ui_visas_tab(),
      summary_ui_download_tab()
    )
  )
}

summary_ui_visas_tab <- function() {
  tabPanel(
    "Visa Categories",

    h4("List of Categories"),
    p("The following pages will display information about many visa categories. ",
      "Below is a summary of what each visa category references."),
    br(),
    
    DT::dataTableOutput("summary_visa_table"),
    
    h4("Sources:"),
    uiOutput("summary_sources")
  )
}

summary_ui_download_tab <- function() {
  tabPanel(
    "Get the Data",
    
    h4("Download Datasets"),
    p("The first 20 rows of each dataset used in this app are displayed below",
      "for your convenience."),
    
    ## select dataset
    selectInput("summary_download_dataset",
                "Choose a dataset:",
                choices = c("Issuances by category, year and nationality", 
                            "Issuances and refusals by category and year", 
                            "Visa category descriptions")),
    
    ## button
    downloadButton("summary_download_data", "Download"),
    
    hr(),
    
    uiOutput("summary_download_title"),
    br(),
    DT::dataTableOutput("summary_download_table")
    
  )
}

###### Server ######

summary_server <- function(input, output, session) {
  summary_server_visas_tab(input, output, session)
  summary_server_download_tab(input, output, session)
}

summary_server_visas_tab <- function(input, output, session) {
  output$summary_visa_table <- DT::renderDataTable({
    colnames(labels) <- c("Visa Category", "Short Description", "Source")
    labels
  })
  
  output$summary_sources <- renderUI({
    all_text <- c("<ul>")
    
    sources_list <- split(label_sources, seq(nrow(label_sources)))
    for(s in sources_list) {
      new_bullet <- sprintf("<li><a href='%s'>%s %s</a></li>", s$URL, s$Source, s$Description)
      all_text <- c(all_text, new_bullet)
    }
    
    all_text <- c(all_text, "</ul>")
    
    HTML(paste(all_text, collapse = ""))
    
  })
}

summary_server_download_tab <- function(input, output, session) {
  dataset_input <- reactive({
    switch(input$summary_download_dataset,
           "Issuances by category, year and nationality" = regional,
           "Issuances and refusals by category and year" = workload,
           "Visa category descriptions" = labels)
  })
  
  output$summary_download_table <- DT::renderDataTable({
    head(dataset_input(), n = 20)
  })
  
  output$summary_download_data <- downloadHandler(
    filename = function() {
      paste0(input$summary_download_dataset, ".csv")
    },
    content = function(file) {
      write.csv(dataset_input(), file)
    }
  )
  
  output$summary_download_title <- renderUI({
    HTML(paste0("<h4>", input$summary_download_dataset, "</h4>"))
  })
}
