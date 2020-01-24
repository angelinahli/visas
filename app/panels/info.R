## Filename:    info.R
## Author:      Angelina Li
## Date:        2020/01/09
## R version:   3.3.3
## Description: Contains UI and server code managing info page
##              Contains top-level info_ui and info_server functions
##              All internal functions are prepended with 'info'

###### UI ######

info_ui <- function() {
  tabPanel(
    "More Info",
     tabsetPanel(
       type = "tabs",
       info_ui_faqs_tab(),
       info_ui_visas_tab(),
       info_ui_download_tab()
    )
  )
}

info_ui_faqs_tab <- function() {
  tabPanel(
    "About the Project",

    h4("What is this?"),
    p("This app explores some public data on how many non-immigrant visas are ",
      "issued and denied ('refused') per year, and by nation of origin. ", 
      "The data used for this project was obtained from the US State Department's ", 
      "public records."),
    br(),
    
    h4("What's a 'non-immigrant visa'?"),
    p("From the ",
      a(href="https://www.cbp.gov/travel/international-visitors/visa-waiver-program/requirements-immigrant-and-nonimmigrant-visas",
        "CBP website"),
      " - Non-immigrant visas are for people 'seeking to ", 
      "enter the United States on a temporary basis'. They differ from ", 
      "immigrant visas, which are for people 'who inten[d] to live and ", 
      "work permanently in the United States'. "
    ),
    br(),
    
    h4("Where can I find more information on this project?"),
    p(a(href="https://github.com/angelinahli/visas", 
        "The GitHub repository is your best bet."),
      " Alternatively, you can email me at angelinahelenli [at] gmail [dot] com."
    ),
    br(),
    
    h4("Is this open source? Can I play around with the data?"),
    p("Yes and yes! You may use this project however you like. To download the data, ",
      "navigate to Summary > Get the Data, or download the .RDS files directly ",
      "from GitHub."
    ),
    br(),
    
    h4("Why did you make this?"),
    p("I wanted to be able to tinker with this dataset! And, I'm an immigrant, ",
      "and I think immigration is generally interesting."
    ),
    br(),
    
    h4("Why only non-immigrant visas?"),
    p("I haven't gotten around to looking at immigrant visa statistics yet, but ",
      "I'm sure those numbers are interesting too. I and a lot of my friends have ",
      "non-immigrant visas, so I thought this would be a good start."
    )
  )
}

info_ui_visas_tab <- function() {
  tabPanel(
    "Visa Categories",
    
    h4("List of Categories"),
    p("The following pages will display information about many visa categories. ",
      "Below is a summary of what each visa category references."),
    br(),
    
    DT::dataTableOutput("info_visa_table"),
    
    h4("Sources"),
    uiOutput("info_sources")
  )
}

info_ui_download_tab <- function() {
  tabPanel(
    "Get the Data",
    
    h4("Download Datasets"),
    p("The first 20 rows of each dataset used in this app are displayed below",
      "for your convenience."),
    
    ## select dataset
    selectInput("info_download_dataset",
                "Choose a dataset:",
                choices = c("Issuances by category, year and nationality", 
                            "Issuances and refusals by category and year", 
                            "Visa category descriptions")),
    
    ## button
    downloadButton("info_download_data", "Download"),
    
    hr(),
    
    uiOutput("info_download_title"),
    br(),
    DT::dataTableOutput("info_download_table")
    
  )
}

###### Server ######

info_server <- function(input, output, session) {
  info_server_visas_tab(input, output, session)
  info_server_download_tab(input, output, session)
}

info_server_visas_tab <- function(input, output, session) {
  output$info_visa_table <- DT::renderDataTable({
    colnames(labels) <- c("Visa Category", "Description", "Source")
    labels
  }, rownames = FALSE)
  
  output$info_sources <- renderUI({
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

info_server_download_tab <- function(input, output, session) {
  dataset_input <- reactive({
    switch(input$info_download_dataset,
           "Issuances by category, year and nationality" = regional,
           "Issuances and refusals by category and year" = workload,
           "Visa category descriptions" = labels)
  })
  
  output$info_download_table <- DT::renderDataTable({
    head(dataset_input(), n = 20)
  }, rownames = FALSE)
  
  output$info_download_data <- downloadHandler(
    filename = function() {
      paste0(input$info_download_dataset, ".csv")
    },
    content = function(file) {
      write.csv(dataset_input(), file)
    }
  )
  
  output$info_download_title <- renderUI({
    HTML(paste0("<h4>", input$info_download_dataset, "</h4>"))
  })
}
