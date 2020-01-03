## Filename:    summary.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing summary page
##              Contains top-level summary_ui and summary_server functions
##              All internal functions are prepended with 'summary'

###### UI ######

summary_ui <- function() {
  tabPanel(
    
    title = "Summary",
    value = "Summary",
    
    tabsetPanel(
      
      type = "tabs",
      
      summary_visas_tab(),
      tabPanel("Download Data")
      
    )
    
  )
}

summary_visas_tab <- function() {
  tabPanel(
    title = "Visa Categories",
    value = "Visa Categories",
    
    h4("List of Categories"),
    p("The following pages will display information about many visa categories. ",
      "Below is a summary of what each visa category references."),
    br(),
    
    DT::dataTableOutput("summary_visa_table"),
    
    h4("Sources:"),
    uiOutput("summary_sources")
  )
}

###### Server ######

summary_server <- function(input, output, session) {
  
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
