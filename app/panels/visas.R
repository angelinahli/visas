## Filename:    visas.R
## Author:      Angelina Li
## Date:        2020/01/24
## R version:   3.3.3
## Description: Contains UI and server code managing visas page
##              Contains top-level visas_ui and visas_server functions
##              All internal functions are prepended with 'visas'

visas_ui <- function() {
  tabPanel(
    "Visa Categories",
    h4("List of Categories"),
    p("Below is a summary of what each visa category analyzed in this app references."),
    
    DT::dataTableOutput("visas_table"),
    
    h4("Sources"),
    uiOutput("visas_sources")
  )
}

visas_server <- function(input, output, session) {
  output$visas_table <- DT::renderDataTable({
    colnames(labels) <- c("Visa Category", "Description", "Source")
    labels
  }, rownames = FALSE)
  
  output$visas_sources <- renderUI({
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