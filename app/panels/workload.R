## Filename:    workload.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing workload page
##              Contains top-level workload_ui and workload_server functions
##              All internal functions are prepended with 'workload'

###### Static vars ######

WORKLOAD_ISSUED_OPTION <- "% Issued"
WORKLOAD_GRANTED_OPTION <- "% Granted"

###### UI ######

workload_ui <- function() {
  tabPanel("Issuances and Refusals",
    tabsetPanel(
      type = "tabs",
      workload_ui_evolution(),
      workload_ui_breakdown()
    )
  )
}

workload_ui_evolution <- function() {
  tabPanel(
    "Evolution",
    
    h4("Visa Issuances and Refusals Over Time"),
    p("This tab explores some data on how the percentages of visa applications ",
      "issued and granted have changed over time."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=workload, slider_id="workload_evolution_year"),
        get_visa_select(dt=workload, select_id="workload_evolution_categories"),
        workload_get_stat_select(select_id="workload_evolution_stats")
      ),
      mainPanel(
        plotlyOutput("workload_evolution_plot"),
        uiOutput("workload_evolution_notes")
      )
    )
    
  )
}

workload_ui_breakdown <- function() {
  tabPanel(
    "Detailed Breakdown",
    
    h4("Visa Issuances and Refusals Over Time"),
    p("This tab explores the raw data on how the percentages of visa applications ",
      "issued and granted have changed over time."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see and download different tables."),
        get_year_slider(dt=workload, slider_id="workload_breakdown_year"),
        get_visa_select(dt=workload, select_id="workload_breakdown_categories"),
        workload_get_stat_select(select_id="workload_breakdown_stats")
      ),
      mainPanel(
        DT::dataTableOutput("workload_breakdown_table"),
        uiOutput("workload_breakdown_notes")
      )
    )
    
  )
}

workload_get_stat_select <- function(select_id) {
  choices <- c(WORKLOAD_ISSUED_OPTION, WORKLOAD_GRANTED_OPTION)
  pickerInput(select_id, 
              label = "Select Comparison Measure",
              choices = choices,
              selected = choices,
              options = list(`actions-box` = TRUE),
              multiple = TRUE)
}

###### Server ######

workload_server <- function(input, output, session) {
  workload_server_evolution(input, output, session)
  workload_server_breakdown(input, output, session)
}

workload_server_evolution <- function(input, output, session) {
  
  output$workload_evolution_plot <- renderPlotly({
    year_min <- input$workload_evolution_year[1]
    year_max <- input$workload_evolution_year[2]
    categories <- input$workload_evolution_categories
    stats <- input$workload_evolution_stats
    
    filtered <- workload %>%
      filter(year >= year_min & year <= year_max & visa_category %in% categories)
    
    if(WORKLOAD_ISSUED_OPTION %in% stats) {
      names(filtered)[names(filtered) == "perc_issued"] <- WORKLOAD_ISSUED_OPTION
    }
    if(WORKLOAD_GRANTED_OPTION %in% stats) {
      names(filtered)[names(filtered) == "perc_granted"] <- WORKLOAD_GRANTED_OPTION
    }
    
    subsection <- filtered %>%
      select(c("year", "visa_category", stats)) %>%
      # below from: https://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr
      gather(variable, value, -year, -visa_category) %>%
      unite(temp, visa_category, variable, sep = " - ") %>%
      spread(temp, value)
    
    visa_category_text <- ifelse(length(categories) == 1, 
                                 paste0("Visa Category: ", categories[1]), 
                                 "Selected Categories")
    stats_text <- ifelse(length(stats) == 0, 
                         "Issued", 
                         paste0(str_replace_all(stats, "% ", ""), collapse = " and "))
    title <- sprintf("Percentage Visas %s Over Time (%s-%s)<br>%s",
                     stats_text,
                     year_min, year_max,
                     visa_category_text)

    y_title <- paste(stats, collapse = " and ")
    y_title <- ifelse(y_title == "", "Percentage (%)", y_title)
    
    plot <- plot_ly() %>%
      layout(title = title,
             yaxis = list(title = y_title, tickformat = "%"),
             xaxis = list(title = "Year"),
             hovermode = "compare")
    
    for(col in colnames(subsection)[-1]) {
      plot <- plot %>% add_trace(x = subsection[["year"]], y = subsection[[col]], 
                                 name = col, type = "scatter", mode = "lines")
    }
    
    plot
  })
  
  output$workload_evolution_notes <- renderUI({
    workload_get_notes_html(input$workload_evolution_stats, input$workload_evolution_categories)
  })
  
}

workload_server_breakdown <- function(input, output, session) {
  
  output$workload_breakdown_table <- DT::renderDataTable({
    ## parse input vars
    year_min <- input$workload_breakdown_year[1]
    year_max <- input$workload_breakdown_year[2]
    categories <- input$workload_breakdown_categories
    stats <- input$workload_breakdown_stats
    
    filtered <- workload %>%
      filter(year >= year_min & year <= year_max & visa_category %in% categories)
    
    names(filtered)[1] <- "Year"
    names(filtered)[2] <- "Visa Category"
    
    if(WORKLOAD_ISSUED_OPTION %in% stats) {
      names(filtered)[names(filtered) == "perc_issued"] <- WORKLOAD_ISSUED_OPTION
    }
    if(WORKLOAD_GRANTED_OPTION %in% stats) {
      names(filtered)[names(filtered) == "perc_granted"] <- WORKLOAD_GRANTED_OPTION
    }
    
    selected <- filtered[, c("Year", "Visa Category", stats)]
    
    # show and format table
    DT::datatable(selected, rownames=FALSE) %>% formatPercentage(-1:-2, digits = 2)
  })
  
  output$workload_breakdown_notes <- renderUI({
    workload_get_notes_html(input$workload_breakdown_stats, input$workload_breakdown_categories)
  })
  
}

workload_get_notes_html <- function(stats, visa_categories) {
  notes <- c()
  notes[WORKLOAD_ISSUED_OPTION] <- paste0(
    "The percentage of applications issued is calculated by dividing the number ",
    "of applications issued by the number of applications processed.")
  notes[WORKLOAD_GRANTED_OPTION] <- paste0(
    "The percentage of applications granted is calculated by dividing the number ",
    "of applications issued, waived or overcome by the number of ",
    "applications processed. Note that applications may be waived or ",
    "overcome in a different fiscal year from when they were first ",
    "processed. Thus, this figure may exceed 100 %, and is meant as ",
    "an estimate of the true number of applications granted overall.")
  
  additional_notes <- c()
  for(stat in stats) {
    additional_notes <- c(additional_notes, notes[stat])
  }
  
  get_notes_html(visa_categories = visa_categories,
                 additional_notes = additional_notes)
}