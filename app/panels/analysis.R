## Filename:    analysis.R
## Author:      Angelina Li
## Date:        2020/01/09
## R version:   3.3.3
## Description: Redesign of the visas app to structure all analyses on the visa
##              level.

###### Static vars and helpers ######

WORKLOAD_ISSUED_OPTION <- "% Issued"
WORKLOAD_GRANTED_OPTION <- "% Granted"

analysis_visa_select <- function(select_id) {
  choices <- unique(
    c( as.character(unique(regional$visa_category)), 
       as.character(unique(workload$visa_category)) )
  )
  choices <- c("Total", sort(choices[ choices != "Total" ]))
  
  select <- pickerInput(select_id, 
                        choices = choices,
                        selected = c("Total"),
                        options = list(
                          "max-options" = 2,
                          "max-options-text" = "No more!"
                        ),
                        multiple = TRUE)
  return(select)
}

###### UI ######

analysis_ui <- function() {
  tabPanel(
    "Analysis",
    
    fluidRow(
      column(4,
             h5(strong("Select Visa Categories (2 max):")),
             analysis_visa_select("analysis_visa_select"),
             div(style = "margin: 25px;")),
      column(8,
             h5(strong("Descriptions")),
             uiOutput("analysis_description"))
    ),
    
    # description
    glide(
      analysis_ui_issuances(),
      analysis_ui_workload(),
      analysis_ui_regional()
    )
    
  )
}

analysis_ui_issuances <- function() {
  screen(
    h4("Change in Visas Issued Over Time"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=regional, slider_id="analysis_issuances_year"),
      ),
      mainPanel(
        plotlyOutput("analysis_issuances_plot")
      )
    )
    
  )
}

analysis_ui_workload <- function() {
  screen(
    h4("Change in Visas Issued and Refused Over Time"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=workload, slider_id="analysis_workload_year"),
        pickerInput("analysis_workload_stats", 
                    label = "Comparison Measure",
                    choices = c(WORKLOAD_ISSUED_OPTION, WORKLOAD_GRANTED_OPTION),
                    selected = c(WORKLOAD_ISSUED_OPTION, WORKLOAD_GRANTED_OPTION),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE)
      ),
      mainPanel(
        plotlyOutput("analysis_workload_plot")
      )
    )
  )
}

analysis_ui_regional <- function() {
  screen(
    
  )
}

###### Server ######

analysis_server <- function(input, output, session) {
  analysis_server_description(input, output, session)
  analysis_server_issuances(input, output, session)
  analysis_server_workload(input, output, session)
}

analysis_server_description <- function(input, output, session) {
  output$analysis_description <- renderUI({
    output_text <- c()
    for(visa in input$analysis_visa_select) {
      label <- ifelse(visa != "Total",
                      labels[ visa_category == visa, description ],
                      "This category encompasses all non-immigrant visa categories per year")
      output_text <- c(output_text, sprintf("<p><b>%s Visa</b>: %s</p>", visa, label))
    }
    HTML(paste(output_text, collapse=""))
  })
}

analysis_server_issuances <- function(input, output, session) {
  output$analysis_issuances_plot <- renderPlotly({
    ## parse input vars
    year_min <- input$analysis_issuances_year[1]
    year_max <- input$analysis_issuances_year[2]
    categories <- input$analysis_visa_select
    
    wide_dt <- regional %>% 
      filter(year >= year_min & year <= year_max & 
               region == "Total" & visa_category %in% categories) %>%
      select(year, visa_category, issued) %>%
      group_by(year, visa_category) %>% 
      summarise(issued = sum(issued)) %>%
      arrange(year, visa_category) %>%
      spread(visa_category, issued)
    
    plot <- plot_ly() %>%
      get_plotly_layout(list(
        title = list(text = sprintf("Number of %s Visas Issued Over Time<br>%s-%s", 
                            paste(categories, collapse=" and "), year_min, year_max)),
        yaxis = list(title = "# Issuances"),
        xaxis = list(title = "Year"),
        hovermode = "compare"))
    
    for(cat in categories) {
      plot <- plot %>% add_trace(x = wide_dt[["year"]], y = wide_dt[[cat]], 
                                 name = cat, type = "scatter", mode = "lines")
    }
    
    plot    
  })
}

analysis_server_workload <- function(input, output, session) {
  
  output$analysis_workload_plot <- renderPlotly({
    year_min <- input$analysis_workload_year[1]
    year_max <- input$analysis_workload_year[2]
    categories <- input$analysis_visa_select
    stats <- input$analysis_workload_stats
    
    if (length(stats) > 0) {
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
      
      stats_text <- ifelse(length(stats) == 0, 
                           "Issued", 
                           paste0(str_replace_all(stats, "% ", ""), collapse = " and "))
      title <- sprintf("Percentage of %s Visas %s Over Time<br>%s-%s<br>",
                       paste(categories, collapse=" and "),
                       stats_text,
                       year_min, year_max)
      
      y_title <- ifelse(length(stats) == 0, "Percentage (%)", paste(stats, collapse = " and "))

      plot <- plot_ly() %>%
        get_plotly_layout(list(
          title = list(text = title),
          yaxis = list(title = y_title, tickformat = "%"),
          xaxis = list(title = "Year"),
          hovermode = "compare"))
      
      for(col in colnames(subsection)[-1]) {
        plot <- plot %>% add_trace(x = subsection[["year"]], y = subsection[[col]], 
                                   name = col, type = "scatter", mode = "lines")
      }
      
      plot
    }
    
  })
  
}