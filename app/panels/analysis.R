## Filename:    analysis.R
## Author:      Angelina Li
## Date:        2020/01/09
## R version:   3.3.3
## Description: Redesign of the visas app to structure all analyses on the visa
##              level.

###### Static vars ######

WORKLOAD_ISSUED_OPTION <- "% Issued"
WORKLOAD_GRANTED_OPTION <- "% Granted"

###### UI ######

analysis_ui <- function() {
  tabPanel(
    "Analysis",
    
    # description
    glide(
      analysis_ui_issuances(),
      analysis_ui_workload(),
      analysis_ui_map(),
      analysis_ui_map_gif(),
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
        get_visa_select(dt=regional, select_id="analysis_issuances_visas"),
        get_year_slider(dt=regional, slider_id="analysis_issuances_year")
      ),
      mainPanel(
        plotlyOutput("analysis_issuances_plot")
      )
    )
    
  )
}

analysis_ui_workload <- function() {
  screen(
    h4("Change in Visa Applications Issued and Granted Over Time"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_visa_select(dt=regional, select_id="analysis_workload_visas"),
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

analysis_ui_map <- function() {
  screen(
    h4("Visas Issued By Country and Year (Interactive)"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different maps."),
        get_visa_select(dt=regional, select_id="analysis_map_visas", multiple=F),
        get_year_slider(dt=regional, slider_id="analysis_map_year", multiple=F),
      ),
      mainPanel(
        plotlyOutput("analysis_map_plot")
      )
    )
  )
}

analysis_ui_map_gif <- function() {
  screen(
    h4("Visas Issued By Country and Year (Animated)"),
    get_spacer(),
    sidebarLayout(
      sidebarPanel(
        p("Testing")
      ),
      mainPanel(
        plotlyOutput("analysis_map_gif_plot")
      )
    )
    
  )
}

analysis_ui_regional <- function() {
  screen(
    h4("Changes in Visas Issued By Country Over Time"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=regional, slider_id="analysis_regional_year"),
        pickerInput("analysis_regional_nationality", 
                    label = "Nationalities",
                    choices = as.character(sort(unique(regional$nationality))),
                    selected = as.character(sort(regional[ is_total == TRUE, unique(nationality)])),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE)
      ),
      mainPanel(
        plotlyOutput("analysis_regional_plot")
      )
    )
  )
}

###### Server ######

analysis_server <- function(input, output, session) {
  analysis_server_issuances(input, output, session)
  analysis_server_workload(input, output, session)
  analysis_server_map(input, output, session)
  analysis_server_map_gif(input, output, session)
  analysis_server_regional(input, output, session)
}

analysis_server_issuances <- function(input, output, session) {
  output$analysis_issuances_plot <- renderPlotly({
    ## parse input vars
    year_min <- input$analysis_issuances_year[1]
    year_max <- input$analysis_issuances_year[2]
    categories <- input$analysis_issuances_visas
    
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
                            get_categories_text(categories), year_min, year_max)),
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
    categories <- input$analysis_workload_visas
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
      title <- sprintf("Percentage of %s Visa Applications %s Over Time<br>%s-%s<br>",
                       get_categories_text(categories),
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

analysis_server_map <- function(input, output, session) {
  output$analysis_map_plot <- renderPlotly({
    selected_year <- input$analysis_map_year
    category <- input$analysis_map_visas
    
    subsection <- regional %>% 
      filter(year == selected_year & visa_category == category & !is.na(country_code))
    
    plot_geo(subsection) %>%
      add_trace(
        z = ~issued, color = ~issued,
        text = ~nationality, locations = ~country_code, colors = "Blues",
        marker = list(line = list(color = toRGB("grey"), width = 0.5)) ) %>%
      colorbar(title = "# Issuances") %>%
      get_plotly_layout(list(
        title = list(text = sprintf("%s Visa Issuances in %s", category, selected_year)),
        geo = list(showframe = F, showcoastlines = F, projection = list(type = "winkel tripel")),
        margin = list(l = 50, r = 50, b = 0, t = 100, pad = 4)
      ))
  })
}

analysis_server_map_gif <- function(input, output, session) {
  
}

analysis_server_regional <- function(input, output, session) {
  
}