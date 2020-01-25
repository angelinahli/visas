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
      analysis_ui_gif(),
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
        plotlyOutput("analysis_issuances_plot"),
        uiOutput("analysis_issuances_notes")
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
        plotlyOutput("analysis_workload_plot"),
        uiOutput("analysis_workload_notes")
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
        get_year_slider(dt=regional, slider_id="analysis_map_year", multiple=F)
      ),
      mainPanel(
        plotlyOutput("analysis_map_plot"),
        uiOutput("analysis_map_notes")
      )
    )
  )
}

analysis_ui_gif <- function() {
  screen(
    h4("Visas Issued By Country and Year (Animated)"),
    get_spacer(),
    sidebarLayout(
      sidebarPanel(
        p("Testing"),
        get_visa_select(dt=regional, select_id="analysis_gif_visas", multiple=F)
      ),
      mainPanel(
        plotlyOutput("analysis_gif_plot"),
        uiOutput("analysis_gif_notes")
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
        get_visa_select(dt=regional, select_id="analysis_regional_visas", multiple=F),
        get_year_slider(dt=regional, slider_id="analysis_regional_year"),
        selectInput("analysis_regional_nationality", 
                    label = "Nationalities",
                    choices = as.character(sort(unique(regional$nationality))),
                    selected = as.character(sort(regional[ is_total == TRUE, unique(nationality)])),
                    multiple = TRUE),
      ),
      mainPanel(
        plotlyOutput("analysis_regional_plot"),
        uiOutput("analysis_regional_notes")
      )
    )
  )
}

###### Server ######

analysis_server <- function(input, output, session) {
  analysis_server_issuances(input, output, session)
  analysis_server_workload(input, output, session)
  analysis_server_map(input, output, session)
  analysis_server_gif(input, output, session)
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
        title = list(text = sprintf("Number of %s Visas Issued<br>%s-%s", 
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
  output$analysis_issuances_notes <- renderUI({
    get_notes_html(input$analysis_issuances_visas)
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
      title <- sprintf("Percentage of %s Applications %s<br>%s-%s<br>",
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
  output$analysis_workload_notes <- renderUI({
    get_notes_html(input$analysis_workload_visas)
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
        text = ~nationality, locations = ~country_code,
        colorscale = "Portland") %>%
      colorbar(title = "# Issuances") %>%
      get_plotly_layout(list(
        title = list(text = sprintf("%s Visa Issuances in %s", category, selected_year)),
        geo = list(projection = list(type = "winkel tripel")),
        margin = list(l = 20, r = 20, b = 0, t = 50, pad = 2)
      ))
  })
  output$analysis_map_notes <- renderUI({
    get_notes_html(input$analysis_map_visas)
  })
}

analysis_server_gif <- function(input, output, session) {
  output$analysis_gif_plot <- renderPlotly({
    category <- input$analysis_gif_visas
    subsection <- regional %>% 
      filter(visa_category == category & !is.na(country_code))
    limits <- c(min(subsection$issued, na.rm=T), max(subsection$issued, na.rm=T)) 
    
    plot_geo(subsection) %>%
      add_trace(
        z = ~issued, color = ~issued, frame = ~year,
        text = ~nationality, locations = ~country_code,
        colorscale = "Portland") %>%
      colorbar(limits = limits) %>%
      # animation_opts(20000, easing = "elastic") %>%
      animation_slider(currentvalue = list(prefix = "Year ", font = list(color = "grey")) ) %>%
      get_plotly_layout(list(
        title = list(text = paste0(category, " Visa Issuances")),
        geo = list(projection = list(type = "winkel tripel")),
        margin = list(l = 0, r = 0, b = 0, t = 30, pad = 2)
      ))
  })
  output$analysis_gif_notes <- renderUI({
    get_notes_html(input$analysis_gif_visas)
  })
}

analysis_server_regional <- function(input, output, session) {
  output$analysis_regional_plot <- renderPlotly({
    year_min <- input$analysis_regional_year[1]
    year_max <- input$analysis_regional_year[2]
    nationalities <- input$analysis_regional_nationality
    category <- input$analysis_regional_visas
    
    dt <- regional %>% 
      filter(year >= year_min & year <= year_max & 
               nationality %in% nationalities & visa_category == category) %>%
      select(year, nationality, issued) %>%
      group_by(year, nationality) %>% 
      summarise(issued = sum(issued)) %>%
      arrange(year, nationality) %>%
      spread(nationality, issued)
    
    nat_title <- "Selected Nationalities"
    if(length(nationalities) == 1) {
      if(nationalities[1] == "Total") { nat_title <- "All Nationalities" }
      else { nat_title <- paste0("Applicants from ", gsub(" - Total", "", nationalities[1])) }
    }
    plot <- plot_ly() %>%
      get_plotly_layout(list(
        title = list(text = sprintf("%s Visa Issuances for %s<br>%s-%s", 
                                    category, nat_title, year_min, year_max)),
        yaxis = list(title = "# Issuances"),
        xaxis = list(title = "Year"),
        hovermode = "compare"))
    
    for(nat in nationalities) {
      plot <- plot %>% add_trace(x = dt[["year"]], y = dt[[nat]], 
                                 name = nat, type = "scatter", mode = "lines")
    }
    
    plot
  })
  output$analysis_regional_notes <- renderUI({
    get_notes_html(input$analysis_regional_visas)
  })
}