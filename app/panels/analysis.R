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
      analysis_ui_start(),
      analysis_ui_issuances(),
      analysis_ui_workload(),
      analysis_ui_nats(),
      analysis_ui_map(),
      analysis_ui_regional()
    )
    
  )
}

analysis_ui_start <- function() {
  screen(
    h4("Welcome!"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("This project explores data on non-immigrant visas in America.",
          "Subset the data below to see different plots, and click next to continue."),
        get_year_slider(dt=regional, slider_id="analysis_start_year"),
        
        sliderInput("analysis_start_num_cats", 
                    "Number of Categories", 
                    min=1, max=30, 
                    value=5,
                    step=1)
      ),
      mainPanel(
        plotlyOutput("analysis_start_plot")
      )
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
        selectInput("analysis_workload_stats", 
                    label = "Comparison Measure",
                    choices = c(WORKLOAD_ISSUED_OPTION, WORKLOAD_GRANTED_OPTION),
                    selected = c(WORKLOAD_ISSUED_OPTION, WORKLOAD_GRANTED_OPTION),
                    multiple = TRUE),
        p(
          "The % of applications issued is the number of applications issued",
          "over the total number of applications processed.",
          br(), br(),
          "The % of applications granted is the number of applications issued",
          "waived or overcome over the total number of applications processed.",
        )
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
    h4("Visas Issued By Country and Year"),
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

analysis_ui_nats <- function() {
  screen(
    h4("Overall Visas Issued By Country"),
    get_spacer(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_visa_select(dt=regional, select_id="analysis_nats_visas", multiple=F),
        get_year_slider(dt=regional, slider_id="analysis_nats_year", multiple=T),
        get_nats_select(select_id="analysis_nats_nats")
      ),
      mainPanel(
        plotlyOutput("analysis_nats_plot"),
        uiOutput("analysis_nats_notes")
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
        get_nats_select(select_id="analysis_regional_nats")
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
  analysis_server_start(input, output, session)
  analysis_server_issuances(input, output, session)
  analysis_server_workload(input, output, session)
  analysis_server_map(input, output, session)
  analysis_server_nats(input, output, session)
  analysis_server_regional(input, output, session)
}

analysis_server_start <- function(input, output, session) {
  output$analysis_start_plot <- renderPlotly({
    ## parse input vars
    year_min <- input$analysis_start_year[1]
    year_max <- input$analysis_start_year[2]
    num_categories <- input$analysis_start_num_cats

    grouped <- regional %>%
      dplyr::filter(year >= year_min & year <= year_max & region == "Total") %>%
      select(visa_category, issued) %>%
      group_by(visa_category) %>%
      summarise(issued = sum(issued)) %>%
      arrange(desc(issued))

    top_rows <- head(grouped, num_categories + 1)
    top_rows$visa_category <- as.character(top_rows$visa_category)
    other_count_df <- grouped %>%
      filter(!(visa_category %in% unique(top_rows$visa_category))) %>%
      summarise(issued = sum(issued))
    other_row <- list(visa_category="Other", issued=other_count_df$issued)
    all_data <- rbind(top_rows, other_row)

    # order:
    all_data$visa_category <- factor(all_data$visa_category, all_data$visa_category)

    plot_ly(type = "bar", data = all_data,
            x = ~visa_category, y = ~issued, color = ~visa_category,
            text = unlist(lapply(as.character(all_data$visa_category), get_label))) %>%
    get_plotly_layout(list(
      title = list(text = sprintf(
        "Overall Number of Visas Issued for Top %s Categories<br>%s-%s", num_categories, year_min, year_max)),
      yaxis = list(title = "# Issuances"),
      xaxis = list(title = "Visa Category"),
      hovermode = "compare"))
  })
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
}

analysis_server_workload <- function(input, output, session) {
  output$analysis_workload_plot <- renderPlotly({
    year_min <- input$analysis_workload_year[1]
    year_max <- input$analysis_workload_year[2]
    categories <- input$analysis_workload_visas
    stats <- sort(input$analysis_workload_stats)
    
    stat_map <- c("perc_issued" = WORKLOAD_ISSUED_OPTION,
                  "perc_granted" = WORKLOAD_GRANTED_OPTION)
    
    if (length(stats) > 0) {
      filtered <- workload %>%
        filter(year >= year_min & year <= year_max & visa_category %in% categories)
      
      for (s in names(stat_map)) {
        names(filtered)[names(filtered) == s] <- stat_map[s]
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
      
      ordered_colnames <- colnames(subsection)[-1]
      if("Total" %in% categories) {
        ordered_colnames <- c()
        for(stat in stats) {
          ordered_colnames <- c(ordered_colnames, paste0("Total - ", stat))
        }
        all_colnames <- colnames(subsection)[-1]
        other_colnames <- all_colnames[!grepl("Total - ", all_colnames)]
        ordered_colnames <- c(ordered_colnames, other_colnames)
      }
      for(col in ordered_colnames) {
        plot <- plot %>% add_trace(x = subsection[["year"]], y = subsection[[col]], 
                                   name = col, type = "scatter", mode = "lines")
      }
      
      plot
    }
  })
  output$analysis_workload_notes <- renderUI({
    get_notes_html(
      additional_notes = c(
        paste0("Note that applications may be waived or ",
               "overcome in a different fiscal year from when they were first ",
               "processed. Thus, the percentage of applications granted may exceed ",
               "100% in a given year, and is meant as an estimation only."))
      )
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
    get_notes_html(visa_categories = input$analysis_map_visas)
  })
}

analysis_server_nats <- function(input, output, session) {
  output$analysis_nats_plot <- renderPlotly({
    year_min <- input$analysis_nats_year[1]
    year_max <- input$analysis_nats_year[2]
    category <- input$analysis_nats_visas
    nationalities <- input$analysis_nats_nats
    
    grouped <- regional %>% 
      filter(year >= year_min & year <= year_max & visa_category == category 
             & nationality %in% nationalities) %>%
      select(nationality, issued) %>%
      group_by(nationality) %>%
      summarise(issued = sum(issued, na.rm=T))
    grouped$nationality <- factor(grouped$nationality, grouped$nationality)
    
    plot_ly(type = "bar", data = grouped,
            x = ~nationality, y = ~issued, color = ~nationality) %>%
      get_plotly_layout(list(
        title = list(text = sprintf(
          "Overall Number of %s Visas Issued Per Nationality<br>%s-%s", category, year_min, year_max)),
        yaxis = list(title = "# Issuances"),
        xaxis = list(title = "Nationality"),
        hovermode = "compare"))
  })
  output$analysis_nats_notes <- renderUI({
    get_notes_html(visa_categories = input$analysis_nats_visas)
  })
}

analysis_server_regional <- function(input, output, session) {
  output$analysis_regional_plot <- renderPlotly({
    year_min <- input$analysis_regional_year[1]
    year_max <- input$analysis_regional_year[2]
    nationalities <- input$analysis_regional_nats
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
      else { nat_title <- paste0("Applicants from ", nationalities[1]) }
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
    get_notes_html(visa_categories = input$analysis_regional_visas)
  })
}
