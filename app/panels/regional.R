## Filename:    regional.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing regional page
##              Contains top-level regional_ui and regional_server functions
##              All internal functions are prepended with 'regional'

###### UI ######

regional_ui <- function() {
  tabPanel(
    "Issuances Per Country", 
    tabsetPanel(
      type = "tabs",
      regional_ui_overall(),
      regional_ui_evolution(),
      regional_ui_breakdown()
    )
  )
}

regional_ui_overall <- function() {
  tabPanel(
    "Overall",
    
    h4("Visa Issuances Per Country and Year"),
    p("This tab explores data on the number of visa issuances per country, ",
      "category and year."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different maps."),
        get_year_slider(dt=regional, slider_id="regional_overall_year", multiple=F),
        get_visa_select(dt=regional, select_id="regional_overall_category", multiple=F)
      ),
      mainPanel(
        plotlyOutput("regional_overall_plot"),
        br(), br(), br(), br(), br(), br(),
        uiOutput("regional_overall_notes")
      )
    )
    
  )
}

regional_ui_evolution <- function() {
  tabPanel(
    "Evolution",
    
    h4("Visa Issuances Per Country Over Time"),
    p("This tab explores data on the changes in visa issuances across countries ",
      "and across time."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=regional, slider_id="regional_evolution_year"),
        regional_get_nationality_select(select_id="regional_evolution_nationality"),
        get_visa_select(dt=regional, select_id="regional_evolution_category", multiple=F)
      ),
      mainPanel(
        plotlyOutput("regional_evolution_plot"),
        uiOutput("regional_evolution_notes")
      )
    )
    
  )
}

regional_ui_breakdown <- function() {
  tabPanel(
    "Detailed Breakdown",
    
    h4("Visa Issuances Per Country Over Time"),
    p("This tab explores the raw data on the changes in visa issuances across countries ",
      "and across time."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see and download different tables."),
        get_year_slider(dt=regional, slider_id="regional_breakdown_year"),
        regional_get_nationality_select(select_id="regional_breakdown_nationality"),
        get_visa_select(dt=regional, select_id="regional_breakdown_category", multiple=F),
        downloadButton("regional_breakdown_download", "Download the Data")
      ),
      mainPanel(
        DT::dataTableOutput("regional_breakdown_table"),
        uiOutput("regional_breakdown_notes")
      )
    )
    
  )
}

regional_get_nationality_select <- function(select_id) {
  select <- pickerInput(select_id, 
                        label = "Nationalities",
                        choices = as.character(sort(unique(regional$nationality))),
                        selected = as.character(sort(regional[ is_total == TRUE, unique(nationality)])),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)
  return(select)
}

###### Server ######

regional_server <- function(input, output, session) {
  regional_server_overall(input, output, session)
  regional_server_evolution(input, output, session)
  regional_server_breakdown(input, output, session)
}

regional_server_overall <- function(input, output, session) {
  
  output$regional_overall_plot <- renderPlotly({
    selected_year <- input$regional_overall_year
    category <- input$regional_overall_category
    
    subsection <- regional %>% 
      filter(year == selected_year & visa_category == category & !is.na(country_code))
    
    plot_geo(subsection, height = 500) %>%
      add_trace(
        z = ~issued, color = ~issued,
        text = ~nationality, locations = ~country_code, colors = "BuPu",
        marker = list(line = list(color = toRGB("grey"), width = 0.5)) ) %>%
      colorbar(title = "# Issuances") %>%
      layout(
        title = sprintf("%s Visa Issuances in %s", category, selected_year),
        geo = list(showframe = F, showcoastlines = F, projection = list(type = "winkel tripel")),
        margin = list(l = 50, r = 50, b = 0, t = 100, pad = 4)
      )
    
  })
  
  output$regional_overall_notes <- renderUI({
    get_notes_html(visa_categories = input$regional_overall_categories)
  })
  
}

regional_server_evolution <- function(input, output, session) {
  
  output$regional_evolution_plot <- renderPlotly({
    year_min <- input$regional_evolution_year[1]
    year_max <- input$regional_evolution_year[2]
    nationalities <- input$regional_evolution_nationality
    category <- input$regional_evolution_category
    
    dt <- regional_get_evolution_dt(year_min, year_max, category, nationalities)
    
    title_nationalities <- ifelse(length(nationalities) == 1, 
                               paste0("Nationality: ", nationalities[1]), 
                               "Selected Nationalities")
    plot <- plot_ly() %>%
      layout(title = sprintf("%s Visa Issuances Over Time (%s-%s)<br>%s", 
                             category, year_min, year_max, title_nationalities),
             yaxis = list(title = "# Issuances"),
             xaxis = list(title = "Year"),
             hovermode = "compare",
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor="rgba(0,0,0,0)")
    
    for(nat in nationalities) {
      plot <- plot %>% add_trace(x = dt[["year"]], y = dt[[nat]], 
                                 name = nat, type = "scatter", mode = "lines")
    }
    
    plot
  })
  
}

regional_server_breakdown <- function(input, output, session) {
  dataset <- reactiveVal(0)
  
  output$regional_breakdown_table <- DT::renderDataTable({
    # set dataset attribute
    dt <- regional_get_evolution_dt(input$regional_breakdown_year[1], 
                                    input$regional_breakdown_year[2], 
                                    input$regional_breakdown_category, 
                                    input$regional_breakdown_nationality)
    colnames(dt)[1] <- "Year"
    dataset(dt)
    
    # call atribute
    DT::datatable(dataset(), rownames=FALSE, options = list(pageLength = nrow(dataset()))) %>% 
      formatRound(-1, digits = 0)
  })
  
  output$regional_breakdown_download <- downloadHandler(
    filename = function() {
      sprintf("issuances_by_year_nationality_%s-%s.csv", 
              input$regional_breakdown_year[1], input$regional_breakdown_year[2])
    },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  output$regional_breakdown_notes <- renderUI({
    get_notes_html(visa_categories = input$regional_breakdown_category)
  })
}

regional_get_evolution_dt <- function(year_min, year_max, category, nationalities) {
  dt <- regional %>% 
    filter(year >= year_min & year <= year_max & 
             nationality %in% nationalities & visa_category == category) %>%
    select(year, nationality, issued) %>%
    group_by(year, nationality) %>% 
    summarise(issued = sum(issued)) %>%
    arrange(year, nationality) %>%
    spread(nationality, issued)
  return(dt)
}