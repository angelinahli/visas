## Filename:    issuances.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing issuances page
##              Contains top-level issuances_ui and issuances_server functions
##              All internal functions are prepended with 'issuances'

###### UI ######

issuances_ui <- function() {
  tabPanel(
    "Overall Issuances", 
    tabsetPanel(
      type = "tabs",
      issuances_ui_overall(),
      issuances_ui_evolution(),
      issuances_ui_breakdown()
    )
  )
}

issuances_ui_overall <- function() {
  tabPanel(
    "Overall",
    
    h4("Overall Issuances by Visa Category"),
    p("This tab explores some basic numbers on how many visas have been",
      "issued across all years."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=regional, slider_id="issuances_overall_year"),
        
        sliderInput("issuances_overall_num_cats", 
                    "Number of Categories to Show", 
                    min=1, max=length(unique(regional$visa_category)), 
                    value=10,
                    step=1)
      ),
      mainPanel(
        plotlyOutput("issuances_overall_plot"),
        uiOutput("issuances_overall_notes")
      )
    )
    
  )
}

issuances_ui_evolution <- function() {
  tabPanel(
    "Evolution",
    
    h4("Visa Issuances Over Time"),
    p("This tab explores some data on how visa issuances have changed",
      "over time."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see different plots."),
        get_year_slider(dt=regional, slider_id="issuances_evolution_year"),
        get_visa_select(dt=regional, select_id="issuances_evolution_categories")
      ),
      mainPanel(
        plotlyOutput("issuances_evolution_plot"),
        uiOutput("issuances_evolution_notes")
      )
    )
    
  )
}

issuances_ui_breakdown <- function() {
  tabPanel(
    "Detailed Breakdown",
    
    h4("Visa Issuances Over Time"),
    p("This tab explores the raw data on how visa issuances have changed",
      "over time."),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        p("Subset the data to see and download different tables."),
        get_year_slider(dt=regional, slider_id="issuances_breakdown_year"),
        get_visa_select(dt=regional, select_id="issuances_breakdown_categories"),
        downloadButton("issuances_breakdown_download", "Download the Data")
      ),
      mainPanel(
        DT::dataTableOutput("issuances_breakdown_table"),
        uiOutput("issuances_breakdown_notes")
      )
    )
    
  )
}

###### Server ######

issuances_server <- function(input, output, session) {
  issuances_server_overall(input, output, session)
  issuances_server_evolution(input, output, session)
  issuances_server_breakdown(input, output, session)
}

issuances_server_overall <- function(input, output, session) {
  categories <- reactiveVal(c())
  
  ## plot
  output$issuances_overall_plot <- renderPlotly({
    ## parse input vars
    year_min <- input$issuances_overall_year[1]
    year_max <- input$issuances_overall_year[2]
    num_categories <- input$issuances_overall_num_cats
    
    grouped <- regional %>% 
               dplyr::filter(year >= year_min & year <= year_max & region == "Total") %>%
               select(visa_category, issued) %>%
               group_by(visa_category) %>% 
               summarise(issued = sum(issued)) %>%
               arrange(desc(issued))
    
    top_rows <- head(grouped, num_categories + 1)
    top_rows$visa_category <- as.character(top_rows$visa_category)
    other_count_df <- grouped %>% 
                      dplyr::filter(!(visa_category %in% unique(top_rows$visa_category))) %>%
                      summarise(issued = sum(issued))
    other_row <- list(visa_category="Other", issued=other_count_df$issued)
    all_data <- rbind(top_rows, other_row)
    
    # save visa categories
    categories(c(categories, as.character(all_data$visa_category)))
    
    # order:
    visa_order <- all_data$visa_category
    all_data$visa_category <- factor(all_data$visa_category, visa_order)
    
    plot_ly(type = "bar", data = all_data, x = ~visa_category, y = ~issued) %>%
      layout(title = sprintf("Overall Visa Issuances (%s-%s)", year_min, year_max),
             yaxis = list(title = "# Issuances"),
             xaxis = list(title = "Visa Category"),
             hovermode = "compare")
  })
  
  output$issuances_overall_notes <- renderUI({
    get_rendered_visa_notes(categories())
  })
  
}

issuances_server_evolution <- function(input, output, session) {
  
  output$issuances_evolution_plot <- renderPlotly({
    ## parse input vars
    year_min <- input$issuances_evolution_year[1]
    year_max <- input$issuances_evolution_year[2]
    categories <- input$issuances_evolution_categories
    
    wide_dt <- issuances_get_evolution_df(year_min, year_max, categories)
    
    title_categories <- ifelse(length(categories) == 1, 
                               paste0("Visa Category: ", categories[1]), 
                               "Selected Categories")
    plot <- plot_ly() %>%
            layout(title = sprintf("Visa Issuances Over Time (%s-%s)<br>%s", 
                                   year_min, year_max, title_categories),
                   yaxis = list(title = "# Issuances"),
                   xaxis = list(title = "Year"),
                   hovermode = "compare")
    
    for(cat in categories) {
      plot <- plot %>% add_trace(x = wide_dt[["year"]], y = wide_dt[[cat]], 
                                 name = cat, type = "scatter", mode = "lines")
    }

    plot    
  })
  
  output$issuances_evolution_notes <- renderUI({
    get_rendered_visa_notes(input$issuances_evolution_categories)
  })
}

issuances_server_breakdown <- function(input, output, session) {
  
  dataset <- reactiveVal(0)
  
  output$issuances_breakdown_table <- DT::renderDataTable({
    ## parse input vars
    year_min <- input$issuances_breakdown_year[1]
    year_max <- input$issuances_breakdown_year[2]
    categories <- input$issuances_breakdown_categories
    
    # set dataset attribute
    df <- issuances_get_evolution_df(input$issuances_breakdown_year[1], 
                                     input$issuances_breakdown_year[2], 
                                     input$issuances_breakdown_categories)
    colnames(df)[1] <- "Year"
    dataset(df)
    
    # call atribute
    DT::datatable(dataset(), rownames=FALSE, options = list(pageLength = nrow(dataset()))) %>% 
      formatRound(-1, digits = 0)
  })
  
  output$issuances_breakdown_download <- downloadHandler(
    filename = function() {
      sprintf("issuances_by_year_cat_%s-%s.csv", 
              input$issuances_breakdown_year[1], input$issuances_breakdown_year[2])
    },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  output$issuances_breakdown_notes <- renderUI({
    get_rendered_visa_notes(input$issuances_breakdown_categories)
  })
  
}

issuances_get_evolution_df <- function(year_min, year_max, categories) {
  grouped <- regional %>% 
    dplyr::filter(year >= year_min & year <= year_max & 
                    region == "Total" & visa_category %in% categories) %>%
    select(year, visa_category, issued) %>%
    group_by(year, visa_category) %>% 
    summarise(issued = sum(issued)) %>%
    arrange(year, visa_category)
  
  wide_dt <- spread(grouped, visa_category, issued)
  return(wide_dt)
}