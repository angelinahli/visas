## Filename:    analysis.R
## Author:      Angelina Li
## Date:        2020/01/09
## R version:   3.3.3
## Description: Redesign of the visas app to structure all analyses on the visa
##              level.

###### UI ######

analysis_ui <- function() {
  tabPanel(
    "Analysis",
    
    h4("Select Visa Categories (2 max):"),
    get_visa_select("analysis_visa_select")
    
    
  )
}

get_visa_select <- function(select_id) {
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

get_sorted_unique_values <- function(values) {
  return( as.character(sort(unique(values))) )
}

###### Server ######

analysis_server <- function(input, output, session) {
}