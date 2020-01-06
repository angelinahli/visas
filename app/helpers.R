## Filename:    helpers.R
## Author:      Angelina Li
## Date:        2020/01/04
## R version:   3.3.3
## Description: Helper functions

get_year_slider <- function(dt, slider_id) {
  # returns a slider component with id slider_id, min=min(dt$year),
  # max=max(dt$year), value=c(min(dt$year), max(dt$year)), label 'Year'
  # and step=1
  min_year <- min(dt$year)
  max_year <- max(dt$year)
  slider <- sliderInput(slider_id, "Year", 
                        min_year, max_year, c(min_year, max_year),
                        step=1, sep="")
  return(slider)
}

get_visa_select <- function(dt, select_id, selected=NA) {
  # return a select component with id select_id, with options sourced
  # from dt$visa_category
  # assume visa_category is a factor
  choices <- sort(unique(dt$visa_category))
  selection <- choices
  if(!is.na(selected)) { selection <- selected }
  select <- pickerInput(select_id, label = "Visa Categories",
                        choices = choices,
                        selected = selection,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)
}