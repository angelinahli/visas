## Filename:    helpers.R
## Author:      Angelina Li
## Date:        2020/01/04
## R version:   3.3.3
## Description: Helper functions

get_year_slider <- function(dt, slider_id, multiple=TRUE) {
  # returns a slider component with id slider_id, min=min(dt$year),
  # max=max(dt$year), value=c(min(dt$year), max(dt$year)), label 'Year'
  # and step=1
  min_year <- min(dt$year)
  max_year <- max(dt$year)
  value <- c(min_year, max_year)
  if(!multiple) { value <- max_year }
  slider <- sliderInput(slider_id, "Year", 
                        min_year, max_year, value,
                        step=1, sep="")
  return(slider)
}

get_visa_select <- function(dt, select_id, multiple=TRUE) {
  choices <- as.character(unique(dt$visa_category))
  choices <- c("Total", sort(choices[ choices != "Total" ]))
  label <- ifelse(multiple, "Visa Categories", "Visa Category")
  select <- selectInput(select_id, label,
                        choices = choices,
                        selected = c("Total"),
                        multiple = multiple)
  return(select)
}

get_categories_text <- function(cats) {
  if(length(cats) == 1) { return(cats[1]) }
  joined_until_last <- paste(cats[1:length(cats)-1], collapse=", ")
  return( paste(joined_until_last, "and", cats[length(cats)]) )
}

get_spacer <- function() {
  div(style="margin: 10px")
}

get_plotly_layout <- function(plot, plotly_layout) {
  # defines some default attribuets of plotly layout to allow for better standardization
  bg_color <- "rgba(255, 255, 255, 1)"
  l <- list(
    p = plot,
    plot_bgcolor = bg_color,
    paper_bgcolor = bg_color,
    font = list(family = '"Montserrat", "Open Sans", verdana, arial, sans-serif')
  )
  for(var in names(plotly_layout)) {
    l[[var]] <- plotly_layout[[var]]
  }
  if("title" %in% names(l)) {
    l[["title"]][["font"]] <- list(size = 14)
  }
  return(do.call(plotly::layout, l))
}