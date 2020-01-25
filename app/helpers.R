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
  choices <- get_sorted_categories(as.character(unique(dt$visa_category)))
  label <- ifelse(multiple, "Visa Categories", "Visa Category")
  select <- selectInput(select_id, label,
                        choices = choices,
                        selected = c("Total"),
                        multiple = multiple)
  return(select)
}

get_categories_text <- function(cats, limit = 5) {
  # limit = max number of cats to return
  if(length(cats) > limit | length(cats) == 0) { return("Selected Non-Immigrant") }
  return(get_joined_elts(get_sorted_categories(cats)))
}

get_joined_elts <- function(sorted_elts) {
  if(length(sorted_elts) == 1) { return(sorted_elts[1]) }
  joined_until_last <- paste(sorted_elts[1:length(sorted_elts)-1], collapse=", ")
  return( paste(joined_until_last, "and", sorted_elts[length(sorted_elts)]) )
}

get_notes_html <- function(visa_categories) {
  # prints out standard notes on visa categories, as well as any additional
  # notes
  cats <- get_sorted_categories(visa_categories)
  labels_subset <- labels[visa_category %in% cats, ]
  notes <- c()
  for(cat in cats) {
    label <- ifelse(cat == "Total",
                    "This category encompasses all non-immigrant visa categories per year",
                    labels_subset[ visa_category == cat, description ])
    notes <- c(notes, sprintf("<li>%s: %s</li>", cat, label))
  }
  
  header <- ifelse(length(notes) > 1, "<h5><b>Notes</b></h5>", "<h5><b>Note</b></h5>")
  all_text <- sprintf("<h5><b>Visa Descriptions</b></h5> <ul>%s</ul>", paste(notes, collapse = ""))

  HTML(all_text)
}

get_sorted_categories <- function(cats) {
  if("Total" %in% cats) {
    return( c("Total", sort(cats[ cats != "Total" ])) )
  }
  return(sort(cats))
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