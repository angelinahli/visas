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

get_visa_select <- function(dt, select_id, selected=c("Total"), multiple = TRUE) {
  # return a select component with id select_id, with options sourced
  # from dt$visa_category
  # assume visa_category is a factor
  choices <- as.character(sort(unique(dt$visa_category)))
  selection <- choices
  if(!is.na(selected)) { selection <- selected }
  select <- pickerInput(select_id, label = "Visa Categories",
                        choices = choices,
                        selected = selection,
                        options = list(`actions-box` = TRUE),
                        multiple = multiple)
}

get_notes_html <- function(visa_categories = c(), additional_notes = c()) {
  # prints out standard notes on visa categories, as well as any additional
  # notes
  labels_subset <- labels[visa_category %in% visa_categories, ]
  # if there are any notes to print
  if(nrow(labels_subset) > 0 | length(additional_notes) > 0) {
    
    notes <- c()
    for(note in additional_notes) {
      new_bullet <- sprintf("<li>%s</li>", note)
      notes <- c(notes, new_bullet)
    }
    for(cat in unique(labels_subset$visa_category)) {
      label <- labels_subset[ visa_category == cat, description ]
      new_bullet <- sprintf("<li>The symbol '%s' refers to: %s</li>", cat, label)
      notes <- c(notes, new_bullet)
    }
    
    header <- ifelse(length(notes) > 1, "<h4>Notes</h4>", "<h4>Note</h4>")
    all_text <- c(header, "<ul>", notes, "</ul>")
    
    HTML(paste(all_text, collapse = ""))
  }
}