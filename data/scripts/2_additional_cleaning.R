## Filename:    2_additional_cleaning.R
## Author:      Angelina Li
## Date:        2020/01/02
## R version:   3.3.3
## Description: Take some aditional cleaning measures in order to have data in the format I'd like

# Clear old variables
rm(list = ls())

###### Load in Packages ######

library(compare)
library(data.table)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(tidyr)
library(writexl)

###### Defining Paths ######

base_path     <- file.path("C:/Users/ali2/Desktop/visas/data")
input_path    <- file.path(base_path, "input")
intermed_path <- file.path(base_path, "intermediate")
output_path   <- file.path(base_path, "output")

###### Load in Data ######

regional <- data.table(readRDS(file.path(output_path, "regional.rds")))
workload <- data.table(readRDS(file.path(output_path, "workload.rds")))

###### Create visa category crosswalk ######

regional_categories <- as.character(unique(regional$visa_category))
workload_categories <- unique(workload$visa_category)
all_visas <- sort(unique(c(
  regional_categories, workload_categories
)))

print(setdiff(regional_categories, workload_categories))
print(setdiff(workload_categories, regional_categories))

# write_xlsx(data.table(all_visas), file.path(intermed_path, "all_visa_categories.xlsx"))

###### Import in crosswalk ######

cw_path <- file.path(intermed_path, "Visa Category Crosswalks.xlsx")

consolidation <- data.table(read.xlsx(cw_path, sheet = "Consolidation", startRow = 2))
name_map <- as.character(consolidation$New.Name)
names(name_map) <- consolidation$Old.Name

labels <- data.table(read.xlsx(cw_path, sheet = "Labels", startRow = 2))

update_visa_names <- function(df) {
  for(name in as.character(unique(df$visa_category))) {
    if (name %in% names(name_map)) {
      df[ visa_category == name, visa_category := name_map[name] ]
    }
  }
  return(df)
}

regional <- update_visa_names(regional)
workload <- update_visa_names(workload)

print(setdiff(unique(regional$visa_category), labels$Visa.Category))
print(setdiff(unique(workload$visa_category), labels$Visa.Category))

###### Check totals match in regional v.s. workload ######

wl_reference <- workload[, c("year", "visa_category", "issued")]
wl_reference <- wl_reference[ order(year, visa_category, issued), ]
wl_reference$year <- as.integer(wl_reference$year)
wl_reference[ , key := paste0(year, " ", visa_category) ]


rg_reference <- regional[ 
  region == "Total" & 
    year %in% unique(wl_reference$year) & 
    visa_category %in% unique(wl_reference$visa_category), 
  c("year", "visa_category", "issued")]
rg_reference <- rg_reference[ order(year, visa_category, issued) ]
rg_reference[ , key := paste0(year, " ", visa_category) ]

print(all_equal(wl_reference, rg_reference))
