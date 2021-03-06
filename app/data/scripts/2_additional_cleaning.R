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

regional <- data.table(readRDS(file.path(intermed_path, "regional.rds")))
workload <- data.table(readRDS(file.path(intermed_path, "workload.rds")))

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
  region == "Total", 
  c("year", "visa_category", "issued")]
rg_reference[ , key := paste0(year, " ", visa_category) ]
rg_reference[ , issued := round(issued) ] # this will probably cause problems with .5s unfortunatelt
rg_reference <- rg_reference[ key %in% wl_reference$key, ]
rg_reference <- rg_reference[ order(year, visa_category, issued) ]

print(all_equal(wl_reference, rg_reference))

###### Create some helper variables ######

workload[ , perc_issued := issued / workload ]
workload[ , perc_granted := (issued + waived_overcome) / workload ]

# no rows where issued > granted, as should be accurate
print(nrow(workload[ perc_issued > perc_granted, ]))

# only 31 rows where there were no waived_overcome
print(nrow(workload[ perc_issued == perc_granted, ]))
print(nrow(workload[ waived_overcome == 0, ]))

###### Create clean reference dictionary for label sources ######

sources <- data.table(
  Source = c("[A]", "[B]", "[C]"),
  Description = c("List of Nonimmigrant Visa Symbols (See GitHub README)",
                  "2004-2008 Table of Nonimmigrant Visas Issued by Classification (See GitHub README)",
                  "Border Crossing Card Info Page"),
  URL = c("https://travel.state.gov/content/dam/visas/Statistics/Non-Immigrant-Statistics/MonthlyNIVIssuances/Nonimmigrant%20Visa%20Symbols.pdf",
          "https://travel.state.gov/content/dam/visas/Statistics/FY08-AR-TableXVI(B).pdf",
          "https://travel.state.gov/content/travel/en/us-visas/tourism-visit/border-crossing-card.html")
)

labels <- left_join(labels, sources, by=c("Short.Description.Source"="URL") )

###### Make relevant variables factors ######

get_visa_factor_levels <- function(dt) {
  levels <- sort(unique(labels$Visa.Category))
  levels <- levels[levels %in% unique(dt$visa_category)]
  levels <- c("Total", levels)
  return(levels)
}

get_nation_factor_levels <- function(dt) {
  all_levels <- unique(dt$nationality)
  level1 <- c("Total")
  level2 <- as.character(sort(all_levels[grepl(" - Total$", all_levels)]))
  sorted_levels <- c(level1, level2)
  level3 <- as.character(sort(all_levels[!(all_levels %in% sorted_levels)]))
  final <- c(sorted_levels, level3)
  return(final)
}


workload$visa_category <- factor(workload$visa_category, get_visa_factor_levels(workload))
regional$visa_category <- factor(regional$visa_category, get_visa_factor_levels(regional))
regional$nationality <- factor(regional$nationality, get_nation_factor_levels(regional))

region_levels <- c("Africa", "Asia", "Europe", "North America", "Oceania", 
                   "South America", "Unknown", "Total")
regional$region <- factor(regional$region, region_levels)
regional$Alpha.3.code <- factor(regional$Alpha.3.code)

# patch nationality names to remove - Total
nat_order <- as.character(sort(unique(regional$nationality)))
nat_order <- gsub(" - Total$", "", nat_order)
regional$nationality <- factor(gsub(" - Total$", "", regional$nationality), nat_order)

###### Reorder and relabel datasets ######

labels <- labels[ , c("Visa.Category", "Short.Description", "Source") ]
colnames(labels) <- c("visa_category", "description", "source")

workload <- workload[ , c("year", "visa_category", "issued", "refused", "workload", 
                          "waived_overcome", "perc_issued", "perc_granted") ]
workload <- workload[ order(year, visa_category) ]

colnames(regional)[7] <- "country_code"
regional <- regional[ , c("year", "visa_category", "nationality", "region", "country_code",
                          "issued", "is_total") ]
regional <- regional[ order(year, visa_category, region, nationality) ]

###### Finally, save all datasets ######

saveRDS(workload, file.path(output_path, "workload.rds"))
saveRDS(regional, file.path(output_path, "regional.rds"))
saveRDS(sources, file.path(output_path, "label_sources.rds"))
saveRDS(labels, file.path(output_path, "labels.rds"))
