## Filename:    cleaning_issuance_refusal.R
## Author:      Angelina Li
## Date:        2020/01/02 
## R version:   3.3.3
## Description: Clean aggregate data on the issuance and refusal of visas by category
##              and year, from 2013-2017

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

setwd(base_path)

###### Load in and clean data ######

import_all_sheets <- function(import_fn, year) {
  import_path <- file.path(intermed_path, "issuance_refusal", import_fn)
  cols <- c("visa_category", "issued", "refused", "workload", "waived_overcome")
  df1 <- read_xlsx(import_path, sheet="Sheet1", skip=2, col_names=cols)
  df2 <- read_xlsx(import_path, sheet="Sheet2", skip=2, col_names=cols)
  
  df <- data.table(rbind(df1, df2))
  df <- df[ !is.na(issued), ]
  
  ## identified during audit
  transformations <- c("1.000000"="F1", "2.000000"="F2", "3.000000"="F3", 
                       "11.000000"="L1", "12.000000"="L2")
  for(initial in names(transformations)) {
    df[visa_category == initial, visa_category := transformations[initial]]
  }
  
  ## identified via audit
  df$visa_category <- gsub("^\\*+|\\*+$", "", df$visa_category)
  
  # fix cases where numeric columns are in fact characters
  for(col in c("issued", "refused", "workload", "waived_overcome")) {
    if( class(df[, get(col)]) == "character" ) {
      df[ , c(col) := gsub("^\\*+|\\*+$|,", "", df[, get(col)]) ]
      df[ , c(col) := as.numeric(as.character(df[, get(col)])) ]
    }
  }
  
  df$year <- year
  return(df)
}

all_dfs <- list()
all_dfs[[2017]] <- import_all_sheets("FY 2017NIVWorkloadbyVisaCategory.xlsx", 2017)
for(year in c(2013, 2014, 2015, 2016, 2018)) {
  fn <- paste0("FY", year, "NIVWorkloadbyVisaCategory.xlsx")
  all_dfs[[year]] <- import_all_sheets(fn, year)
}

overall_df <- rbindlist(all_dfs)

###### Basic validation checks ######

for(yr in 2013:2018) {
  cols_to_sum <- c("issued", "refused", "workload", "waived_overcome")
  summed <- overall_df[ visa_category != "Grand Totals" & year == yr, 
                        lapply(.SD, sum, na.rm=T), .SDcols = cols_to_sum]
  comparison <- overall_df[ visa_category == "Grand Totals" & year == yr, 2:5]
  if (!all(summed == comparison)) {
    print(paste0("Grand totals don't match summed totals for year ", yr))
  }
}

# rename totals
overall_df[ visa_category == "Grand Totals", visa_category := "Total" ]

## check if there are any visa categories that are available only in certain years
for(cat in unique(overall_df$visa_category)) {
  available <- overall_df[visa_category == cat, year]
  missing <- setdiff(2013:2018, available)
  if(length(missing) > 0) {
    print(paste0("The category '", cat, "' is missing from years: ", paste(missing, collapse=", ")))
  }
}
## results:
# [1] "The category 'F3' is missing from years: 2016, 2017, 2018"
# [1] "The category 'M3' is missing from years: 2016, 2017, 2018"
# [1] "The category 'T1' is missing from years: 2013, 2015, 2016, 2018"
# [1] "The category 'T6' is missing from years: 2013, 2014"

saveRDS(overall_df, file.path(output_path, "issuance_refusal.rds"))