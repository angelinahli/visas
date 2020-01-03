## Filename:    cleaning_regional.R
## Author:      Angelina Li
## Date:        2020/01/20
## R version:   3.3.3
## Description: Clean data on the issuance of visas by category,
##              year and nationality, from 1997-2018

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
library(zoo)

###### Defining Paths ######

base_path     <- file.path("C:/Users/ali2/Desktop/visas/data")
input_path    <- file.path(base_path, "input")
intermed_path <- file.path(base_path, "intermediate")
output_path   <- file.path(base_path, "output")

setwd(base_path)

###### Load in and clean data ######

import_sheet <- function(filename, sheet_name, year) {
  df <- data.table(read_xlsx(file.path(input_path, filename), sheet=sheet_name))
  # remove empty rows
  colnames(df)[1] <- "nationality"
  df <- df[ !is.na(nationality), ]
  
  # set region
  df[ is.na(`Grand Total`), region := nationality ]
  df[ , region := na.locf(df$region)]
  
  # drop region rows
  df <- df[ !is.na(`Grand Total`), ]
  
  # rename nationalities and some regions
  df[ nationality == "Grand Totals", region := "Total" ]
  df[ nationality == "Grand Totals", nationality := "Total" ]
  
  df[ grepl("^Totals for ", nationality), is_total := 1 ]
  df[ is_total == 1, nationality := paste0(region, " - Total") ]
  df[ nationality == "Total", is_total := 1 ]
  
  print(paste0("# rows where grand total != total visas: ", nrow(df[`Grand Total` != `Total Visas`])))
  
  # reshape data
  df$nationality <- factor(df$nationality)
  df <- gather(df, key="visa_category", value="issued", -nationality, -region, -is_total, factor_key = T)
  
  ## clean visa_category
  df$visa_category <- gsub("-", "", df$visa_category)
  
  df$year <- year
  return(df)
}

filename <- "FYs97-18_NIVDetailTable.xlsx"
all_dfs <- list()
for(year in 1999:2018) {
  sheet_name <- paste0("FY", substring(toString(year), 3))
  all_dfs[[year]] <- import_sheet(filename, sheet_name, year)
}

overall_df <- rbindlist(all_dfs)

###### Updating country names for consistency ######

# check nationality consistency
for(nat in unique(overall_df$nationality)) {
  available <- overall_df[nationality == nat, year]
  missing <- setdiff(1999:2018, available)
  if(length(missing) > 0) {
    print(paste0("The nationality '", nat, "' is missing from years: ", paste(missing, collapse=", ")))
  }
}
# [1] "The nationality 'Cape Verde' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The nationality 'Congo, Dem. Rep. of the (Congo Kinshasa)' is missing from years: 2012, 2013, 2014, 2015, 2016, 2017, 2018"
# [1] "The nationality 'Congo, Rep. of the (Congo Brazzaville)' is missing from years: 2012, 2013, 2014, 2015, 2016, 2017, 2018"
# [1] "The nationality 'Swaziland' is missing from years: 2018"
# [1] "The nationality 'Serbia and Montenegro' is missing from years: 2013, 2014, 2015, 2016, 2017, 2018"
# [1] "The nationality 'Macau S.A.R.' is missing from years: 1999"
# [1] "The nationality 'Timor-Leste' is missing from years: 1999"
# [1] "The nationality 'Serbia' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007"
# [1] "The nationality 'Kosovo' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007"
# [1] "The nationality 'Montenegro' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007"
# [1] "The nationality 'South Sudan' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010"
# [1] "The nationality 'Congo, Dem. Rep. of the (Kinshasa)' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011"
# [1] "The nationality 'Congo, Rep. of the (Brazzaville)' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011"
# [1] "The nationality 'Cabo Verde' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013"
# [1] "The nationality 'Eswatini' is missing from years: 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017"

## where it seems like some countries have been renamed, let's update those naming conventions
name_dictionary <- c(
  # old = new
  "Cape Verde" = "Cabo Verde",
  "Congo, Dem. Rep. of the (Congo Kinshasa)" = "Congo, Dem. Rep. of the (Kinshasa)",
  "Congo, Rep. of the (Congo Brazzaville)" = "Congo, Rep. of the (Brazzaville)",
  "Swaziland" = "Eswatini"
)
for(old_name in names(name_dictionary)) {
  overall_df[ nationality == old_name, nationality := name_dictionary[old_name] ]
}

###### Updating visa categories ######

for(cat in unique(overall_df$visa_category)) {
  available <- overall_df[visa_category == cat, year]
  missing <- setdiff(2013:2018, available)
  if(length(missing) > 0) {
    print(paste0("The category '", cat, "' is missing from years: ", paste(missing, collapse=", ")))
  }
}
# [1] "The category 'DCREW' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'H1A' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'H2R' is missing from years: 2013, 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'Q2' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'Q3' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'V1' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'V2' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'V3' is missing from years: 2014, 2015, 2016, 2017, 2018"
# [1] "The category 'T6' is missing from years: 2013"

# Seems largely consistent - for now, just update the 'Grand Total' name
## Maybe drop the Total Visas category since it currently is the same as the Grand Total

overall_df <- overall_df[ visa_category != "Total Visas", ]
overall_df[ visa_category == "Grand Total", visa_category := "Total" ]


###### Basic summation checks ######

## check totals per year, per region, and per nationality
totals_df <- overall_df[ is_total == 1, ]
for(yr in 1999:2018) {
  summed <- overall_df[ is.na(is_total) & year == yr & visa_category != "Total", 
                        sum(issued, na.rm = T) ]
  comparison <- totals_df[ region == "Total" & visa_category == "Total" & year == yr, issued ]
  if(!all(summed == comparison)) {
    print(paste0("Grand totals don't match summed totals for year ", year))

  }
  regions <- c("Africa", "Asia", "Europe", "North America", "Oceania", 
               "South America", "Unknown")
  for(reg in regions) {
    summed <- overall_df[ is.na(is_total) & year == yr & visa_category != "Total" & region == reg, 
                          sum(issued, na.rm = T) ]
    comparison <- totals_df[ region == reg & visa_category == "Total" & year == yr, issued ]
    if(!all(summed == comparison)) {
      print(paste0("Regional totals don't match summed totals for year ", yr, " and region ", reg))
    }
  }
  
  print(paste0("Done with year ", yr))
}

saveRDS(overall_df, file.path(intermed_path, "regional.rds"))
