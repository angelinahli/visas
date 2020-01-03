# US Nonimmigrant Visas
## Some Data Exploration

Did you know that the US Travel Department releases statistics on the [processing of nonimmigrant visas](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics.html)? I certainly didn't.

I'm making this app to satisfy my own desire to explore and tinker with this data in an interactive way.

### Project Objectives
* Create an R Shiny app exploring various dimensions of the data:
    * By year, nationality and visa: What do acceptance rates look like over time?
    * By year and visa: Which visas are most difficult to obtain (highest refusal rate, or maybe refusal - waived/overcome rate)

### To dos

* Final data auditing:
    * Check whether the numbers from datasets (1) and (2) match up

* Graph ideas:
    * Summary
        * Initial visa documentation detailing what each visa means
        * Download datasets for your own purposes
    * Overall Issuances 
        * Over all years, total number of issuances per visa category (bar graph)
        * Number of issuances per visa category and year, including an overall category (line graph)
        * Table of total issuances per visa category, per year
    * Issuances and Refusals
        * Proportion of applications issued and either issued, waved or overcome, per category and year
        * Table of proportion issued and issued/waived/overcome, per visa category and year
    * Issuances Per Country
        * Number of issuances in all countries, given a category and year (sliding scale)
        * Animated version of the above? See [this link](https://rstudio-pubs-static.s3.amazonaws.com/315157_73b802e0532c4ea3839f98afc0378ca1.html).
        * Table of issuances per country / year

#### Data Documentation
##### (1) Workload (Issued, Refused, and Waived/Overcome) by visa category and year
    
* Source: See [Nonimmigrant Worldwide Issuance and Refusal Date by Visa Category](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics.html)
* Cleaning steps:
    * Download each years' data as a PDF
    * Use the built-in OCR reader in Nuance Power PDF to convert each document to an Excel sheet
    * For each document, check that the # visas issued + # visas refused = total workload, and manually audit 10% of the numbers (including any rows that failed the previous check)
    * Import into R and clean each worksheet, and convert into a year-category level dataset.

##### (2) Issuances by visa category, year and nationality

* Source: See [Nonimmigrant Visa Issuances by Visa Class and by Nationality](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics.html)
* Cleaning steps: Download data, import each worksheet into R and convert into a year-category-nationality level dataset.

##### (3) Visa symbol descriptions

* Sources:
    * [travel.state.gov List of Nonimmigrant Visa Symbols](data/input/NonimmigrantVisaSymbols.pdf), also available to download [here](https://travel.state.gov/content/dam/visas/Statistics/Non-Immigrant-Statistics/MonthlyNIVIssuances/Nonimmigrant%20Visa%20Symbols.pdf).
    * [travel.state.gov 2004-2008 Table of Nonimmigrant Visas Issued by Classification](data/input/FY08-AR-TableXVIB.pdf), also available to download [here](https://travel.state.gov/content/dam/visas/Statistics/FY08-AR-TableXVI(B\).pdf)
    * [travel.state.gov Border Crossing Card Info Page](https://travel.state.gov/content/travel/en/us-visas/tourism-visit/border-crossing-card.html)
* Cleaning steps:
    * Cleaned and consolidate visa categories across datasets (1) and (2)
    * Created crosswalk linking each visa symbol to its description, using the links above to populate crosswalk in order provided.