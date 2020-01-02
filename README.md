# US Nonimmigrant Visas
## Some Data Exploration

Did you know that the US Travel Department releases statistics on the [processing of nonimmigrant visas](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics.html)? I certainly didn't.

I'm making this app to satisfy my own desire to explore and tinker with this data in an interactive way.

### Project Objectives
* Create an R Shiny app exploring various dimensions of the data:
    * By year, nationality and visa: What do acceptance rates look like over time?
    * By year and visa: Which visas are most difficult to obtain (highest refusal rate, or maybe refusal - waived/overcome rate)

### To dos
* Clean all the data:
    * Transform y-n-v into a long dataset on the y-n-v level

#### Data Documentation
(1) Workload (Issued, Refused, and Waived/Overcome) by visa category and year
    
* Source: See [Nonimmigrant Worldwide Issuance and Refusal Date by Visa Category](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics.html)
* Cleaning steps:
    * Download each years' data as a PDF
    * Use the built-in OCR reader in Nuance Power PDF to convert each document to an Excel sheet
    * For each document, check that the # visas issued + # visas refused = total workload, and manually audit 10% of the numbers (including any rows that failed the previous check)
    * Import into R and clean each worksheet, and convert into a year-category level dataset.

(2) Issuances by visa category, year and nationality

* Source: See [Nonimmigrant Visa Issuances by Visa Class and by Nationality](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics.html)
* Cleaning steps: Download data, import each worksheet into R and convert into a year-category-nationality level dataset.