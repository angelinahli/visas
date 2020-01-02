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
    * Transform y-v data into a usable data format, and then into a long dataset on the y-v level
