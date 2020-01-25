## Filename:    faqs.R
## Author:      Angelina Li
## Date:        2020/01/24
## R version:   3.3.3
## Description: Contains UI and server code managing faqs page
##              Contains top-level faqs_ui and faqs_server functions
##              All internal functions are prepended with 'faqs'

faqs_ui <- function() {
  tabPanel(
    "About",
    h4("What is this?", class="faq-question"),
    p("This app explores some public data on how many non-immigrant visas are ",
      "issued and denied ('refused') per year, and by nation of origin. ", 
      "The data used for this project was obtained from the US State Department's ", 
      "public records."),
    div(class="faq-spacer"),
    
    h4("What's a 'non-immigrant visa'?", class="faq-question"),
    p("From the ",
      a(href="https://www.cbp.gov/travel/international-visitors/visa-waiver-program/requirements-immigrant-and-nonimmigrant-visas",
        "CBP website"),
      " - Non-immigrant visas are for people 'seeking to ", 
      "enter the United States on a temporary basis'. They differ from ", 
      "immigrant visas, which are for people 'who inten[d] to live and ", 
      "work permanently in the United States'. "
    ),
    div(class="faq-spacer"),
    
    h4("Where can I find more information on this project?", class="faq-question"),
    p(a(href="https://github.com/angelinahli/visas", 
        "The GitHub repository is your best bet."),
      " Alternatively, you can email me at angelinahelenli [at] gmail [dot] com."
    ),
    div(class="faq-spacer"),
    
    h4("Is this open source? Can I play around with the data?", class="faq-question"),
    p("Yes and yes! You may use this project however you like. To download the data, ",
      "navigate to Summary > Get the Data, or download the .RDS files directly ",
      "from GitHub."
    ),
    div(class="faq-spacer"),
    
    h4("Why did you make this?", class="faq-question"),
    p("I wanted to be able to tinker with this dataset! And, I'm an immigrant, ",
      "and I think immigration is generally interesting."
    ),
    div(class="faq-spacer"),
    
    h4("Why only non-immigrant visas?", class="faq-question"),
    p("I haven't gotten around to looking at immigrant visa statistics yet, but ",
      "I'm sure those numbers are interesting too. I and a lot of my friends have ",
      "non-immigrant visas, so I thought this would be a good start."
    )
    
  )
}

faqs_server  <- function(input, output, session) {
  
}
