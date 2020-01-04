## Filename:    home.R
## Author:      Angelina Li
## Date:        2020/01/03
## R version:   3.3.3
## Description: Contains UI and server code managing home page
##              Contains top-level home_ui and home_server functions
##              All internal functions are prepended with 'home'

###### UI ######

home_ui <- function() {
  tabPanel(
    "Home",
    h3("Welcome to the Non-Immigrant Visa Data Explorer!"),
    p("Feel free to take a look around ", emo::ji("slightly_smiling_face")),
    hr(),
    
      
    h4("What is this?"),
    p("This app explores some public data on how many non-immigrant visas are ",
      "issued and denied ('refused') per year, and by nation of origin. ", 
      "The data used for this project was obtained from the US State Department's ", 
      "public records."),
    br(),
    
    h4("What's a 'non-immigrant visa'?"),
    p("From the ",
      a(href="https://www.cbp.gov/travel/international-visitors/visa-waiver-program/requirements-immigrant-and-nonimmigrant-visas",
        "CBP website"),
      " - Non-immigrant visas are for people 'seeking to ", 
      "enter the United States on a temporary basis'. They differ from ", 
      "immigrant visas, which are for people 'who inten[d] to live and ", 
      "work permanently in the United States'. "
    ),
    br(),
      
    h4("Where can I find more information on this project?"),
    p(a(href="https://github.com/angelinahli/visas", 
        "The GitHub repository is your best bet."),
      " Alternatively, you can email me at angelinahelenli [at] gmail [dot] com."
    ),
    br(),
    
    h4("Is this open source? Can I play around with the data?"),
    p("Yes and yes! You may use this project however you like. To download the data, ",
      "navigate to Summary > Get the Data, or download the .RDS files directly ",
      "from GitHub."
    ),
    br(),
    
    h4("Why did you make this?"),
    p("I wanted to be able to tinker with this dataset! And, I'm an immigrant, ",
      "and I think immigration is generally interesting."
    ),
    br(),
    
    h4("Why only non-immigrant visas?"),
    p("I haven't gotten around to looking at immigrant visa statistics yet, but ",
      "I'm sure those numbers are interesting too. I and a lot of my friends have ",
      "non-immigrant visas, so I thought this would be a good start."
    )
  )
}

get_accordion_card <- function(number, data_parent, question_text, answer_text) {
  question_id <- paste0("home_q", number)
  answer_id <- paste0("home_a", number)
  template <- "
    <div class='card'>
      <div class='card-header' id='%s'>
        <h2 class='mb-0'>
          <button class='btn btn-link' type='button' data-toggle='collapse' 
           data-target='#%s' aria-expanded='true' aria-controls='%s'>
            %s
           </button>
        </h2>
      </div>
             
      <div id='%s' class='collapse show' aria-labelledby='%s' data-parent='#%s'>
        <div class='card-body'>
          %s
        </div>
      </div>
    </div>
  "
  rendered <- sprintf(template, 
                      question_id, answer_id, answer_id, question_text, answer_id, 
                      question_id, data_parent, answer_text)
  return(rendered)
}

###### Server ######

home_server <- function(input, output, session) {
  
}