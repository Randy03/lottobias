library(shiny)
library(shinyjs)

lotto_number <- fluidRow(
  column(8,
         numericInput("favNumber",label="What's your lucky favourite number",value=0,min = 0),
         h4("Last winners"),
         tableOutput('winnersTable')     
  ),
  column(4,
         verbatimTextOutput("lottoResult")
  )
)

lotto_good_one <- {
  h4("Which one is a real lotto number?")
  
}

lotto_bad_one <- {
  fluidRow(
    column(12, h3("Which number is more likely to win lottery?", align='center'),
           
           
    br(),
    
    fluidRow(
      column(4, actionButton('right', textOutput('random_selector_1')), align='center'),
      column(4, actionButton('middle', textOutput('random_selector_2')), align='center'),
      column(4, actionButton('left', textOutput('random_selector_3')), align='center')),
    
    br(),
    br(),
    
    fluidRow(
      paste("You have selected ", '7', ' real random numbers and ', '3',' "Human-Trap numbers"'), align='center'),
      br(),
      column(12, align='center', '70%')
    )
  )

}

shinyUI(
  fluidPage(
    titlePanel("Lotto bias"),
    navlistPanel(
      id = "tabset",
      "Games",
      tabPanel("What's your favourite number", lotto_number),
      tabPanel("Guess the good one", lotto_good_one),
      tabPanel("Guess the bad one", lotto_bad_one)
    )
  )
)
