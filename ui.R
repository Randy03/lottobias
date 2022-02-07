library(shiny)
#library(shinyjs)

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
  
  fluidRow(
    column(12, h3("Which one is a real lotto number?"), align="center"),
    br(),
    br(),
    fluidRow(
      column(3, br()),
      column(3, verbatimTextOutput('sel_option_1'), align="center"), 
      column(6, actionButton("select1", "this one"), align="left")

      ),
    br(),
    fluidRow(
      column(3, br()),
      column(3, verbatimTextOutput('sel_option_2'), align="center"),
      column(6, actionButton("select2", "this one"), align="left")

    ),
    br(),
    fluidRow(
      column(12, textOutput('guess_right'), align="center")),
    
    br(),
    fluidRow(
      column(12, textOutput('total_per'), align="center"))
    
  )
}

lotto_bad_one <- {
  h4("Which number is more likely to win lottery")

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
