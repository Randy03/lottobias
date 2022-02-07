library(shiny)
library(shinyjs)

lotto_number <- {
  numericInput("favNumber",label="What's your lucky favourite number",value=0,min = 0)
  h4("Last winners")
  dataTableOutput('winnersTable')
}

lotto_good_one <- {
  h4("Which one is a real lotto number?")
  
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
