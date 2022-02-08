library(shiny)
#library(shinyjs)
library(shinythemes)

css <- "
@import url('https://fonts.googleapis.com/css2?family=Audiowide&family=Bai+Jamjuree:wght@300;400&family=Press+Start+2P&display=swap');

mark {
  padding: 0;
  background-color: orange;
  color: black;
}

h1 {
  font-family: 'Audiowide', cursive;
}

h5 {
  font-family: 'Bai Jamjuree', sans-serif;
}

"

lotto_number <- fluidRow(
  column(2),
  column(3,
         numericInput("favNumber",label="What's your lucky favourite number",value=0,min = 0),
         h4("Last winners"),
         tableOutput('winnersTable')     
  ),
  br(),
  br(),
  column(5,
         h2(textOutput("favNumberResult",inline=TRUE))
  )
)

lotto_good_one <- fluidRow(
  column(12, h3("Which one is a real lotto number?"), align="center"),
  br(),
  br(),
  fluidRow(
    column(3, br()),
    column(3, verbatimTextOutput('textGoodBadOption1'), align="center"), 
    column(6, actionButton("buttonGoodBadOption1", "this one"), align="left")
    
  ),
  br(),
  fluidRow(
    column(3, br()),
    column(3, verbatimTextOutput('textGoodBadOption2'), align="center"),
    column(6, actionButton("buttonGoodBadOption2", "this one"), align="left")
    
  ),
  br(),
  fluidRow(
    column(12, textOutput('goodBadResult'), align="center"))
  
)

lotto_trap_one <- fluidRow(
        column(12, h3("Which number is more likely to win lottery?", align='center'),
        br(),
         fluidRow(
           column(4, actionButton('buttonTrap1', textOutput('textTrap1')), align='center'),
           column(4, actionButton('buttonTrap2', textOutput('textTrap2')), align='center'),
           column(4, actionButton('buttonTrap3', textOutput('textTrap3')), align='center')),
         br(),
         br(),
         fluidRow(
           textOutput('trapResult'), align='center'),
         br()
  )
)

shinyUI(
  fluidPage(theme = shinytheme("superhero"),tags$head(
    tags$style(HTML(css))
  ),
    titlePanel(h1("Lotto bias")),
    navlistPanel(widths = c(2, 10),
      id = "tabset",
      "Games",
      tabPanel(h5("What's your favourite number"), lotto_number),
      tabPanel(h5("Guess the good one"), lotto_good_one),
      tabPanel(h5("Guess the bad one"), lotto_trap_one)
    )
  )
)
