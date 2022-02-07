library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

set.seed(123)
create_lotto_df <-function(n){
  number <- do.call(paste0, replicate(5, sample(0:9, n, TRUE), FALSE))
  date <- seq(Sys.Date()-(n-1), Sys.Date(), by="days")
  return(data.frame(date,number))
}

paint_text <- function(text,paint){
  return(gsub(paint,paste("<mark>",paint,"</mark>",sep=""),text))
}

lotto_df <- create_lotto_df(10000)


shinyServer(function(input, output) {
  
  lotto_table <- reactive({
    lotto_df %>% 
      arrange(desc(date))  %>% 
      slice_head(n=7) %>% 
      mutate(date=as.character(date)) %>% 
      mutate(
        number = if_else(str_detect(number,as.character(input$favNumber)),
                         paint_text(number,as.character(input$favNumber)),
                         number)
      )
  })
  lotto_result <- reactive({
    rights <- lotto_df %>% filter(str_detect(number,as.character(input$favNumber))) %>% nrow()
    total <- nrow(lotto_df)
    pctg <- rights*100/total
    c(rights,total,pctg)
  })
  
  output$winnersTable <- renderTable(lotto_table(), sanitize.text = function(x) x)
  output$lottoResult <- renderText(paste("You guess right \n",lotto_result()[1],"lottos of \n",lotto_result()[2],"lottos \n","(",lotto_result()[3],"%)"))
    
  
})
