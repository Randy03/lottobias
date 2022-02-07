library(shiny)
library(ggplot2)
library(dplyr)

set.seed(123)
create_lotto_df <-function(n){
  number <- do.call(paste0, replicate(5, sample(0:9, n, TRUE), FALSE))
  date <- seq(Sys.Date()-(n-1), Sys.Date(), by="days")
  return(data.frame(date,number))
}

lotto_df <- create_lotto_df(10000)

shinyServer(function(input, output) {
  output$winnersTable <- renderTable(lotto_df %>% arrange(desc(date)) %>% slice_head(n=7))
  
})
