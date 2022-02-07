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

human_trap <- c(123456, 112233, 445566, 666666)

set.seed(666)

guess_good_df <- create_lotto_df(10000)

shinyServer(function(input, output) {
  output$winnersTable <- renderTable(lotto_df %>% arrange(desc(date)) %>% slice_head(n=7))
  
  store_panel3 <- reactiveValues()
  
  store_panel3$list <- list(0)
  
  store_panel3$num_select <- list(paste(lotto_df %>% sample_n(1) %>% select(number)),
                               paste(lotto_df %>% sample_n(1) %>% select(number)),
                        paste(guess_good_df %>% sample_n(1) %>% select(number)),
                        sample(c(1, 2, 3), 3))
  
  output$random_selector_1 <- renderText(store_panel3$num_select[4])
  output$random_selector_2 <- renderText(store_panel3$num_select[5])
  output$random_selector_3 <- renderText(store_panel3$num_select[6])
  
  
})



  