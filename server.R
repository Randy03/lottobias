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

set.seed(666)

guess_good_df <- create_lotto_df(10000)

shinyServer(function(input, output) {
  
  output$winnersTable <- renderTable(lotto_df %>% arrange(desc(date)) %>% slice_head(n=7))

  
  store <- reactiveValues()
  
  store$list <- data.frame("count"=0)
  
  store$num_select <- c(paste(lotto_df %>% sample_n(1) %>% select(number)),
                    paste(guess_good_df %>% sample_n(1) %>% select(number)))
  
  
  
  store$sample_idx <- sample(c(1, 2), 1)
  
  output$sel_option_1 <- renderText(paste(store$num_select[store$sample_idx]))
  
  output$sel_option_2 <- renderText(paste(store$num_select[if(store$sample_idx==1){2} else{1}]))
  
  
  observeEvent(input$select1,{
    
    if (sample_idx == 1){
      
      store$list <- store$list %>% add_row(count=0)
      
    }
    
    else{store$list <- store$list %>% add_row(count=1)}
    
    
    store$num_select <- c(paste(lotto_df %>% sample_n(1) %>% select(number)),
                      paste(guess_good_df %>% sample_n(1) %>% select(number)),
                      sample(c(1, 2), 1))
    
    
    store$sample_idx <- sample(c(1, 2), 1)

    
  })
  
  
  observeEvent(input$select2,{
    
    if (sample_idx != 1){
      
      store$list <- store$list %>% add_row(count=0)
      
    }
    
    else{store$list <- store$list %>% add_row(count=1)}
    
    
    store$num_select <- c(paste(lotto_df %>% sample_n(1) %>% select(number)),
                          paste(guess_good_df %>% sample_n(1) %>% select(number)),
                          sample(c(1, 2), 1))
    
    store$sample_idx <- sample(c(1, 2), 1)
    
  })
    

   
    output$guess_right <- renderText(paste("You've found ", sum(store$list$count), " fake numbers !!"))
    
    output$total_per <- renderText(paste0(sum(store$list$count), "/", length(store$list$count)-1,"\n","(",100*sum(store$list$count)/(length(store$list$count)-1), "%)" )) 
    
  
  
})
