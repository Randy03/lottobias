library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
source('./trapNumbers.R')



create_lotto_df <-function(n){
  #set.seed(123)
  number <- do.call(paste0, replicate(5, sample(0:9, n, TRUE), FALSE))
  date <- seq(Sys.Date()-(n-1), Sys.Date(), by="days")
  return(data.frame(date,number))
}

paint_text <- function(text,paint){
  return(gsub(paint,paste("<mark>",paint,"</mark>",sep=""),text))
}

create_trap_numbers_df <- function(n){
  nice <- do.call(paste0, replicate(12, sample(0:9, 1000, TRUE), FALSE))
  return(data.frame(nice))
}

generate_random_lotto_number <- function(l){
  return(paste0(replicate(l,sample(0:9,1,TRUE)),collapse=""))
}

#lotto_hist_df <- create_lotto_df(10000)
lotto_hist_df <- read.csv("Lottery_NY_Lotto_Winning_Numbers__Beginning_2001.csv") %>% 
  select(Draw.Date,Winning.Numbers) %>% 
  mutate(Winning.Numbers = str_replace_all(Winning.Numbers," ",""))
names(lotto_hist_df) <- c('date','number')

#trap_numbers_df <- create_trap_numbers_df(10000)
trap_numbers_df <- data.frame(trap_numbers)




shinyServer(function(input, output) {
  
  #Favourite Numbers
  lotto_numbers_table_hist <- reactive({
    req(as.numeric(input$favNumber)>10)
    lotto_hist_df %>% 
      arrange(desc(date))  %>% 
      slice_head(n=7) %>% 
      mutate(date=as.character(date)) %>% 
      mutate(
        number = if_else(str_detect(number,input$favNumber),
                         paint_text(number,input$favNumber),
                         number)
      )
  })
  
  lotto_numbers_result <- reactive({
    rights <- lotto_hist_df %>% filter(str_detect(number,input$favNumber)) %>% nrow()
    total <- nrow(lotto_hist_df)
    pctg <- round(rights*100/total,2)
    c(rights,total,pctg)
  })
  
  output$winnersTable <- renderTable(lotto_numbers_table_hist(), sanitize.text = function(x) x)
  output$favNumberResult <- renderText(
    paste("You guess right \n",lotto_numbers_result()[1],
          "lottos of \n",lotto_numbers_result()[2],
          "lottos \n","(",lotto_numbers_result()[3],"%)"))
  
  #Good numbers
  
  output_numbers <- c(paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1)),generate_random_lotto_number(12))
  sample_seq <- sample(1:2)
  fake_index <- if_else(all(output_numbers[sample_seq] == output_numbers),2,1)

  good_bad_values <- reactiveValues(
    output_numbers_rnd=output_numbers[sample_seq],
    total=0,
    right=0,
    pctg=0)
  
  observeEvent(input$buttonGoodBadOption1,{
    good_bad_values$total <- good_bad_values$total + 1
    if (fake_index == 1){
      good_bad_values$right <- good_bad_values$right + 1
    }
    good_bad_values$pctg <- round(100 * good_bad_values$right / good_bad_values$total,2)
    
    output_numbers <- c(paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1)),generate_random_lotto_number(12))
    
    sample_seq <- sample(1:2)
    fake_index <<- if_else(all(output_numbers[sample_seq] == output_numbers),2,1)
    good_bad_values$output_numbers_rnd <- output_numbers[sample_seq]
  })
  
  observeEvent(input$buttonGoodBadOption2,{
    good_bad_values$total <- good_bad_values$total + 1
    if (fake_index == 1){
      good_bad_values$right <- good_bad_values$right + 1
    }
    good_bad_values$pctg <- round(100 * good_bad_values$right / good_bad_values$total,2)
    
    output_numbers <- c(paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1)),generate_random_lotto_number(12))
    
    sample_seq <- sample(1:2)
    fake_index <<- if_else(all(output_numbers[sample_seq] == output_numbers),2,1)
    good_bad_values$output_numbers_rnd <- output_numbers[sample_seq]
  })
  
  output$textGoodBadOption1 <- renderText(good_bad_values$output_numbers_rnd[1])
  output$textGoodBadOption2 <- renderText(good_bad_values$output_numbers_rnd[2])
  
  output$goodBadResult <- renderText(
    if_else(good_bad_values$total>0,
    paste("You've found ",good_bad_values$right,
          " fake numbers !!\n",good_bad_values$right,
          "/",good_bad_values$total,
          "(",good_bad_values$pctg,
          ")%"),""))
  
  
  
  
  #Trap numbers
  
  trap_numbers_rnd <- c(0,0,0)
  good_index <- sample(1:3,1)
  trap_numbers_rnd[good_index] <- paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1))
  for (index in (1:3)[-good_index]){
    trap_numbers_rnd[index] <- paste(trap_numbers_df %>% slice_sample(n=1))
  }
  trap_values <- reactiveValues(trap_numbers_rnd=trap_numbers_rnd,
                                wrong=0,
                                right=0,
                                pctg=0)
  
  
  observeEvent(input$buttonTrap1,{
      if (good_index == 1){
        trap_values$right = trap_values$right + 1 
      }else{
        trap_values$wrong = trap_values$wrong + 1 
      }
      trap_values$pctg = round(100*trap_values$right/(trap_values$right+trap_values$wrong),2)
      good_index <<- sample(1:3,1)
      trap_values$trap_numbers_rnd[good_index] <- paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1))
      for (index in (1:3)[-good_index]){
        trap_values$trap_numbers_rnd[index] <- paste(trap_numbers_df %>% slice_sample(n=1))
      }
    }
  )
  
  observeEvent(input$buttonTrap2,{
    if (good_index == 2){
      trap_values$right = trap_values$right + 1 
    }else{
      trap_values$wrong = trap_values$wrong + 1 
    }
    trap_values$pctg = round(100*trap_values$right/(trap_values$right+trap_values$wrong),2)
    good_index <<- sample(1:3,1)
    trap_values$trap_numbers_rnd[good_index] <- paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1))
    for (index in (1:3)[-good_index]){
      trap_values$trap_numbers_rnd[index] <- paste(trap_numbers_df %>% slice_sample(n=1))
    }
  }
  )
  observeEvent(input$buttonTrap3,{
    if (good_index == 3){
      trap_values$right = trap_values$right + 1 
    }else{
      trap_values$wrong = trap_values$wrong + 1 
    }
    trap_values$pctg = round(100*trap_values$right/(trap_values$right+trap_values$wrong),2)
    good_index <<- sample(1:3,1)
    trap_values$trap_numbers_rnd[good_index] <- paste(lotto_hist_df %>%select(number)%>% slice_sample(n=1))
    for (index in (1:3)[-good_index]){
      trap_values$trap_numbers_rnd[index] <- paste(trap_numbers_df %>% slice_sample(n=1))
    }
  }
  )
  
  
  output$textTrap1 <- renderText(trap_values$trap_numbers_rnd[1])
  output$textTrap2 <- renderText(trap_values$trap_numbers_rnd[2])
  output$textTrap3 <- renderText(trap_values$trap_numbers_rnd[3])
  output$trapResult <- renderText(
    if_else(trap_values$right+trap_values$wrong>0,
    paste("You select",trap_values$right,
          "real random numbers and",trap_values$wrong,
          "human-trap number \n",trap_values$pctg,"%"),""))
  
  })
