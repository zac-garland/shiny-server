library(rvest)
library(tidyverse)
pacman::p_load("janitor")
pacman::p_load("plotly")
pacman::p_load("shinythemes")
pacman::p_load("shiny")
pacman::p_load("lubridate")
pacman::p_load("shinyWidgets")
pacman::p_load("tidyquant")
library(bootstraplib)
 

current_mortgage_rates <- function() {
  pg <- read_html("https://www.bankrate.com/mortgages/mortgage-rates/")


  pg %>%
    html_nodes("p") %>%
    html_text() %>%
    tibble(line = .) %>%
    filter(str_detect(line, "APR")) %>%
    slice(1) %>%
    pull() %>%
    str_extract_all("\\d+(?:\\.\\d+)?%") %>%
    unlist() %>%
    tibble(value = .) %>%
    rowid_to_column() %>%
    left_join(
      tribble(
        ~group, ~type,
        "thirty_year", "rate",
        "thirty_year", "APR",
        "fifteen_year", "rate",
        "fifteen_year", "APR",
        "arm_loan", "rate",
        "arm_loan", "APR"
      ) %>%
        rowid_to_column(),
      by = "rowid"
    ) %>%
    select(-rowid) %>%
    mutate(value = str_replace(value, "%", "") %>% as.numeric()) %>%
    spread(type, value) %>%
    arrange(desc(group)) %>%
    select(mortgage = group, int_rate = rate, APR)
}





## Calculate payment
pay <- function(principal = 250000, 
                interest = 3.5, 
                duration = 30, 
                payfreq = 1, 
                firstpay = 1, 
                compoundfreq = 1) {
  r <- interest / (100 * 12 / compoundfreq)

  if (firstpay > 1) {
    principal <- principal * (1 + r)^((firstpay - 1) / compoundfreq)
    duration <- duration - (firstpay - 1) / 12
  }

  payment <- principal * r / (1 - (1 + r)^(-duration * 12 / compoundfreq)) * payfreq / compoundfreq
 list(r = r, payment = payment, principal = principal)
}



clean_pay <- function(pay){
  pay %>% 
  tibble(group = names(.),
         value = .) %>%
    unnest(value) %>%
    spread(group,value)
}



## Amortization table
amort <- function(principal = 250000, 
                  interest = 3.5, 
                  duration = 30, 
                  payfreq = 1, 
                  firstpay = 1, 
                  compoundfreq = 1,
                  home_appreciation = .00309) {
  loan = principal
  pay <- pay(principal, interest, duration, payfreq, firstpay, compoundfreq)
  data <- data.frame(month = seq(0, duration * 12))
  data$payment <- 0
  data$payment[(data$month - firstpay) >= 0 & (data$month - firstpay) %% payfreq == 0] <- pay$payment
  i <- which(data$payment != 0)
  i <- i[length(i)]
  data$payment[i] <- 0
  data$payment[i] <- pay$payment * (duration - (firstpay - 1) / 12) * 12 / payfreq - sum(data$payment)
  data$totalPayed <- cumsum(data$payment)

  data$principal <- NA
  data$principal[1] <- principal
  idx <- (data$month - firstpay) >= 0 & (data$month - firstpay) %% compoundfreq == 0
  idx.pr <- which(idx)[-length(idx)] + compoundfreq - 1
  if (any(idx.pr > max(data$month))) {
    idx.pr <- idx.pr[-which(idx.pr > max(data$month))]
  }

  if (firstpay > 1) {
    data$principal[firstpay] <- pay$principal
  }
  data$principal[idx.pr] <- (1 + pay$r)^seq_len(length(idx.pr)) * pay$principal - ((1 + pay$r)^seq_len(length(idx.pr)) - 1) / pay$r * pay$payment * compoundfreq / payfreq
  data$principal[nrow(data)] <- 0


  data <- as_tibble(data) %>% 
    clean_names() %>% 
    # rowwise() %>% 
    mutate(towards_principal = abs(principal-lag(principal))) %>% 
    na.omit() %>% 
    mutate(towards_principal = with_order(month,cumsum,towards_principal)) %>%
    rowwise() %>% 
    mutate(interest = total_payed - towards_principal) %>% 
    select(month,payment,interest,towards_principal,total_payed,principal) %>% 
    ungroup()
  
  if(duration < 30){
   data <- data %>% 
      full_join(tibble(month = seq(max(.$month),(max(.$month)+(12*(30-duration))))))
  }
  

  
  list(pay = clean_pay(pay),
       data = data %>% 
         fill(total_payed,interest,towards_principal,payment,principal) %>% 
         mutate(monthly_growth_rate = 1+home_appreciation,
                cumulative_growth_rate = with_order(month,cumprod,monthly_growth_rate),
                home_value = (loan*cumulative_growth_rate)-loan) %>% 
         select(month:principal,home_value)
       )
}





formal_args <- function(name) {
  formal_args <- args(name)
  fun_name <- match.call(name)[2] %>%
    as.character()
  
  capture.output(formal_args) %>%
    tibble(line = .) %>%
    filter(line != "NULL") %>%
    mutate(line = str_replace(line, "function ", fun_name)) %>%
    pull(line) %>%
    writeLines()
}
