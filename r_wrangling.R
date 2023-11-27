rm(list = ls())
#important to do this part to get 
setwd("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
getwd()

#load library
library(readxl)
library(writexl)
library(tidyverse)
library(forecast)
library(xts)

files_ <- c("rates_and_currencies.xlsx", "macro_tz.xlsx")
cpi <- 242.2575
inflation <- 1.83

#load excel worksheet and differences data
#adjust<- function(file, sheet){
  attach(file)
  if(sheet == "MB"){
    file$Mb_millions <- (file$Mb_millions / cpi) * 100
    file$Mb_millions <- diff(log(file$Mb_millions))
  }
  if(sheet == "macros"){
    file$UNRATE <- diff(log(file$UNRATE))
  }
  
#}

#FOCUS ON GETTING THE DATA IN STATIONARY FORMMMMMM
for(items in files_){
  if(items == "rates_and_currencies.xlsx"){
    mb <- read_excel(items, sheet = "MB")
    rt <- read_excel(items, sheet = "rates")
    
    mb <- adjust(mb, "MB")
    mb <- merge(rt, mb, all = TRUE)
    #write_xlsx(mb, "n_rates_and_currencies.xlsx", col_names = TRUE)
  }
  if(items == "macros_tz.xlsx"){
    mc <- read_excel(items, sheet = "macros")
    mc <- adjust(mc, "macros")
    #write_xlsx(mc, "n_macro_tz.xlsx", col_names = TRUE)
    
  }
  # make each excel sheet a time series object 
  # difference and log the values eliminate rows with na's if you have to
  # turn it back to a dataframe 
  # resave it to its original excel file
  # loop it
  
}
dt <- read_excel("standard_dataset_tsz.xlsx")
attach(dt)
adjust <- function(){
  dt <- subset(dt, dates < "2020-01-01" & dates > "2000-11-30")
  dt <- dt[, -c(15, 17:25)]
   
  stocks <- dt[, c(1, 2, 4)]
  stocks <- stocks %>% 
    group_by(dates = ceiling_date(dates, unit = "month")) %>%
    summarise(
      sp500 = mean(sp_500, na.rm = TRUE), 
      russell = mean(russell_2000, na.rm = TRUE)
    )
  debt <- dt[, c(1, 9:11)]
  debt <- debt %>%
    group_by(dates = ceiling_date(dates, unit = "month")) %>%
    summarise(
      debt = mean(Public_Debt, na.rm = TRUE),
      govt = mean(Govt_Held, na.rm = TRUE),
      public = mean(Public_Held, na.rm = TRUE)
    )
  yields <- dt[, c(1, 12, 13, 15)]
  yields <- yields %>%
    group_by(dates = ceiling_date(dates, unit = "month")) %>%
    summarise(
      corp_baa = mean(Baa, na.rm = TRUE),
      rate_yr = mean(yields_10yr, na.rm = TRUE),
      funds_rt = mean(eff_fund_rates, na.rm = TRUE)
      
    )
  macro <- dt[, c(1, 8, 14)]
  macro <- macro %>% 
    group_by(dates = ceiling_date(dates, unit = "month")) %>%
    summarise(
      unrate = mean(UNRATE, na.rm = TRUE),
      monetary_base = mean(Mb_millions,na.rm = TRUE)
    
    )
  
  macro_debt <- merge(macro, debt, all = TRUE)
  stock_yields <- merge(stocks, yields, all = TRUE)
  
  data <- merge(stock_yields, macro_debt, all = TRUE)
}

data <- subset(data, dates >= "2000-12-01" & "2019-12-01" >= dates)
write_xlsx(data, "monthly_dataset.xlsx", col_names = TRUE)


  
  
  