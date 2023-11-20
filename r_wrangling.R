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

#load excel worksheet
adjust<- function(file, sheet){
  attach(file)
  if(sheet == "MB"){
    file$Mb_millions <- (file$Mb_millions / cpi) * 100
    file$Mb_millions <- diff(log(file$Mb_millions))
  }
  if(sheet == "macros"){
    file$UNRATE <- diff(log(file$UNRATE))
  }
  
}

#FOCUS ON GETTING THE DATA IN STATIONARY FORMMMMMM
for(items in files_){
  if(items == "rates_and_currencies.xlsx"){
    mb <- read_excel(items, sheet = "MB")
    rt <- read_excel(items, sheet = "rates")
    
    mb <- adjust(mb, "MB")
    mb <- merge(rt, mb, all = TRUE)
    write_xlsx(mb, "n_rates_and_currencies.xlsx", col_names = TRUE)
  }
  if(items == "macros_tz.xlsx"){
    mc <- read_excel(items, sheet = "macros")
    mc <- adjust(mc, "macros")
    write_xlsx(mc, "n_macro_tz.xlsx", col_names = TRUE)
    
  }
  # make each excel sheet a time series object 
  # difference and log the values eliminate rows with na's if you have to
  # turn it back to a dataframe 
  # resave it to its original excel file
  # loop it
  
  
}




  
  
  