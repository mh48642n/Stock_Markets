rm(list = ls())
setwd("C:/Users/marvi/OneDrive/Documents/GitHub/Stock_Markets/excel_sheets")
getwd()

library(readxl)
library(tidyverse)
library(writexl)


#creates a vector with the other files that I want to join together
files <- c("debt_tz.xlsx", "DGS10.xlsx", "DEXUSEU.xlsx",
           "rates_and_monetary_base.xlsx","DAAA.xlsx","DBAA.xlsx","DFF.xlsx", 
           "futures_data.xlsx", "monthly_gdp.xlsx", "M2NS.xlsx", 
           "budget_surplus_deficit.xlsx","cpi_release_dates.xlsx", 
           "meeting_dates.xlsx", "debt_ceiling_tz.xlsx")

#function that attaches and cleans/transforms the data
join_ <- function(){
  #reads and attaches the first file; change the file to russell 
  head_ <- inter_att("indices_tsz.xlsx")
  
  #attaches each file and joins them together
  for(value in files){
   tail_ <- inter_att(value)
   head_ <- merge(head_, tail_, all = TRUE) 
  }  
  
  #subsets data by date range
  head_ <- subset(head_, dates > "1999-05-31")
  
  return(head_)   
}

#function attaches and makes formats date in specific way
inter_att <- function(a){
  
  attach(a)
  
  #this transforms the datatype of the dates column
  a <- a %>% mutate(dates = as.Date(dates))
  return(a)
}


data_set <- join_()

write_xlsx(data_set, "C:/Users/marvi/OneDrive/Documents/Github/Stock_Markets/full_dataset.xlsx")

 
  