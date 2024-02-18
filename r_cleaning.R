rm(list = ls())
setwd("C:/Users/marvi/OneDrive/Documents/GitHub/Stock_Markets/excel_sheets")
getwd()

library(readxl)
library(tidyverse)
library(writexl)


#creates a vector with the other files that I want to join together
#----add expected inflation
files <- c("russell_2000_d.xlsx", "debt_tz.xlsx", "DGS10.xlsx", 
           "DEXUSEU.xlsx","DAAA.xlsx","DBAA.xlsx","DFF.xlsx", 
           "futures_data.xlsx", "monthly_gdp.xlsx", "M2NS.xlsx", 
           "budget_surplus_deficit.xlsx", "cpi_core.xlsx",
           "cpi_release_dates.xlsx","auction_dates.xlsx",
           "job_report.xlsx","expect_1yr.xlsx","expect_10yr.xlsx",
           "meeting_dates.xlsx","debt_ceiling_tz.xlsx")

#function that attaches and cleans/transforms the data
join_ <- function(){
  #reads and attaches the first file; change the file to russell 
  head_ <- inter_at("sp_500.xlsx")
  
  #attaches each file and joins them together
  for(value in files){
   tail_ <- inter_at(value)
   head_ <- merge(head_, tail_, all = TRUE) 
  }  
  
  #subsets data by date range
  head_ <- subset(head_, dates > "1999-05-31")
  
  return(head_)   
}

#function attaches and makes formats date in specific way
inter_at <- function(a){
  a <- read_excel(a)
  
  attach(a)
  
  #this transforms the datatype of the dates column
  a <- a %>% mutate(dates = as.Date(dates))
  return(a)
}

data_set <- join_()

# In excel two columns were made
# Raised and suspend and these are binary variables

write_xlsx(data_set, "C:/Users/marvi/OneDrive/Documents/GitHub/Stock_Markets/excel_sheets/full_data_set.xlsx")

 
  