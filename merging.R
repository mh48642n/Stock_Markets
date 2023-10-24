rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
path <- ("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
setwd(path)

#loads libraries that 
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(lubridate)

#puts file names into list object to iterate through it better
files <- list("macro_tz.xlsx")
#creates data objects and attaches them to better join them
x <- read_excel("indices_tz.xlsx")
y <- read_excel("debt_tz.xlsx")

attach(x)
#x <- transform_weekly(x)
#y <- transform_weekly(y)
#l <- transform_weekly(l)
attach(y)

#function merges data into one sheet
merge <- function(){
  #joins x and y dataframes
  merged <- left_join(x, y, by = "dates")
  merged <- fortify.zoo(dailyavg_weekly(merged))
  merged$dates = merged$Index
  
  # iterates through files in working directory and joins them together
  for(items in files){
    k <- read_excel(items)
    attach(k)
    k <- monthly_weekly(k)
    
    merged <- left_join(merged, k, by = "dates")
  }
  merged <- merged[, -c(1, 5, 6, 8, 10, 11)]
  
  #makes the dates column of type date and formats the dates a specific way 
  merged <- merged %>% mutate(dates = as.Date(dates, format = "%Y-%m-%d")) 
  
  #returns joined tables
  return(merged)  
}

#collapses daily data into weekly increments and finds the average value for the week
dailyavg_weekly <- function(z){
  z <- z %>% mutate(dates = as.Date(z$dates))
  z <- z %>% as.xts(order.by = .$dates)  
  z <- apply.weekly(z, mean)
  
  return(z)
}
#makes monthly into weekly increments and shifts the root date by 5 days
monthly_weekly <- function(l){
  l <- l %>% mutate(dates = as.Date(l$dates))
  new_dates <- (ceiling_date(l$dates, unit = "week") + 5)
  l$dates <- new_dates
  return(l)
}

#returns merged data and stores it in a data object
data <- merge()

#gives a summary of the dataset
summary(data)

#to save excel workbook
#write_xlsx(data, "dataset.xlsx", col_names = TRUE)




