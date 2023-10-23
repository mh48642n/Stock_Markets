rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
path <- ("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
setwd(path)

#loads libraries that 
library(tidyverse)
library(readxl)
library(writexl)

#puts file names into list object to iterate through it better
files <- list("macro_tz.xlsx")

#creates data objects and attaches them to better join them
x <- read_excel("indices_tz.xlsx")
y <- read_excel("debt_tz.xlsx")
attach(x)
attach(y)

#function merges data into one sheet
merge <- function(){
  #joins x and y dataframes
  merged <- left_join(x, y, by = "dates")

  # iterates through files in working directory and joins them together
  for(items in files){
    x <- read_excel(items)
    attach(x)
    
    merged <- left_join(merged, x, by = "dates")
    
  }
  
  #makes the dates column of type date and formats the dates a specific way 
  merged <- merged %>% mutate(dates = as.Date(dates, format = "%Y-%m-%d"))
  
  #returns joined tables
  return(merged)  
}

#returns merged data and stores it in a data object
data <- merge()

#strips excel sheet for specific columns
data <- data[, -c(4,5,7,9,10)]

#gives a summary of the dataset
summary(data)

#to save excel workbook
#write_xlsx(data, "dataset.xlsx", col_names = TRUE)




