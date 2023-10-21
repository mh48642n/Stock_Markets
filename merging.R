rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
path <- ("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
setwd(path)

library(tidyverse)
library(readxl)

files <- list("macro_tz.xlsx")
x <- read_excel("indices_tz.xlsx")
y <- read_excel("debt_tz.xlsx")
attach(x)
attach(y)

merge <- function(){
  merged <- left_join(x, y, by = "dates")

  for(items in files){
    x <- read_excel(items)
    attach(x)
    
    merged <- left_join(merged, x, by = "dates")
    
  }
  
  merged <- merged %>% mutate(dates = as.Date(dates, format = "%Y-%m-%d"))
  
  return(merged)  
}
data <- merge()
data <- data[, -c(4,5,7,9,10)]
summary(data)

str(data)




