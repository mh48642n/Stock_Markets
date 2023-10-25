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
files <- list("macro_tz.xlsx", "debt_ceiling_tz.xlsx", "debt_tz.xlsx")
#creates data objects and attaches them to better join them
x <- read_excel("indices_tz.xlsx")
attach(x)

#function merges data into one sheet
merge <- function(){
  #This helps when iterating through the files
  confirm_ = 1
  #joins x and y dataframes
  merged <- weekly_(x, confirm_)
  rownames(merged) <- NULL
  merged <- merged[,-c(2)]
  colnames(merged)[1] = "dates"
  
  confirm_ = 2
  
  # iterates through files in working directory and joins them together
  for(items in files){
    k <- read_excel(items)
    attach(k)
  
    k <- weekly_(k, confirm_)
    
    merged <- left_join(merged, k, by = "dates")
    confirm_ = confirm_ + 1 
      }
  
  
  #makes the dates column of type date and formats the dates a specific way 
  merged <- merged %>% mutate(dates = as.Date(dates, format = "%Y-%m-%d")) 
  
  #returns joined tables
  return(merged)  
}

#collapses daily/monthly data into weekly increments and finds the average value for the week
weekly_ <- function(z, confirm_){
  z = z
  if(confirm_ == 1){
    #find the weekly average for different values
    z <- z %>% mutate(dates = as.Date(z$dates))
    z <- z %>% as.xts(order.by = .$dates)
    z <- apply.weekly(z, mean)
    z <- fortify.zoo(z)
    print(z)
    }
  else if(confirm_ == 2){
    #finds the week of each first of the month put places it on Friday
    z <- z %>% mutate(dates = as.Date(z$dates))
    new_dates <- (ceiling_date(z$dates, unit = "week") + 5)
    z$dates <- new_dates
    print(z)
    }
  else if(confirm_ == 3 || 4){
    if(confirm_ == 3){  
      #finds the week the date of the debt ceiling change
      #and sets a variable called 
      z <- z %>% mutate(dates = as.Date(z$dates))
      new_dates <- (ceiling_date(z$dates, unit = "week") - 2)
      z <- z %>% add_column(ceiling_dates = z$dates) %>%
        mutate(dates = new_dates)
      print(x)
          }
    else{
      #finds the week the debt is raised and then make the week the identifier 
      #locates the dates when the date was raised
      z <- z %>% mutate(dates = as.Date(z$dates))
      new_dates <- (ceiling_date(z$dates, unit = "week") - 2)
      z <- z %>% mutate(action_dates = dates) %>%
        mutate(dates = new_dates)
      print(z)
      z <- z[z$dates == z$action_dates, ]
        } 
      
    }  
  
  
  return(z)
}


#returns merged data and stores it in a data object
data <- merge()
data <- data[, -c(4, 6, 9:12, 14:15)]
#gives a summary of the dataset
summary(data)
str(data)

#gives a correlation matrix
corr <- data[, c(2:6, 8)] 
cor(corr, use = "complete.obs")

#to save excel workbook
#write_xlsx(data, "dataset.xlsx", col_names = TRUE)




