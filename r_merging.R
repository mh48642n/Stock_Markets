rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
#important to do this part to get 
setwd("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")

#loads libraries that is needed
library(tidyverse)
library(readxl) 
library(writexl)
library(xts)
library(lubridate)

#adding data object
merged <- read_excel("indices_tsz.xlsx")
attach(merged)

#adjust for inflation
cpi_2015 <- 242.2575

#adjusting prices for inflation
merged <- merged %>% 
  mutate(
    sp_500 = as.numeric(as.character(sp_500)), 
    russell_vol = as.numeric(as.character(russell_vol)))
merged <- merged %>% 
  mutate(
    russell_2000 = 100 * (russell_2000 / cpi_2015),
    sp_500 =  100 * (sp_500 / cpi_2015),
  )
  
#puts file names into list object to iterate through it better
files <- list("debt_ceiling_tz.xlsx", "debt_tz.xlsx", "stationary_data.xlsx",
              "meeting_dates.xlsx", "release_dates_10.xlsx") 

#function merges data into one sheet
merge_ <- function(){
  #This helps when iterating through the files
  confirm_ = 1
  right <- morph_(merged, confirm_)
  right <- right %>% 
    mutate(
      s_avgr2000 = log(avg_rl2000) - lag(log(avg_rl2000)),
      s_avgsp500 = log(avg_sp500) - lag(log(avg_sp500))
      
    )
  merged <- merged %>% 
    mutate(
      s_russellp = log(russell_2000) - lag(log(russell_2000)),
      s_russellv = log(russell_vol) - lag(log(russell_vol)),
      s_sp500_p = log(sp_500) - lag(log(sp_500)),
      s_sp500_v = log(sp_500_vol) - lag(log(sp_500_vol)),
      
    ) 
  merged <- left_join(merged, right, by = "dates")
  confirm_ = 2
  
  # iterates through files in working directory and joins them together
  for(items in files){
    k <- read_excel(items)
    attach(k)
    
    if(confirm_ == 3){
     k <- morph_(k, confirm_)
    }
    else{
     k <- k %>% mutate(dates = as.Date(dates, format = "%Y-%m-%d"))  
      
    }
    
    merged <- merge(merged, k, all = TRUE)
    confirm_ = confirm_ + 1
  }
  
  return(merged)  
}

#collapses daily/monthly data into weekly increments and finds the average value for the week
morph_ <- function(z, confirm_){
  z = z
  if(confirm_ == 1){
    #find the weekly average for different values
    z <- z %>% mutate(dates = as.Date(z$dates))
    #groups the stock prices 
    z <- z %>% group_by(dates = ceiling_date(z$dates, unit = "week") - 2) %>% 
      summarise(avg_rl2000 = mean(russell_2000, na.rm = TRUE),
                avg_sp500 = mean(sp_500, na.rm = TRUE)) 
    print(z)
    
    }
  if(confirm_ == 3){  
    #finds the week the date of the debt ceiling change
    #and sets a variable called 
    z <- z %>%
     mutate(  
        Govt_Held = (100 * (Govt_Held / cpi_2015))/ 10^6,
        Public_Held = (100 * (Govt_Held / cpi_2015))/ 10^6,
        Public_Debt = (100 * (Public_Debt / cpi_2015))/ 10^6
     )
    
    }
  return(z)
}  
 
#returns merged data and stores it in a data object
data <- merge_()
data$raised <- ifelse(data$action_taken == "Raised Debt Ceiling", 1,  0)
data$suspend <- ifelse(data$action_taken == "Suspend Debt Ceiling", 1, 0)

print("WRANGLINGGGGG.......")
#wrangle adjust prices for inflation to 2015 dollars
wrangle <- function(data){
  
  data$meeting = ifelse(is.na(data$meeting), 0, data$meeting)
  data$release_info = ifelse(is.na(data$release_info), 0, data$release_info)
  data$raised = ifelse(is.na(data$raised), 0, data$raised)
  data$suspend = ifelse(is.na(data$suspend), 0, data$suspend)
  
  return(data) 
}
data <- wrangle(data)
data <- subset(data, (dates > "2000-12-31" & dates < "2020-01-01") & dates != "2020-01-01", 
               select = c(1:8, 13:31))
data <- data[, c(1, 3:9, 14:17, 18:23, 2, 24, 26:27, 10:13)]
data <- data[, -c(12)]
#gives a summary of the data set
#summary_ <- as.data.frame(summary(data[, c(2:11, 15:22)]))
str(data[, c(2:9, 15:22)])

#gives a correlation matrix
cor(data[, c(2:17)], use = "complete.obs")

#to save excel workbook
write_xlsx(data, "dataset_tsz.xlsx", col_names = TRUE)

#wrangle <- read_excel("marketable_securities.xlsx")
month_ly <- function(wrangle){
l <- read_excel("marketable_securities.xlsx")
l <- wrangle
attach(l)
bills <- subset(l, Security_Class_Description == "Bill",
                select = c(1, 4))
notes <- subset(l, Security_Class_Description == "Note", 
                select = c(1, 4))

l <- left_join(bills, notes, by = ("Record_Date" = "Record_Date"))
l <- l %>% arrange(l) %>% 
  rename(
    "dates" = "Record_Date", 
    "bills" = "Securities_Sold_Count.x",
    "notes" = "Securities_Sold_Count.y"
  ) 
l <- l %>% mutate(
  dates = as.Date(dates, format = "%Y-%m-%d"),
  bills = as.numeric(bills),
  notes = as.numeric(notes)) 

return(l)
}

