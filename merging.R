rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
#important to do this part to get 
path <- ("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
setwd(path)

#loads libraries that is needed
library(tidyverse)
library(readxl) 
library(writexl)
library(xts)
library(lubridate)

#wrangle <- read_excel("marketable_securities.xlsx")
month_ly <- function(wrangle){
  l <- wrangle
  l <- filter(l, (l$`Security Type Description` == "Marketable" & 
                 (l$`Security Class Description` == "Bill" | l$`Security Class Description`== "Note")))
  l$`Security Class Description` <- as.factor(l$`Security Class Description`)
  
  l <- l %>% mutate(
      bills = case_when(
        l$`Security Class Description` == "Bill" ~ l$`Securities Sold Count`),
      notes = case_when(
        l$`Security Class Description` == "Note" ~ l$`Securities Sold Count`)
      )
  l <- l %>% group_by(dates = l$`Record Date`)  
  bills <- na.omit(l[, c(11, 9)])
  notes <- na.omit(l[, c(11, 10)])
  
  l <- left_join(bills, notes, by = ("dates" = "dates"))
  l <- l %>% arrange(l)
  
  return(l)
}

#puts file names into list object to iterate through it better
files <- list("macro_tz.xlsx", "marketable_securities.xlsx", "debt_ceiling_tz.xlsx", "debt_tz.xlsx")
files_ <-list("meeting_dates.xlsx", "release_dates_10.xlsx") 
#adding data object
merged <- read_excel("indices_tsz.xlsx")
attach(merged)


#function merges data into one sheet
merge_ <- function(){
  #This helps when iterating through the files
  confirm_ = 1
  right <- morph_(merged, confirm_)
  merged <- left_join(merged, right, by = "dates")
  confirm_ = 2
  
  # iterates through files in working directory and joins them together
  for(items in files){
    k <- read_excel(items)
    attach(k)
    
    if(confirm_ != 2){
     k <- morph_(k, confirm_)
     merged <- left_join(merged, k, by = "dates")
    }
    else{
      k <- k %>% 
        mutate(
          cpi_inflation = ((CPI - lag(CPI))/ lag(CPI)) * 100,
          core_inflation = ((CORE_CPI - lag(CORE_CPI))/ lag(CORE_CPI)) * 100 
        )
      
      merged <- merge(merged, k, by.x = "dates", by.y = "dates", all.x = T, all.y = T)
      }
    confirm_ = confirm_ + 1 
  }
  
  futures <- read_excel("DFF.xlsx", sheet = "Futures")
  funds_rate <- read_excel("DFF.xlsx", sheet = "DFF")
  
  merged <- left_join(merged, funds_rate, by = "dates") 
  merged <- left_join(merged, futures, by = "dates")
  #returns joined tables
  merged <- merged %>% 
    mutate(
      future_funds_rate = 100 - Price
    )
  
  for(items in files_){
    k <- read_excel(items)
    attach(k)
    
    k <- k %>% mutate(dates = as.Date(dates)) 
    
    #merged <- left_join(merged, k, by = "dates")
    merged <- merge(merged, k, by.x = "dates", by.y = "dates", all.x = T, all.y = T)
  }
  
  return(merged)  
}

#collapses daily/monthly data into weekly increments and finds the average value for the week
morph_ <- function(z, confirm_){
  z = z
  if(confirm_ == 1){
    #find the weekly average for different values
    z <- z %>% mutate(dates = as.Date(z$dates))
    z$russell_vol <- as.numeric(z$russell_vol)
    z$sp_500 <- as.numeric(z$sp_500)
    #groups the stock prices 
    z <- z %>% group_by(dates = ceiling_date(z$dates, unit = "week") - 2) %>% 
      summarise(avg_rl2000 = mean(russell_2000, na.rm = TRUE),
                avg_sp500 = mean(sp_500, na.rm = TRUE)) 
  }
  else if(confirm_ == 3){
    z <- month_ly(z)
    
  }
  else if(confirm_ == 4 || 5){
    if(confirm_ == 4){  
      #finds the week the date of the debt ceiling change
      #and sets a variable called 
      z <- z %>% mutate(dates = as.Date(z$dates))
      z <- z %>% add_column(ceiling_dates = z$dates) 
      print(z)
          }
    else{
      #finds the week the debt is raised and then make the week the identifier 
      #locates the dates when the date was raised
      z <- z %>% mutate(dates = as.Date(z$dates))
     }
    }
  return(z)
  }  
 
#returns merged data and stores it in a data object
data <- merge_()

print("WRANGLINGGGGG.......")
#wrangle adjust prices for inflation to 2015 dollars
wrangle <- function(data){
  cpi_2015 <- 237.00175
  
  data <- data %>% mutate(sp_500 = as.numeric(as.character(sp_500)), 
                          russell_vol = as.numeric(as.character(russell_vol)))
  data$meeting = ifelse(is.na(data$meeting), 0, data$meeting)
  data$release_info = ifelse(is.na(data$release_info), 0, data$release_info)
  data$Public_Debt = data$Public_Debt / (10^7)
  data$Public_Held = data$Public_Held / (10^7)
  data$Govt_Held
  
  data <- data %>% 
    mutate(
      russell_2000 = 100 * (russell_2000 / cpi_2015),
      avg_rl2000 = 100 * (avg_rl2000 / cpi_2015),
      avg_sp500 = 100 * (avg_sp500 / cpi_2015),
      sp_500 =  100 * (sp_500 / cpi_2015),
      Govt_Held = 100 * (Govt_Held / cpi_2015),
      Public_Held = 100 * (Govt_Held / cpi_2015),
      Public_Debt = 100 * (Public_Debt / cpi_2015),
    )
  
   
  return(data)
}
data <- wrangle(data)
data <- subset(data, dates > "2000-1-1" & dates < "2020-01-01", select = c(1:7, 15:25, 27:30))

#gives a summary of the data set
summary_ <- as.data.frame(summary(data[, c(2:9, 15:22)]))
str(data[, c(2:9, 15:22)])

#gives a correlation matrix
cor(data[, c(2:9, 15:19)], use = "complete.obs")

#to save excel workbook
#write_xlsx(data, "dataset_tsz.xlsx", col_names = TRUE)

