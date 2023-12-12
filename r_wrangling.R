rm(list = ls())
#important to do this part to get 
setwd("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
getwd()

#load library
library(readxl)
library(writexl)
library(tidyverse)
library(readr)

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
res <- read_excel("reserve.xlsx")
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

#working on wrangling to weekly 
data <- read_xlsx("indices_tsz.xlsx")
fund <- read_xlsx("funds_futures.xlsx")
debt <- read_xlsx("debt_tz.xlsx")
attach(fund)
attach(data)
attach(debt)

cpi <- 242.2575
rate <- 1.83
  
data$dates <- as.Date(data$dates) 
fund$dates <- as.Date(fund$dates)
debt$dates <- as.Date(debt$dates)


fund <- fund %>% 
  mutate(
    Baa = ((1 + Baa) / (1 + rate)) - 1,
    yields_10yr = ((1 + yields_10yr) / (1 + rate)) - 1,
    eff_fund_rates  = ((1 + eff_fund_rates) / (1 + rate)) - 1,
    expected_fundsrate = ((1 + expected_fundsrate) / (1 + rate)) - 1
  )

data <- data %>%
  mutate(
    russell_2000 = (100 * (russell_2000 / cpi)),
    sp_500 = (100 * (sp_500 / cpi)) 
  )

debt <- debt %>% 
  mutate(
    Govt_Held = (100 * (Govt_Held / cpi))/ 10^6,
    Public_Held = (100 * (Public_Held / cpi))/ 10^6,
    Public_Debt = (100 * (Public_Debt / cpi))/ 10^6
    
  )

data_2 <- data %>% group_by(dates = ceiling_date(dates, unit = "week") - 2) %>%
  summarise( 
    avg_russell = mean(russell_2000, na.rm = TRUE),
    avg_standards = mean(sp_500, na.rm = TRUE),
    avg_rv = mean(russell_vol, na.rm = TRUE),
    avg_sp = mean(sp_500_vol, na.rm = TRUE)
    )
funds <- fund %>% group_by(dates = ceiling_date(dates, unit = "week") - 2) %>%
  summarise(
    avg_Baa = mean(Baa, na.rm = TRUE),
    avg_10yr = mean(yields_10yr, na.rm = TRUE),
    avg_funds = mean(eff_fund_rates, na.rm = TRUE),
    avg_future = mean(expected_fundsrate, na.rm = TRUE)
  )

dataset <- merge(funds, data_2)
dataset <- merge(dataset,debt)


dataset <- subset(dataset,"2000-12-31" < dates & dates <= "2019-12-27")
write_xlsx(dataset, "weekly_dataset.xlsx")

#wrangling bond supply
setwd("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets\\excel_sheets")
tr <- read_xlsx("Treasuries.xlsx")
attach(tr)

#Clean data and make dataset that has the supply of 10 year, 2 year and 3 month on a weekly level
#filter out the type of securities
tr <- filter(tr, `Security Class 1 Description` == "Bills Maturity Value" | 
                 `Security Class 1 Description` == "Notes",
                  Issue_Date >= "1999-12-31")
tr <- tr %>% 
  mutate(Issue_Date = as.Date(Issue_Date, "%Y-%m-%d"),
         Maturity_Date = as.Date(Maturity_Date, "%Y-%m-%d")) %>%
  arrange(Issue_Date)  

tr <- tr %>%
  mutate(
     weeks_ = round(as.numeric(difftime(Maturity_Date, Issue_Date, units = "weeks")), digits = 1),  
     ten_year = case_when((521.3 <= weeks_) & (weeks_ <= 521.9) ~ 1, FALSE ~ 0),
     two_year = case_when((103.7 < weeks_) & (weeks_ <= 104.4) ~ 1, FALSE ~ 0),
     three_mo = case_when((13.0 <= weeks_) & (weeks_ < 13.4) ~ 1, FALSE ~ 0)
  )
  
tr <- tr[ ,c(8,26:28)]
  
#Join monthly bond supply amounts 
setwd("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
md <- read_excel("monthly_dataset.xlsx")
tr <- read_excel("treasury_monthly.xlsx")
res <- read_excel("reserve_data.xlsx")
attach(md)
attach(tr)

tr <- filter(tr, tr$`Security Type Description` == "Marketable",
             tr$`Security Class Description` == "Bill" |
             tr$`Security Class Description` == "Note")
tr <- tr %>% mutate(dates = as.Date(`Record Date`))
tr <- tr[,c(16, 3, 4)]
 
bills <- filter(tr, tr$`Security Class Description` == "Bill")
notes <- filter(tr, tr$`Security Class Description` == "Note")

bills$bills_amt <- bills$`Securities Sold Count`
notes$notes_amt <- notes$`Securities Sold Count`

bills <- bills[, -c(2, 3)]
notes <- notes[, -c(2, 3)]

monthly_treasury <- left_join(notes, bills, by = "dates")
monthly_treasury <- monthly_treasury %>% 
  mutate(dates = as.Date(dates))
md <- md %>% mutate(dates = as.Date(dates))
res <- res %>% mutate(dates = as.Date(dates))
mtr <- merge(md, res, by = "dates", all = TRUE)
ro <- merge(md, bills, by = "dates", all = TRUE)

#merging prices and volumes for futures of 10 year notes
future <- read_excel("ten_year_futures.xlsx")
attach(future)

future$futures_vol <- ifelse(grepl("K", futures_vol), parse_number(futures_vol)*1000, parse_number(futures_vol)*10^6)

future <- future %>% group_by(dates = as.Date(dates, "%Y-%m-%d"))
future <- future %>% group_by(dates = ceiling_date(dates, unit = "weeks") - 2) %>%
  summarise(
    ten_year_futures = mean(ten_year_futures, na.rm = TRUE),
    futures_vol = mean(futures_vol, na.rm = TRUE)
  )

weekly <- read_excel("weekly_dataset.xlsx")
attach(weekly)
weekly$dates <- as.Date(weekly$dates)

weekl <- left_join(weekly, future, by = "dates") 
write_xlsx(weekl, "weekly_dataset.xlsx")


#merging daily prices with futures
daily <- read_excel("standard_dataset_tsz.xlsx")
data <- merge(daily, future, all = TRUE)

write_xlsx(data, "standard_tzs.xlsx")
summary <- as.data.frame(weekly[,c(2:4, 6, 12)])

stargazer::stargazer(summary, title = "Summary Statistics",
      covariate.labels = c("Baa Yields", "Treasury Yields", "Federal Funds Rate", "Russell Index", "Public Debt"),
      summary.stat = c("n", "min", "median", "max", "sd", "mean", "p25", "p75"), font.size = "small",out = "Summary.html",
      notes = c("Public Debt and Russell are adjusted for inflation using 2015 Core CPI and the rates are adjusted using the exact formula to by subtracting 1 from the quotient of the nominal and expected inflation "))

