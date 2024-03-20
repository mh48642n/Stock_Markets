rm(list = ls())
setwd("C:/Users/marvi/OneDrive/Documents/Github/Stock_Markets/")
getwd()

library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(stargazer)

#import dataset and makes it a dataframe
data_set <- read_excel("full_data_set.xlsx")
attach(data_set)
as.data.frame(data_set)

names(data_set)[9] <- "t_note_10yr"

#adjusting dollar values 
#----Public Debt, Held and Govt Held are adjusted to billions
#----Monthly Budget surplus or deficit are adjusted to billions
data_st <- data_set %>%
  mutate(
    Public_Debt = Public_Debt / (10^9),
    Public_Held = Public_Held / (10^9),
    Govt_Held = Govt_Held / (10^9),
    budget = (budget / 1000)
  )

#Making certain variables numeric and rounding off
data_st <- data_st %>%
  mutate(
    dates = as.Date(dates),
    Aaa = as.numeric(Aaa),
    Baa = as.numeric(Baa),
    DEXUSEU = round(as.numeric(DEXUSEU), digits = 4),
    t_note_10yr = as.numeric(t_note_10yr),
    core_cpi = as.numeric(core_cpi),
    
  )

#--when adjusting prices for inflation I adhere to the formula
#----Value in X dollars = Value in Y dollars * (CPI of X dollars / CPI of Y dollars) 
 
#adjustment to the base cpi for December 2015
base_cpi <- 244.955

#creating adjusting columns for the formula 
#----adjust divides the avg cpi in 2015 by the core cpi for that month
#----rate_adj finds the inflation rate with the core cpi
data_st$adjust_  <- round(base_cpi/(data_st$core_cpi), 3)
data_st$rate_adj <- ifelse(dates < "2015-12-31", round(((base_cpi/core_cpi)-1), 3), 
                           round(((core_cpi/base_cpi)-1), 3))

#creates column to help with conditional statements
#and fill the adjustment factors column 
data_st <- data_st %>%
  mutate(
    Month_Yr  = dates
  )
data_st <- data_st %>% 
  fill(adjust_, .direction = "up") %>%
  fill(rate_adj,.direction = "up")

#adjusts all dollar values to 2015 dollars
#----multiplies dollar values by the adjustment factor
data_st <- data_st %>%
  mutate(
  sp_500 = if_else(data_st$Month_Yr == dates, sp_500*data_st$adjust_, sp_500),
  russell_2000 = if_else(data_st$Month_Yr == dates, russell_2000*data_st$adjust_, russell_2000),
  public_held = if_else(data_st$Month_Yr == dates, Public_Held*data_st$adjust_, Public_Held),
  govt_held = if_else(data_st$Month_Yr == dates, Govt_Held*data_st$adjust_, Govt_Held),
  public_debt = if_else(data_st$Month_Yr == dates, Public_Debt*data_st$adjust_, Public_Debt),
  real_gdp = if_else(data_st$Month_Yr == dates, Nominal_GDP_Index*data_st$adjust_, Nominal_GDP_Index),
  M2 = if_else(data_st$Month_Yr == dates, M2*data_st$adjust_, M2),
  budget = if_else(data_st$Month_Yr == dates, budget*data_st$adjust_, budget)
)

#adjusts all interest rates to 2015 inflation
#Uses Fisher's exact formula for inflated or deflated rates
#----Where r = ((1 + nominal rate) / (1 + inflation rate)) - 1
data_st <- data_st %>%
  mutate(
    t_note_10yr = ((1 + t_note_10yr)/(1 + rate_adj) - 1),
    Aaa = ((1 + Aaa)/(1 + rate_adj) - 1),
    Baa = ((1 + Baa)/(1 + rate_adj) - 1),
    eff_fund_rates = ((1 + eff_fund_rates) / (1 + rate_adj) - 1),
    euro_us_ex = ((1 + DEXUSEU) / (1 + rate_adj) - 1)
    
  )

#subsetting dataset
data_st <- subset(data_st, select = c(1,2,4,13:14,9,11:12,40,38,36,37,17:18,23:24,39,20:22,25,31:32))
data_st <- subset(data_st, dates < "2023-06-09")
 
data_st <- data_st %>%
  mutate(
    Month_Yr  = dates
  )

str(data_st)

write_xlsx(data_st, "~/Github/Stock_Markets/real_full_dataset.xlsx")

data_st <- read_excel(file.choose())
attach(data_st)
#monthly averages for continuous data
dummies <- data_st[,c(24, 18:23)]
data_st <- data_st[,1:17]

data_st <- data_st %>% group_by(months_time = (ceiling_date(dates, unit = "month") - 1)) %>%
  summarize(
    sp_500 = mean(sp_500, na.rm = TRUE),
    russell_2000 = mean(russell_2000, na.rm = TRUE),
    eff_fund_rates = mean(eff_fund_rates, na.rm = TRUE),
    futures_10year = mean(futures_10year, na.rm = TRUE),
    t_note_10yr = mean(t_note_10yr, na.rm = TRUE),
    Aaa = mean(Aaa, na.rm = TRUE),
    Baa = mean(Baa, na.rm = TRUE),
    euro_us_ex = mean(euro_us_ex, na.rm = TRUE),
    public_debt = mean(public_debt, na.rm = TRUE),
    public_held = mean(public_held, na.rm = TRUE),
    govt_held = mean(govt_held, na.rm = TRUE),
    M2 = mean(M2, na.rm = TRUE),
    budget = mean(budget, na.rm = TRUE),
    inflation_expectations_1yr = mean(inflation_expectations_1yr, na.rm = TRUE),
    inflation_expectations_10 = mean(inflation_expectations_10, na.rm = TRUE),
    real_gdp = mean(real_gdp, na.rm = TRUE)
  )

dummies[dummies == 0] <- NA
dummies <- dummies %>% group_by(months_time = (ceiling_date(Month_Yr, unit = "month") - 1))

#splitting up dummy variable data set to make it easier to merge
raised <- dummies[, c(8,6)]
raised <- raised[complete.cases(raised), ]

suspend <- dummies[, c(8,7)]
suspend <- suspend[complete.cases(suspend), ]

auction_day <- dummies[, c(8,5)]
auction_day <- auction_day[complete.cases(auction_day), ]

meeting <- dummies[, c(8, 3)]
meeting <- meeting[complete.cases(meeting), ]

# merging dummy variable dataframes
data_st <- merge(data_st, raised, all = TRUE)
data_st <- merge(data_st, suspend, all = TRUE)
data_st <- merge(data_st, auction_day, all = TRUE)
data_st <- merge(data_st, meeting, all = TRUE)

#inserts 0 for NA's in the columns that are binary
data_st$raised <- ifelse(is.na(data_st$raised), 0, 1)
data_st$auction_day <- ifelse(is.na(data_st$auction_day), 0, 1)
data_st$suspend <- ifelse(is.na(data_st$suspend), 0, 1)
data_st$meeting <-  ifelse(is.na(data_st$meeting), 0, 1)

write_xlsx(data_st, "C:/Users/marvi/OneDrive/Documents/GitHub/Stock_Markets/dataset_final.xlsx")


data <- read_excel("C:/Users/marvi/OneDrive/Documents/GitHub/Stock_Markets/dataset_final.xlsx")
attach(data)
data <- as.data.frame(data)

stargazer(data[c(2:10, 14:16)], type = "html", digits = 2, column.sep.width = "4pt",
          covariate.labels = c("S&P 500", "Russell 2000", "Federal Funds Rate", "Future Contracts",
                               "10 Year Note Yields", "Corporate Aaa Bond Yields", 
                               "Corporate Baa Bond Yields", "Euro/US Spot Exchange Rate", 
                               "Monthly Public Debt Average","Monthly National Surplus/Deficit",
                               "1-yr Inflation Expectations", "10-yr Inflation Expectations"),
          title = "Table 1: Summary Statistics",
          summary.stat = c("mean","sd","median","min", "max"), out = "pb_summary.html")


