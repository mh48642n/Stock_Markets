rm(list = ls())
setwd("C:/Users/marvi/OneDrive/Documents/Github/Stock_Markets/excel_sheets")
getwd()

library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)

#data_set <- read_excel("full_dataset.xlsx")
#attach(data_set)
#as.data.frame(data_set)

#names(data_set)[9] <- "treasuries_10year"

#str(data_set)


#data_set <- data_set %>%
#  mutate(
#    Aaa = as.numeric(Aaa),
#    Baa = as.numeric(Baa),
#    DEXUSEU = round(as.numeric(DEXUSEU), digits = 2)
#  )

x <- unlist(excel_sheets("DFF.xlsx"))
print(excel_sheets("DFF.xlsx"))

answer <- function(x){
  sheet_s <- readline("Sheet that you want to add: ")
  
  if(sheet_s %in% x){
    cat("Sheet does exist")
  }
  else{
    cat("Sheet does not exist")
  }
}

if(interactive())answer(x)


cat("Say hi")
answer <- readline("stdin", n = 1)
print(answer)

# NOT RUN {
fun <- function() {
  ANSWER <- readline("Are you a satisfied R user? ")
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
  if (substr(ANSWER, 1, 1) == "n")
    cat("This is impossible.  YOU LIED!\n")
  else
    cat("I knew it.\n")
}
if(interactive()) fun()
# }

answer <- readline("Say hu")

