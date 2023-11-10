rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
#important to do this part to get 
path <- ("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
setwd(path)

library(writexl)
library(readxl)
library(tidyverse)
library(ggthemes)
library(ggpubr)

#autoplot(data_sets[], facets = TRUE) + xlab("Dates") +
#  ylab(" ") + ggtitle("Daily percent changes in yields and public debt")

#gridExtra::grid.arrange(futures_rates, actual_rates)

data <- read_excel("dataset.xlsx")
attach(data)

actual <- ggplot(data, aes(dates, eff_fund_rates)) + 
  geom_line(aes(color = "orange")) + 
  labs(
    x = "Date",
    y = "Rate"
  ) + theme(panel.background = element_rect("lightgray")) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())

expected <- ggplot(data, aes(dates, future_funds_rate)) + 
  geom_line(aes(color = "blue")) + 
  labs(
    x = "Date",
    y = "Rate"
  ) + theme(panel.background = element_rect("lightgray")) + 
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())

plot <- ggarrange(actual, expected, nrow = 1, common.legend = TRUE, legend = "bottom")
annotate_figure(plot, top = text_grob("Federal Funds Rate",face = "bold", size = 14))    
