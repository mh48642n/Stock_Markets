rm(list = ls())
#Sets specific path to work directory, so set it based on your actual working directory
#important to do this part to get 
path <- ("C:\\Users\\marvi\\OneDrive\\Documents\\Github\\Stock_Markets")
setwd(path)

library(readxl)
library(writexl)
library(tidyverse)
library(lubridate)
library(lattice)
library(latticeExtra)
library(grid)

#autoplot(data_sets[], facets = TRUE) + xlab("Dates") +
#  ylab(" ") + ggtitle("Daily percent changes in yields and public debt")

#gridExtra::grid.arrange(futures_rates, actual_rates)

data <- read_excel("dataset_tsz.xlsx")
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

everything <- c("Russell 2000", "Russell Volume","S&P 500", "S&P Volume", "Close Price: Russell", "Close Price: S&P",
  "Num of Bills", "Num of Notes", "Public Held(in millions)", "Govt Held(in millions)", "Public Debt(in millions)", "Funds Rate", "Futures Funds Rate",
  "Meeting Dates", "Basis Points Changes", "CPI Report Release") 


#Prints out descriptives
stargazer::stargazer(as.data.frame(data[, c(2,17:18)]), title = "Descriptives",
                     covariate.labels = c("Russell 2000", "Public Debt", "Funds Rate"),summary = TRUE, summary.stat = c("n", "mean", "sd", "min", "max", "p25", "p75"), 
                     type = "html", out = "table.html")


sp <- xyplot(log(sp_500) ~ Public_Debt, data = data, 
       type = c("p", "r"), main = "Figure 3",
       xlab = "", ylab = "S&P Value",
       scales = list(txk = c(1, 0), x = list(cex = 0.8), y = list(cex = 0.8)),
       xlim = c(0, (1.0 * 10^7)), ylim = c(4.5, 8))

ru <- xyplot(log(russell_2000) ~ Public_Debt, data = data, 
       type = c("p", "r"), main = "Figure 4",
       xlab = "", ylab = "Russell Value",
       scales = list(txk = c(1, 0), x = list(cex = 0.8), y = list(cex = 0.8)),
       xlim = c(0, (1.0 * 10^7)), ylim = c(4.5, 8))

sp0 <- xyplot(sp_500 ~ Public_Debt, data = data, 
          type = c("p", "r"), main = "Figure 1",
          xlab = "", ylab = "S&P Value",
          scales = list(txk = c(1, 0), x = list(cex = 0.8), y = list(cex = 0.8)),
          xlim = c(0, (1.0 * 10^7)), ylim = c(0, 2500))

ru0 <- xyplot(russell_2000 ~ Public_Debt, data = data,
          type = c("p", "r"), main = "Figure 2",
          xlab = "", ylab = "Russell Value",
          scales = list(txk = c(1, 0), x = list(cex = 0.8), y = list(cex = 0.8)),
          xlim = c(0, (1.0 * 10^7)), ylim = c(0, 1200))

gridExtra::grid.arrange(sp0, ru0, sp, ru, ncol = 2,
              top = textGrob("Index Values and Public Debt", 
                    gp = gpar(fontsize = 15, font = 2)),
              bottom = textGrob(paste0("Public Debt is on the x axis to the millions and adjusted to inflation using the average CPI for 2015.",
                                " Both the Russell 2000 and S&P 500 \nwas adjusted for inflation the same way.",
                                " Public debt data was found from United States Treasury's Debt to the Penny dataset and \nthe index values acquired through Bloomberg.",
                                " Figures 3 and 4 has the log values of the stock indices with public debt as a regressor,\nfigures 1 and 2 have the unlogged values"),
                        x = 0,
                        just = "left",
                        gp = gpar(fontsize = 9, font = 1)))

