#### Script description ####


# libraries ----
require(pacman)
p_load(tidyverse, lubridate, forecast)

# import data ----
data <- read_rds("Datasets/CleanTotalData.rds")

# 

# creating the time series object ----
ts <- ts(data[,"W.A_HeatCold"], start = c(2007), frequency = 60*24*365.25)




