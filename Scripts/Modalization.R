# Energy consumption modalization

# Libraries and data ----
require(pacman)
p_load(tidyverse, lubridate, forecast)

# Uploading data
df <- read_rds("Datasets/CleanTotalData.rds")
df.weather <- read_rds("Datasets/CleanWeatherData.rds")

# Prepare and analyse the data ----

# We are going to start the analysys from 2017-01-01, so let's filter it
data.month <- df %>%
  filter(year(DateTime) > 2006) %>% 
  mutate(year = year(DateTime), 
         month = month(DateTime)) %>%
  group_by(year, month) %>% 
  summarise(ActiveEnergy_avg = mean(ActiveEnergy), 
            Kitchen_avg = mean(Kitchen),
            Laundry_avg = mean(Laundry),
            AirWarm_avg = mean(W.A_HeatCold),
            Unknown_avg = mean(UnkownEnergy))

# time series creation ----

# creation by month
ts.month <- ts(data.month, start = c(2007), frequency = 12)

# visualization ----

# whole data visualzation
plot(ts.month[,c("ActiveEnergy_avg","Kitchen_avg",
                 "Laundry_avg","AirWarm_avg","Unknown_avg")], 
     main = "Average energy by month in relation to each month")

# Series decomposition ----

# decomposition total energy consumed by month 
plot(decompose(ts.month[,"ActiveEnergy_avg"], 
               type = c("additive")))

# decomposition Kitchen 
plot(decompose(ts.month[,"ActiveEnergy_avg"], 
               type = c("additive")))

# decomposition Laundry
plot(decompose(ts.month[,"Laundry_avg"], 
               type = c("additive")))

# decomposition AirWater heater
plot(decompose(ts.month[,"AirWarm_avg"],
               type = c("multiplicative"))) # !!! Its trend is increasing a lot, why? Applienc analysys

# decomposition unknown energy spending
plot(decompose(ts.month[,"Unknown_avg"],
               type = c("additive")))

# Applience analysis: train and test ---- 
train.ts <- window(ts.month[,"AirWarm_avg"], end = c(2009, 12))
test.ts <- window(ts.month[,"AirWarm_avg"], start = c(2010, 1))

# 1st forecasting ----

numberPeriods <- length(test.ts)
mod.meanForecast <- meanf(train.ts, h = numberPeriods)
mod.driftMethod <- rwf(train.ts, h = numberPeriods)
mod.seasonalNaive <- snaive(train.ts, h = numberPeriods)

# ploting the models
autoplot(train.mts[,"ActiveEnergy_avg"]) +
  autolayer(mod.seasonalNaive$mean, series = "Season naïve method") +
  autolayer(mod.meanForecast$mean, series = "Mean method") +
  autolayer(mod.driftMethod$mean, series = "Naïve method") +
  theme_bw() + ylab("Energy consumed in avg")

# checking the results
accuracy(mod.seasonalNaive, test.ts)
checkresiduals(mod.seasonalNaive)












# 2nd forecasting ----

# Applying HoltWinters
ts.holtWinters <- HoltWinters(ts.month, gamma = TRUE)

# Haremos la predicción a dos anyos vista con el método de holt winters
df.forecast.hw <- forecast(ts.holtWinters[,"ActiveEnergy_avg"], h = 24)
plot(df.forecast.hw, main = "Forecast from HoltWinters in months")

# Creación de modelos autorregresivo de media móvil. Con arima intentaremos crear un modelo que nos permita ajustar diferentes partes del mismo. Porque surt igual???
df.ts.arima <- auto.arima(df.ts)
df.forecast.arima <- forecast(df.ts.arima, h = 48)
plot(df.forecast.arima, main = "Forecast 2011: model Arima by month")

####    Forecasting methods   ####
# Linear model time series
df.ts.lm <- tslm(df.filter$Mean~season+trend,df.ts)
df.forecast.lm<- forecast(df.ts.lm,h=12)
autoplot(df.forecast.lm)
# Average method
plot(meanf(df.ts, h = 12))
# Na?ve method, forecasts the next values using the last values observed. The problem with whtis prediction is that is not taking into account the seasonality of the serie not the trend.
plot(naive(df.ts, h = 12))
# Seasonal na?ve method, it shows the seasonal pattern, whihc will take into account the values of the last cycle
df.forecast.naive <- snaive(df.ts, h = 12)
plot(df.forecast.naive)
# Drift method, which predictions will be the result of drawing a line between the first and the last line of the serie
plot(df.ts, h = 12, drift = TRUE) # it gives problems
# Holt winters method, which will combine the season na?ve and drift methods
plot(forecast(HoltWinters(df.ts), h = 12))

####    Forecasting vs 2010   ####
# Creation and preparation of the training and dataset
df.testing <- filter(.data = df, 
                     df$year >= 2010)
df.training <- filter(.data = df,
                      df$year >= 2007 & df$year < 2010)
df.training <- df.training %>% 
  group_by(year = year, month = month) %>%
  summarise(Mean = mean(Global_active_power))
df.testing <- df.testing %>% 
  group_by(year = year, month = month) %>%
  summarise(Mean = mean(Global_active_power))
# Creation of the time series
df.training.ts <- ts(df.training$Mean, start = c(2007), frequency = 12)
df.testing.ts <- ts(df.testing$Mean, start = c(2010), frequency = 12)
# Creation of a linaer model to predict
df.training.ts.lm <- tslm(df.training$Mean~season+trend,df.training.ts)
df.training.forecast.lm <- forecast(df.training.ts.ml, h = 12)
plot(df.training.forecast.model, main = "Forecast vs 2010: Linear model")
lines(df.testing.ts,col = "black", lwd = 2)
# Creation of an arima model to predict
df.training.ts.arima <- auto.arima(df.training.ts)
df.training.forecast.arima <- forecast(df.training.ts.arima, h = 12)
plot(df.training.forecast.model, main = "Mean by month forecast vs 2010: Arima model", cex.main = 2)
lines(df.testing.ts,col = "black", lwd = 2)
# Creation of an HoltWinters model to predict
df.training.ts.hw <- HoltWinters(df.training.ts)
df.training.forecast.hw <- forecast(df.training.ts.hw, h = 12)
plot(df.training.forecast.model, main = "Forecast active power_07/09 vs real active power 2010: HoltWinters model", cex.main = 2)
lines(df.testing.ts,col = "black", lwd = 2)

# Differences between the foreast with arima model and 2010
df.testing.results <- df.testing.ts - df.training.forecast.lm$mean
sum(df.testing.results)# the small result is for hw to predict 2010


# day month prediction ----
library(fpp2)
df_day <- df %>% 
  mutate(date = date(DateTime)) %>% 
  group_by(date) %>% 
  summarise(mean = mean(ActiveEnergy, na.rm = T))

ts_day <- ts(df_day$mean, 
             start = 2007,
             frequency = 365.25)

train <- df_day %>% 
  filter(date <= "2010-11-15")

test <- df_day %>% 
  filter(date > "2010-11-15")

ts_train <- ts(train$mean, start = c(2007,1), frequency = 365.25)
ts_test <- ts(test$mean, start = c(2010, 319), frequency = 365.25)

ts_train_red <- window(ts_train, start = c(2010, 365-60))

ggplot2::autoplot(ts_train_red) +  autolayer(ts_test) + theme_bw()

autoplot(ts_train)

autoplot(ts_day)
autoplot(decompose(ts_day))

mod.ts_day <- tslm(ts_day ~trend + season)

forecast(mod.ts_day, h = 4)

autoplot(mod.ts_day)

pred_day <- window(ts)