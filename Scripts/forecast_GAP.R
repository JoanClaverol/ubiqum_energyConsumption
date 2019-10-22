# GOAL: regression GAP energy consumption
# DEFINITION: create and analyse the prediction of energy consumption to regression algorithms
# AUTHOR: Joan Claverol 


# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, lubridate, fastDummies, magrittr, modelr, caret)

# load data ---------------------------------------------------------------

df <- read_rds("Datasets/CleanTotalData.rds")
# df_temp <- read_rds("Datasets/CleanWeatherData.rds")

# 1st pre-process ---------------------------------------

# selecting relevant variables 
df_frcst <- df %>%
  select(DateTime,ActiveEnergy,season,wday,Period.Day)

# edefining main predictors
df_frcst %<>% 
  mutate(second = second(DateTime), minute = minute(DateTime), 
         hour = hour(DateTime), week = week(DateTime), 
         month = month(DateTime), year = year(DateTime))
  
# filtering 2007
df_frcst_2007 <- df_frcst %>% 
  filter(year == 2007) %>% 
  group_by(date = date(DateTime), Period.Day, wday, season) %>% 
  summarise(avg_energy = mean(ActiveEnergy))

# visualizing the consumption and hypothesis creation, week days and months 
# really affects the data
df_frcst_2007 %>% 
  ggplot(aes(x = date, y = avg_energy)) + 
    geom_point(aes(color = season)) +
    geom_smooth(se = F) +
    facet_wrap(~Period.Day)
    

# let's visualize the distribtuion of energy in relation to the week day
df_frcst_2007 %>% 
  ggplot(aes(x = wday, y = avg_energy)) +
    geom_boxplot(aes(color = season)) + facet_wrap(~Period.Day)
# sunday is the day with more consumption + other insights


# 1st dummify variables -------------------------------------------------------

cat_var <- c("Period.Day","wday","season")
df_frcst_2007[,cat_var] <- apply(df_frcst_2007[,cat_var], 2, as.factor)
df_frcst_2007 <- df_frcst_2007 %>% 
  dummy_cols() %>% 
  select(-Period.Day, -wday, -season)

# 1st modeling with lm --------------------------------------------------------

# creating the linear model 
mod_lm <- lm(avg_energy ~ ., 
             data = temp)

# visualizing the errors
grid <- df_frcst_2007 %>% 
  data_grid(df_frcst_2007) %>% 
  add_predictions(model = mod_lm, var = "avg_energy")

df_frcst_2007 <- df_frcst_2007 %>% 
  add_residuals(model = mod_lm, var = "resid_mod1")

df_frcst_2007 %>% 
  ggplot(aes(x = date, y = resid_mod1)) +
    geom_ref_line(h = 0) +
    geom_line()
# In summer and at the end of the sprint we have better predictions

# Problems defining the outliers in relation to the season
# df_frcst_2007 %>%
# df_frcst_2007 %>%   
#   ggplot(aes(wday, avg_energy)) +
#     geom_boxplot() +
#     geom_point(data = grid, color = "red") +
#     facet_wrap(~season)

# notes to improve my model: 
# add temperature
# add more data
# goal of the task is to do the best predictions in relation to the date





# 2nd pre-process ---------------------------------------------------------

# Hyp: the main factors affecting the consume of energy is summer and winter, 
# day of the week, number of the week

# adding week number and selecting the variables we want to use to predict
df_frcst <- df %>% 
  mutate(week_n = week(DateTime), year = year(DateTime)) %>% 
  group_by(date = date(DateTime), wday, week_n) %>% 
  summarise(AE_avg = mean(ActiveEnergy))

# # creating winter and summer season 
# getSeason_W_S <- function(date) {
#   WS <- as.Date("2008-12-21", format = "%Y-%m-%d") # Winter Solstice
#   # SE <- as.Date("2008-3-20",  format = "%Y-%m-%d") # Spring Equinox
#   SS <- as.Date("2008-6-21",  format = "%Y-%m-%d") # Summer Solstice
#   # FE <- as.Date("2008-9-22",  format = "%Y-%m-%d") # Fall Equinox
#   
#   # Convert dates from any year to 2012 dates
#   d <- as.Date(strftime(date, format="2008-%m-%d"))
#   
#   ifelse (d >= WS | d < SS, "Winter","Summer")
# }
# 
# df_frcst$season_W_S <- getSeason_W_S(df_frcst$date)

# creating dummy variables 
cat_var <- c("wday","week_n") # ,"season_W_S"

df_frcst[,cat_var] <- lapply(df_frcst[,cat_var], as.factor) 

df_frcst %<>% 
  dummy_columns(select_columns = cat_var) %>% 
  select(-wday, - week_n) # , -season_W_S


# 2nd modalization --------------------------------------------------------

# creating a train and test
train <- df_frcst %>%
  filter(year(date) != 2010)
test <- df_frcst %>% 
  filter(year(date) == 2010)

# using caret to predict
mod2_caret <- caret::train(form = AE_avg ~ ., 
                           data = train %>% select(-date),
                           method = "lm")

# ploting the model in train
test %>% 
  add_predictions(model = mod2_caret, var = "pred") %>% 
  ggplot(aes(x = date)) +
    geom_smooth(aes(y = AE_avg), color = "blue") +
    geom_smooth(aes(y = pred), color = "red") +
    geom_smooth(data = train, aes(y = AE_avg))

# 2nd check performance ---------------------------------------------------

results_mod2 <- predict(object = mod2_caret, newdata = test)
postResample(pred = results_mod2, obs = test$AE_avg)


# 3rd pre process ---------------------------------------------------------

# adding temperature 
df_weather <- read_rds(path = "Datasets/CleanWeatherData.rds")

df_frcst <- df %>%
  mutate(week_n = week(DateTime), year = year(DateTime)) %>% 
  group_by(date = date(DateTime), wday, week_n) %>% 
  summarise(AE_avg = mean(W.A_HeatCold)) %>% 
  left_join(y = df_weather %>% select(date, Avg.Temp), 
            by = "date")

# dummy variables
cat_var <- c("wday") # ,"season_W_S","week_n"

df_frcst %<>% 
  dummy_columns(select_columns = cat_var) %>% 
  select(-wday, -week_n) 


# 3rd model ---------------------------------------------------------------

# create test and train
train <- df_frcst %>%
  filter(year(date) != 2010)
test <- df_frcst %>% 
  filter(year(date) == 2010)


# running the model 
mod3 <- caret::train(AE_avg ~., 
                     data = train %>% select(-date),
                     method = "rf")

results_mod3 <- predict(object = mod3, newdata = test)
postResample(pred = results_mod3, obs = test$AE_avg)
# using only avg temperature as a predictor is not useful. 
# we also tried to use max.temp and min.temp but the resultst where not
# succesful


# 4rth pre process --------------------------------------------------------

# use of ts models to see is I can get the same performance than in the 
# second pre-process 
