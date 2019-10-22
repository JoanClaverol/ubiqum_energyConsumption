# -------------------------------------------------------------------------
# GOAL: create a model to predict the energy consumption 
# DESCRIPTION: we want to create a model to predict the energy consumption
# by day. 
# -------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, lubridate, fastDummies, modelr, magrittr, caret)

# load data ---------------------------------------------------------------
data <- read_rds("data/clean_data/processed_data.rds")


# feature creation --------------------------------------------------------

model_data <- data %>%
  # group the information by date, season 
  group_by(season, date = date(date_time)) %>% # time_period
  summarise_at(vars(ends_with("Energy"), Kitchen, Laundry, W.A_HeatCold), 
               mean) %>% 
  # create new features: week day, week number and year
  mutate(
    week_day = wday(date, label = T, abbr = F),
    week_n = week(date),
    month = month(date, label = T, abbr = F),
    year = year(date)
  ) %>%
  # create dummy variables 
  dummy_cols(select_columns = c("season","week_day","month","week_n")) %>%
  # extract dummy variables that cause coliniearity
  select(-week_n_1, -week_day_Monday) %>% 
  write_rds("data/clean_data/model_data.rds")


# modalisation ------------------------------------------------------------

# data before 2010 will be used as a training, and the rest would be testing
train <- model_data %>% 
  filter(year < 2010)
  
test <- model_data %>% 
  filter(year >= 2010)

# create a linear model based on the features done before
fit <- lm(
  formula = ActiveEnergy ~ ., 
  data = train %>% select(ActiveEnergy,
                          starts_with("week_day_"), 
                          starts_with("week_n_"), 
                          year))

# save the model 
write_rds(fit, path = "data/clean_data/lm_fit.rds")



# check the predictions
model_data %>% 
  add_predictions(model = fit, var = "pred") %>% 
  add_residuals(model = fit, var = "resid") %>%
  filter(year == 2010 | year == 2009) %>%
  ggplot(aes(x = date)) +
    geom_line(aes(y = ActiveEnergy)) +
    geom_line(aes(y = pred), color = "red")
    # geom_ref_line(h = 0) +
    # geom_line(aes(y = resid))


