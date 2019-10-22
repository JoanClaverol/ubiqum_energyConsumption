# -------------------------------------------------------------------------
# GOAL:
# DESCRIPTION:
# DEVELOPER:
# -------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(tidyverse)

# load data ---------------------------------------------------------------
data <- read_rds("data/clean_data/prepared_data.rds")


# pre process -------------------------------------------------------------

# function to get season 
source("Scripts/processing/function/getSeason.R")

# function to get hour period
source("Scripts/processing/function/getPeriod.R")

# pipe line pre process
data %>% 
  mutate(season = getSeason(date_time), 
         time_period = getPeriodTime(date_time)) %>% 
  write_rds("data/clean_data/processed_data.rds")
  
