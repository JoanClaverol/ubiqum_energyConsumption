# -------------------------------------------------------------------------
# GOAL: clean raw data to used on the modeling
# DESCRIPTION:
# DEVELOPER: Joan Claverol romero
# -------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, lubridate, magrittr)

# load data ---------------------------------------------------------------
# House data
df <- read_delim("data/raw_data/household_power_consumption.txt", 
                  delim = ";", 
                  col_types = cols(
                    Date = col_date(format = "%d/%m/%Y")
                  ))

# weather data


# data preparation --------------------------------------------------------

# functio to replace missing values
source("Scripts/preparation/functions/replace_missing_data.R")

# pipe line to apply all the data transformation 
df %<>% 
  # unite date and time columns and transform to time format (locale GMT)
  unite(col = date_time, Date, Time, sep = " ") %>% 
  mutate(date_time = parse_datetime(date_time, locale = locale(tz = "GMT"))) %>% 
  # apply the function to replace values 
  map_df(na_imputation) %>% 
  # rename variables
  rename(ActiveEnergy = Global_active_power, 
         ReactiveEnergy = Global_reactive_power, 
         Intensity = Global_intensity, 
         Kitchen = Sub_metering_1, 
         Laundry = Sub_metering_2, 
         W.A_HeatCold = Sub_metering_3) %>% 
  mutate(
    # Transforming active and reactve energy from KW to Whatt/h
    ActiveEnergy = ActiveEnergy*1000/60,
    ReactiveEnergy = ReactiveEnergy *1000/60,
    # New variable to represent all the information not covered by the three submetters
    UnkownEnergy = ActiveEnergy - (Kitchen + Laundry + W.A_HeatCold)
  ) %>% 
  # save it to the clean folder
  write_rds(path = "data/clean_data/prepared_data.rds")