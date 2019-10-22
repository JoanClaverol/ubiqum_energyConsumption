# Load, treat and creation of the energy time series object

# Libraries ----
require(pacman)
p_load(tidyverse, mice, lubridate)

# Uploading data ----

# House data
df1 <- read_delim("Datasets/household_power_consumption.txt", delim = ";", 
                  col_types = cols(
                    Date = col_date(format = "%d/%m/%Y")
                    )
                  )

# Weather region data
weather <- read_csv("Datasets/weather_conditions.csv") # downloaded for the webpage https://www.ncdc.noaa.gov

# weather dataset exploration ----

summary(weather)
mice::md.pattern(weather)

# Discard columns we are not interested from the dataset of the weather
weather <- weather %>% 
  select(DATE, TAVG, TMAX, TMIN) %>% rename(date = DATE, Avg.Temp = TAVG, Max.Temp = TMAX, Min.Temp = TMIN)

# Dealing with NAs
weather <- weather %>%
  dplyr::mutate(Avg.Temp = if_else(is.na(Avg.Temp),(Max.Temp - Min.Temp)/2 + Min.Temp,Avg.Temp))

columnMean <- function(x,y){
  
}

# Dealing with time ----

# Looking for problems in Daylight saving time (changing time)
timeChange.Summer <- "2007-03-25"
timeChange.Winter <- "2007-10-28"

x <- df1 %>% filter((Date == timeChange.Summer | Date == timeChange.Winter) & between(hour(Time), 2,3))
# there seems there are no problems

# Unification of the information and changing the format
df1 <- df1 %>% unite(col = DateTime, Date, Time, sep = " ") 
df1$DateTime <- parse_datetime(df1$DateTime, locale = locale(tz = "GMT"))

# Treating NAs ----

# There are NAs in the dataset? 
anyNA(df1) # YES

# Where are they placed?
#NAnalysis <- md.pattern(df1)
#write_rds(NAnalysis, "Scripts/Electricity_consumption/Information/NAnalysis.rds")
read_rds("Scripts/Information/NAnalysis.rds")
# All the NAs are in the same rows. No information for submeters and any other energy indicator

# Which strategy I have to follow to deal with NAs? Distribution of NAs across the time
all.NAs <- df1  %>% 
  filter(is.na(Global_active_power)) 
all.NAs %>% 
  mutate(date = date(DateTime)) %>% 
  ggplot(aes(date)) + geom_bar(fill = "red") + 
  theme_light() +
  labs(title = "Quantity of NAs in relation to the day of the year")

# Goal: create a function to replace the NA for the previous value
na_imputation <- function(vector) {
  for (i in 1:length(vector)) {
    if (is.na(vector[i])) {
      vector[i] <- vector[i-10080] # replacinmg NAs with 1 week previous values
    }
  }
  return(vector)
}

energy_var <- c("Global_active_power","Global_reactive_power",
                "Voltage","Global_intensity","Sub_metering_1",
                "Sub_metering_2","Sub_metering_3")
df1[,energy_var] <- apply(df1[,energy_var], 2, na_imputation)


# Rename and select variables ----

df1 <- df1 %>% dplyr::select(-Voltage) %>% 
  dplyr::rename(ActiveEnergy=Global_active_power, ReactiveEnergy=Global_reactive_power, 
         Intensity=Global_intensity, Kitchen=Sub_metering_1, Laundry=Sub_metering_2, W.A_HeatCold=Sub_metering_3)

# Transforming data from KW to Whatt/h ----

df1 <- df1 %>% mutate (ActiveEnergy= ActiveEnergy*1000/60,
                       ReactiveEnergy=ReactiveEnergy *1000/60)

# Dealing with negative ----

# Looking for negative values
df1 %>% filter_all(all_vars(. < 0)) # any value

# New variable creation ----

# Variable related to all the information not covered by the three submetters
df1 <- df1 %>% 
  mutate(UnkownEnergy = ActiveEnergy - (Kitchen + Laundry + W.A_HeatCold))

# Season of the year
getSeason <- function(date) {
  WS <- as.Date("2008-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2008-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2008-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2008-9-22",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(date, format="2008-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

df1$season <- getSeason(df1$DateTime)

# Day of the week
df1 <- df1 %>% 
  mutate(wday = wday(DateTime, label = T, abbr = F, locale = "us", week_start = 1))

# Moment of the day
getPeriodTime <- function(time) {
  time <- hour(time)
  ifelse (time >= 0 & time < 6, "Night",
          ifelse (time >= 6 & time < 12, "Morning",
                  ifelse(time >= 12 & time < 18, "Midday","Evening")))
}

df1$Period.Day <- getPeriodTime(df1$DateTime)



# Filter the data (start 1 jan 2007) ----
# We are going to start the analysys from 2017-01-01
df1 <- df1 %>% 
  filter(date(DateTime) >= "2007-01-01")

# Create df.weather ----

# Merging the two datasets
x <- c("ActiveEnergy","ReactiveEnergy","Intensity","Kitchen","Laundry","W.A_HeatCold","UnkownEnergy")

df.weather <- df1 %>%
  mutate(date = date(DateTime)) %>% group_by(date) %>% select(date, x) %>%
  summarise_all(mean) %>% full_join(weather, by = "date") %>%
  mutate(wday = wday(date, label = T, abbr = F, locale = "us", week_start = 1))

# Saving the file ----

write_rds(df1,"Datasets/CleanTotalData.rds")
write_rds(df.weather, "Datasets/CleanWeatherData.rds")

# rm(list = ls())

