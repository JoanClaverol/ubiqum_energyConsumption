# Exploration of the energy consumption modified


# Libraries and data ----

require(pacman)
p_load(fpp2, tidyverse, lubridate, plotly, beanplot, tibbletime)

df2 <- as_tbl_time(read_rds("Datasets/CleanTotalData.rds"), index = DateTime)
df2_weather <- read_rds("Datasets/CleanWeatherData.rds")



# Seasons effect ----

df2 %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(ActiveEnergy)) %>%
  ggplot(aes(`date(DateTime)`, mean)) + 
    geom_line(color = "firebrick1") + 
    geom_smooth(se = F) + 
    labs(title = "Mean active energy consumed by day") + 
    ylab("Watt/h") + xlab("Time") + theme_light() -> g1
ggplotly(g1)
# Yes, there is a seasonality on the data, so the season has an influence in the energy consumption: In winter there is a high consumption and in summer there is less consumption. Important to consider that the season in our model

# 2nd way to show the results
df2 %>% 
  mutate(month = month(DateTime, label = T, locale = "us"), 
         year = year(DateTime)) %>% 
  group_by(year, month) %>% 
  summarise(mean_A.E. = mean(ActiveEnergy)) -> x

ts <- ts(x$mean_A.E., start = 2007, frequency = 12)
ggseasonplot(ts, polar = T) + 
  theme_bw() + 
  labs(title = "Mean energy consumption/month") + 
  ylab("Mean active energy Watts/h") 

# 3rd way to show your results
ggsubseriesplot(ts) + theme_bw()
# the blue line talks about the mean and shows clearly the differences between seasons, and it shows the changes in seasonality among time

# Week day effect ----

df2 %>% 
  mutate(wday = wday(DateTime, label = T, abbr = F, 
                     locale = "us", week_start = 1), 
         hour.minutes = paste(hour(DateTime), 
                              minute(DateTime), 
                              second(DateTime), sep = ":")) %>% 
  group_by(wday, hour.minutes) %>% 
  summarise(median = round(median(ActiveEnergy),2)) -> week.general

week.general$hour.minutes <- as.POSIXct(strptime(week.general$hour.minutes, 
                                                 format = "%H:%M:%S", tz = "GMT"), 
                                        format = "%H:%M:%S")

week.general %>% 
  ggplot(aes(hour.minutes, median)) + 
  geom_line() + facet_wrap(wday~.) + 
  labs(title = "Median active energy/week") + 
  ylab("Median Watt/h") + xlab("Time") + 
  theme_light() + scale_x_datetime(date_labels = "%H:%M") -> g2
ggplotly(g2)
# Yes, there is diffent energy consumption in relation to the day of the week: 
#   Monday to Friday they follow similar patterns
#   Saturday the most different day of the whol week
#   Sunday ALso different to the resto of the week, but has similitudes
# And we can also see that there are different consumptions in the morning, miday and evening. 

# Understanding the distribution of each wday for the whole year
week.general %>% 
  ggplot(aes(wday, median)) + 
  geom_boxplot() # No outliers

beanplot(week.general$median ~ week.general$wday, col = c("blue", "red", "yellow"))
# Great graph to see the distribution of the data en relation to each day


# datacollected by submetters ----

# Are the submeters collecting all the possible information
x <- c("Kitchen","Laundry","W.A_HeatCold","UnkownEnergy")

df2 %>% 
  select(season, x) %>% 
  gather(x, key = submeters, value = energy_consumption) %>% 
  group_by(season, submeters) %>% 
  summarise(energy_consumption = sum(energy_consumption)) -> submPerc

brks <- c(0, 0.25, 0.5, 0.75, 1)
submPerc %>%
  ggplot(aes(season, energy_consumption)) + 
    geom_col(aes(fill = submeters), position = "fill") +
    labs(title = "Energy consumption by season") +
    scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
    ylab("% submters") + xlab("Season") + theme_minimal() -> g3
g3

# temperature effect ----

# How is the relation between the avg temperature and teh active energy consumed
df2_weather %>% 
  ggplot(aes(x = date)) +
    geom_line(aes(y = ActiveEnergy), col = "red", alpha = 0.4) + 
    geom_smooth(aes(y = ActiveEnergy), col = "red", se = F) +
    geom_line(aes(y = Avg.Temp), col = "blue", alpha = 0.4) + 
    geom_smooth(aes(y = Avg.Temp), col = "blue", se = F) +
    scale_y_continuous("Active energy Watts/h",
                     sec.axis = sec_axis(trans = ~.*0.9, breaks = c(-10,0,10,20,30),
                                         name = "Avg. temperature in ºC")) + 
  theme_classic() + 
  labs(title = "Energy consumed avg. vs temperature avg. from 2007 to 2011", 
       subtitle = "red = average active energy in watts\nblue = average temperature in ºC")
  
# other ways to see the relation
ts <- ts(df2_weather[c("ActiveEnergy","Avg.Temp")], start = 2007, frequency = 365.25)
autoplot(ts[,c("ActiveEnergy","Avg.Temp")], facets = T) + 
  ylab("") + labs(title = "Relation btwn energy consumptions and temperature")
# there seems there is a high negative correlation between temeprature and energy consumption



# Relation between active energy consumption and temperature
df2_weather %>% 
  ggplot(aes(ActiveEnergy, Avg.Temp)) + 
  geom_point() + 
  geom_smooth() -> g4
ggplotly(g4)


# Correlation btwn var ----

# all the information
plot(df2_weather) 

# Correlation between submetters
cor(df2_weather[c("ActiveEnergy","Kitchen","Laundry","W.A_HeatCold")],use="complete.obs", method="pearson")

# relation between variables
df2_weather %>% select(ActiveEnergy,Kitchen,Laundry,W.A_HeatCold) %>% 
  gather("Kitchen","Laundry"," ", key = "submetters", value = "value") %>% 
  ggplot(aes(x = value, y = ActiveEnergy, color = submetters))+
    geom_point() + geom_smooth(se = F) + 
  labs(title = "Correlation submetters vs active energy", 
       subtitle = "Laundry correlation = 0.49\nKitchen correlation = 0.56\nW.A_HeatCold correlation = 0.76") +
  xlab("Submetters value in Watts/h") + ylab("Global active energy in Watts/h") + theme_bw()-> g5
ggplotly(g5)

# ploting potential predictors

ts <- ts(df2_weather[c("ActiveEnergy","UnkownEnergy","W.A_HeatCold","Kitchen","Laundry")], start = 2007, frequency = 365.25)
autoplot(ts[,1:5], facets = T)
GGally::ggpairs(as.data.frame(ts[,1:5]))

# Autocorrelation plots
# it helps us to understand how lagged values affect in a time series
ts <- ts(df2_weather, start = 2007, frequency = 365.25)
ggAcf(ts[,c("ActiveEnergy")], lag.max = 365.25*4)
# There is a trend which is showing a decreasment of the ActiveEnergy correlation, it shows us there is a trend which is normalizing the energy consumption



ts <- ts(df2, start = 2007, frequency = 365.25*24*60)
ggAcf(ts[,c("ActiveEnergy")])