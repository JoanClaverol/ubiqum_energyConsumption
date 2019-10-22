# exercices to understand correlaiton

help("gold") #Daily morning gold prices in US dollars. 1 January 1985 – 31 March 1989.
help("woolyrnq") #Quarterly production of woollen yarn in Australia: tonnes. Mar 1965 – Sep 1994.
help("gas") #Australian monthly gas production: 1956–1995.

require(pacman)
p_load(tidyverse, fpp2)

tute1 <- read_csv("../../../../../Desktop/tute1.csv")

mytimeseries <- ts(tute1[,-1], start = 1981, frequency = 4)
autoplot(mytimeseries, facets = T, colour = mytimeseries) + theme_bw() 

retaildata <- readxl::read_excel("c:/Users/romcl/Desktop/retail.xlsx", skip = 1)
myts <- ts(retaildata[,"A3349873A"], frequency = 12, start = c(1982, 4))
autoplot(myts) # seasonality and positive trend
ggseasonplot(myts, polar = T) 
ggsubseriesplot(myts) # clear seasonality effect, winter higher values(only december and november)
gglagplot(myts)
ggAcf(myts, lag.max = 381) # high correlation with the previous values

ts <- writing
autoplot(ts) # seasonality and positive trend
ggseasonplot(ts, polar = F) 
ggsubseriesplot(ts) # clear seasonality effect, winter higher values(only december and november)
gglagplot(ts)
ggAcf(ts, lag.max = 9*9*9*9*9) # high correlation with the previous values

autoplot(diff(goog))
