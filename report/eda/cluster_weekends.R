library(tidyverse)
library(lubridate)
library(scales)

# load data
df <- read_rds("data/clean_data/CleanTotalData.rds")

# function to normalize man an max
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}

# create the df by day and information by hour
df_hour <- df %>% 
  select(DateTime, ActiveEnergy) %>% 
  group_by(date = date(DateTime), hour = hour(DateTime)) %>% 
  summarise(sum = sum(ActiveEnergy)) %>%
  ungroup() %>% 
  group_by(date) %>% 
  mutate(sum_scales = normalit(sum)) %>% 
  ungroup() %>%
  select(-sum) %>% 
  pivot_wider(names_from = hour, values_from = sum_scales) %>%
  drop_na()

# apply kmeans model
cl <- kmeans(df_hour %>% select(-date), centers = 2)
df_hour$cluster <- cl$cluster
df_hour$wday <- wday(df_hour$date, label = T, abbr = F, week_start = 1)

# check the performance of kmeans
df_hour %>% 
  group_by(wday, cluster) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(wday) %>% 
  mutate(perc = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wday, y = perc, fill = factor(cluster))) + 
    geom_col(position = "dodge") + 
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    annotate(geom = "rect", xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 1, 
             alpha = 0.2) +
    annotate("text", y = 0.95, x = 5.95, label = "Weekends", size = 6) + 
    scale_y_continuous(labels = percent_format(accuracy = 1), 
                       breaks = seq(0,1,0.1), 
                       limits = c(0, 1)) +
    scale_fill_manual(values = c("dodgerblue4", "#6FB8FF")) +
    labs(title = "Percentage of week days by cluster",
         subtitle = "Results applying kmeans technique to find groups in the data", 
         fill = "Cluster:") +
    theme_void() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.y = element_blank(), 
      axis.text.y = element_text(face = "bold", size = 10),
      axis.title.x = element_blank(),
      axis.text.x = element_text(), 
      axis.line.x = element_blank(),
      legend.position = "top",
      plot.margin = unit(c(1, 1, 1, 1), "lines")
    ) 

