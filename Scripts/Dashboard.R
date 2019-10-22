# application for the final usar in energy consumption task

# libraries and data ----

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
p_load(shiny, shinydashboard, dplyr, ggplot2, plotly, lubridate)

# Loading data

data <- readr::read_rds("Datasets/CleanTotalData.rds")

# Shiny application ----

ui <- dashboardPage(
  skin = "green",
  
  #Header
  dashboardHeader(
    title = "Electricty consumption task"
  ),
  # Sidebar
  dashboardSidebar(
    menuItem(text = "Seasons and week consumption", label = "totalEnergy"),
    menuItem(text = "Predictions", label = "predictions")
  ),
  
  # Body
  dashboardBody(
    box(plotOutput("totalEnergyConsumed"), width = "100%")
  )
)

server <- function(input, output) {
  output$totalEnergyConsumed <- renderPlot(
    data %>%
      group_by(date(DateTime)) %>% summarise(mean = mean(ActiveEnergy)) %>%
      ggplot(aes(`date(DateTime)`, mean)) + geom_line(color = "firebrick1") + geom_smooth(se = F) + 
      labs(title = "Total active energy consumed by day") + ylab("Watt/h") + xlab("Time") + theme_light()
  )
}

shinyApp(ui, server)



# dataset to work with ----------------------------------------------------

df <- data %>% 
  select(-season, -wday, -Period.Day, -Intensity, -ReactiveEnergy) %>% 
  group_by(date = date(DateTime)) %>% 
  summarise(ActiveEnergy_avg = mean(ActiveEnergy), 
            Kitchen_avg = mean(Kitchen),
            Laundry_avg = mean(Laundry),
            W.A_HeatCold_avg = mean(W.A_HeatCold),
            UnkownEnergy_avg = mean(UnkownEnergy))

round_var <- c("ActiveEnergy_avg","Kitchen_avg", "Laundry_avg",
               "W.A_HeatCold_avg","UnkownEnergy_avg")

df[,round_var] <- apply(df[,round_var], 2, round)

