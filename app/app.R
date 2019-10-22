# -------------------------------------------------------------------------
# GOAL: shiny app to communicate relevant information to final customer
# DEVELOPER:
# -------------------------------------------------------------------------


# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
    install.packages("pacman")
}
pacman::p_load(shiny, tidyverse, lubridate, highcharter)


# load data ---------------------------------------------------------------

data <- read_rds("data/clean_data/processed_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(
               outputId = "month_plot"
               )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$month_plot <- renderPlot({

        data %>% 
            filter(month(date_time) == 9, year(date_time) == 2010) %>%
            group_by(date = date_time) %>% 
            summarise(ActiveEnergy = mean(ActiveEnergy)) %>% 
            ggplot(aes(x = date, y = ActiveEnergy)) +
                geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
