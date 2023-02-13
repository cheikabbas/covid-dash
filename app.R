#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(DT)
library(tidyverse)
library(leaflet)
library(data.table)

# Loading data

# df = read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
#               header = TRUE)


# URL
url_cases <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv"
url_locations <- "data/locations.csv"

cases <- read_csv(url_cases,
         num_threads = 4, progress = show_progress(),
         col_select = c(date,location,total_cases,total_deaths))

locations <- read_csv(url_locations,
                      num_threads = 4, progress = show_progress())
## Data manipulation

#replace empty number of deaths with 0
cases$total_deaths <- df$total_deaths %>% replace_na(0)

#delete "international" from locations
locations <- locations%>%dplyr::filter(!is.na(population))

# Merging data
data <- left_join(cases, locations, by = "location")


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
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
