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
         num_threads = 4,
         col_select = c(date,location,total_cases,total_deaths),
         show_col_types = FALSE)

locations <- read_csv(url_locations,
                      num_threads = 4,
                      show_col_types = FALSE)
## Data manipulation

#replace empty number of deaths with 0
cases$total_deaths <- cases$total_deaths %>% replace_na(0)

#delete "international" from locations
locations <- locations%>%dplyr::filter(!is.na(population))

# Merging data
data <- left_join(cases, locations, by = "location")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "World COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuSubItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Raw data", tabName = "rawdata", icon = icon("database"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("curve"))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$curve <- renderPlot({
    fig <- plot_ly(data, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~date, y = ~total_cases)
    fig <- fig %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6', width = 900)
    
    
    fig
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
