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
library(reactable)
library(tidyverse)
library(leaflet)
library(data.table)
library(lubridate)
library(scales)
library(shinycssloaders)

# Loading data

# URL
url_cases <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv"
url_cts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_dts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
url_locations <- "data/locations.csv"
# url_cases <- "data/full_data.csv"
# url_cts <- "data/time_series_covid19_confirmed_global.csv"
# url_dts <- "data/time_series_covid19_deaths_global.csv"

cases <- read_csv(url_cases,
                  num_threads = 4,
                  col_select = c(date,location,total_cases,total_deaths),
                  show_col_types = FALSE)

locations <- read_csv(url_locations,
                      num_threads = 4,
                      show_col_types = FALSE)

ctsdata <- read_csv(url_cts,
                    num_threads = 4,
                    show_col_types = FALSE)

ctsdata %>% 
  rename('subregion' = 'Province/State'
         ,'country' = 'Country/Region'
         ,'lat' = 'Lat'
         ,'long' = 'Long'
  ) -> ctsdata

ctsdata %>% 
  pivot_longer(cols = -one_of('country','subregion','lat','long')
               ,names_to = 'date'
               ,values_to = 'confirmed')%>% 
  mutate(date = mdy(date)) -> ctsdata


dtsdata <- read_csv(url_dts,
                    num_threads = 4,
                    show_col_types = FALSE)

dtsdata %>% 
  rename('subregion' = 'Province/State'
         ,'country' = 'Country/Region'
         ,'lat' = 'Lat'
         ,'long' = 'Long'
  ) -> dtsdata

dtsdata %>% 
  pivot_longer(cols = -one_of('country','subregion','lat','long')
               ,names_to = 'date'
               ,values_to = 'deaths')%>% 
  mutate(date = mdy(date)) -> dtsdata

## Data manipulation

#replace empty number of deaths with 0
cases$total_cases <- cases$total_cases %>% replace_na(0)

#replace empty number of deaths with 0
cases$total_deaths <- cases$total_deaths %>% replace_na(0)

#delete "international" from locations
locations <- locations%>%dplyr::filter(!is.na(population))

# Merging data
data <- left_join(cases, locations, by = "location")



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global dashboard", tabName = "dashboard", icon = icon("globe")),
      menuItem("Countries dashboard", tabName = "bycountry", icon = icon("flag")),
      menuItem("Raw data", tabName = "rawdata", icon = icon("database"))
    ),
    hr()
  ),
  dashboardBody(
    h1("World COVID-19 Dashboard"),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidPage(
          fluidRow(
            valueBoxOutput("nb_cases_gb", width = 6),
            valueBoxOutput("nb_deaths_gb",width = 6)
        ),
        fluidRow(
          box(width = 12, plotlyOutput("piechart_gb", height = "55vh", 
                                       width = "100%"))
        ),
        fluidRow(
          box(width = 12, plotlyOutput("barchart_gb", height = "55vh", 
                                       width = "100%"))
        )
      )
    ),
    tabItem(
      tabName = "bycountry",
      fluidPage(
        fluidRow(
          column(width = 4,selectInput("country", "Select a country", unique(ctsdata$country))) ,
          column(width = 4,valueBoxOutput(width = 12,"nb_cases_cn")) ,
          column(width = 4,valueBoxOutput(width = 12,"nb_deaths_cn"))
        ),
        fluidRow(
          box(width = 12, plotlyOutput("curves_cn", height = "55vh", 
                                       width = "100%"))
        )
      )
    ),
    
    tabItem(
      tabName = "rawdata",
      fluidPage(
        fluidRow(
          column(12,
                 offset = 9,
                 downloadButton('downloadData', 'Download CSV', 
                                style="width: 230px;color: white; background:#1790E1"),
                 style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:5px'
          )
        ),
        fluidRow(box(width = 12, 
                     shinycssloaders::withSpinner(reactableOutput("raw"), type = 1)))
      )
    )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #  Piechart
  prepdata_pie <- reactive({
    values <- c(
      sum(ctsdata %>% filter(date == max(date)) %>% select(confirmed)), 
      sum(dtsdata %>% filter(date == max(date)) %>% select(deaths))
      )
  })
  
  output$piechart_gb <- renderPlotly({
    labels <- c("Confirmed cases", "Deaths")
    fig <- plot_ly(type='pie', labels=labels, values=prepdata_pie(), 
                   textinfo='label+percent')
  })
  
  prepdata_bar <- reactive({
    data %>% filter(date == max(date)) %>% group_by(date, continent) %>% 
      summarise(cases = sum(total_cases), deaths = sum(total_deaths))
  })
  
  # Barchart
  output$barchart_gb <- renderPlotly({
    fig <- plot_ly(prepdata_bar(), x = ~continent, y = ~cases, type = 'bar', name = 'Confirmed')
    fig <- fig %>% add_trace(y = ~deaths, name = 'Deaths')
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
  })
  
  
  
  # Curves
  prepdata_curves <- reactive({
    full_join(ctsdata %>%
                filter(country==input$country),
              dtsdata %>%
                filter(country==input$country),
              by = c("country", "date"))%>%
      select(date, confirmed, deaths)
  })
  
  output$curves_cn <- renderPlotly({
    
    basep <- plot_ly(
      xaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      yaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      plot_bgcolor='#e5ecf6'
    )
    
    fig <- plot_ly(
      prepdata_curves()
      , type = 'scatter', mode = 'lines')%>%
      add_trace(basep, x = ~date, y = ~confirmed, name = 'Confirmed',
                line = list(color = 'rgb(0,0,128)', width = 2))%>%
      add_trace(basep, x = ~date, y = ~deaths, name = 'Deaths',
                line = list(color = 'rgb(255, 0, 0)', width = 2))%>%
      layout(xaxis = list(title = 'Date'), yaxis = list(title = 'Number'), 
             title = paste('Confirmed cases and deaths in', input$country))
  })
  
  # Value boxes
  
  # Global
  nb_cases_total_gb <- reactive({
    scales::label_comma(accuracy = NULL)(sum(ctsdata %>% filter(date == max(date)) %>% select(confirmed)))
  })
  
  nb_deaths_total_gb <- reactive({
    scales::label_comma(accuracy = NULL)(sum(dtsdata %>% filter(date == max(date)) %>% select(deaths)))
  })
  
  output$nb_cases_gb <- renderValueBox({
    valueBox(
      "Total confirmed cases in the world",
      value = nb_cases_total_gb(),
      color = "light-blue"
    )
  })
  
  output$nb_deaths_gb <- renderValueBox({
    valueBox(
      "Total death cases in the world",
      value = nb_deaths_total_gb(),
      color = "red"
    )
  })
  
  # Country
  nb_cases_total_cn <- reactive({
    scales::label_comma(accuracy = NULL)(sum(ctsdata %>% filter(date == max(date) & country == input$country) %>% select(confirmed)))
  })
  
  nb_deaths_total_cn <- reactive({
    scales::label_comma(accuracy = NULL)(sum(dtsdata %>% filter(date == max(date) & country == input$country) %>% select(deaths)))
  })
  
  output$nb_cases_cn <- renderValueBox({
    valueBox(
      paste("Total confirmed cases in", input$country),
      value = nb_cases_total_cn(),
      color = "light-blue"
    )
  })
  
  output$nb_deaths_cn <- renderValueBox({
    valueBox(
      paste("Total death cases in ", input$country),
      value = nb_deaths_total_cn(),
      color = "red"
    )
  })
  
  
  # Raw data
  
  prepdataraw <- reactive({
    data %>% 
      select(date, continent, location, total_cases, total_deaths, population, lat, lng)
    #mutate(date = mdy(date))%>%
  })
  
  output$raw <- renderReactable(
        prepdataraw() %>%
            reactable(
               columns = list(
                 date = colDef(name = "Date", align = "center"),
                 location = colDef(name = "Country name", align = "center"),
                 total_cases = colDef(name = "Confirmed cases", align = "center"),
                 total_deaths = colDef(name = "Deaths", align = "center"),
                 continent = colDef(name = "Continent", align = "center"),
                 population = colDef(name = "Population size", align = "center"),
                 lat = colDef(name = "Latitude", align = "center"),
                 lng = colDef(name = "Longitude", align = "center")
               ),
               filterable = TRUE,
               highlight = TRUE,
               defaultPageSize = 10,
               showPageSizeOptions = TRUE
             )
     )
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("covid-dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data %>% 
                  select(date, continent, location, total_cases, total_deaths, population, lat, lng),
                  file)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
