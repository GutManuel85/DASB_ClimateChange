library(readr)
library(datasets)
library(dplyr)
library(shiny)
dataset <- read_csv("MyCleanedGlobalLandTemperaturesByCountry.csv")
countries <- unique(dataset$Country)
dataset$dt <- format(dataset$dt, format = "%Y")
dataset <- subset(dataset, select = c(dt, AverageTemperature, Country))
dataset <- group_by(dataset, dt, Country) %>% summarize(AverageTemperature = mean(AverageTemperature))
dates <- as.numeric(as.character(dataset$dt))
View(dataset)

min_date <- min(dates)
min_date
max_date <- max(dates)
max_date

ui <- fluidPage(
  tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 60%; /* Webkit browsers */
}
              "),
  sliderInput(inputId = "min_date", label = "Start date", value = 1850, min = min_date, 
              max = max_date, sep = ""),
  sliderInput(inputId = "max_date", label = "End date", value = max_date, min = min_date, 
              max = max_date, sep = ""),
  selectInput(inputId = "country1", label = "Country 1", choices = countries, selected = "Switzerland"),
  selectInput(inputId = "country2", label = "Country 2", choices = countries, selected = "Ecuador"),
  plotOutput("line1"),
  plotOutput("line2")

)

server <- function(input, output, session) {
  output$line1 <- renderPlot({
    title <- paste("Average annual temperature in", input$country1)
    plot(dataset$dt[dataset$Country == input$country1 &
                      dataset$dt >= input$min_date & dataset$dt <= input$max_date],
         dataset$AverageTemperature[dataset$Country == input$country1 &
                                      dataset$dt >= input$min_date & dataset$dt <= input$max_date],
         type="l", lwd=5, lty=3, col="red", main = title,
         ylab="degrees", xlab="year")
  })   
    output$line2 <- renderPlot({
    title <- paste("Average annual temperature in", input$country2)
    plot(dataset$dt[dataset$Country == input$country2 &
                      dataset$dt >= input$min_date & dataset$dt <= input$max_date],
         dataset$AverageTemperature[dataset$Country == input$country2 &
                                      dataset$dt >= input$min_date & dataset$dt <= input$max_date],
         type="l", lwd=5, lty=3, col="green", main = title,
         ylab="degrees", xlab="year")
  })
}

shinyApp(ui, server)

