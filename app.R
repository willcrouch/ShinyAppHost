# Practice Shiny to call from Github
# Map and some data pieces

library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(plotly)

# Define UI for application
ui <- dashboardPage(
  skin = "black",
  dashboardHeader( title = "Test Shiny"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 6,
        plotlyOutput("plot")
      ),
      box(width = 6,
        dataTableOutput("table")
      )
    ),
    tags$style(type = "text/css", "#map {height: calc(60vh - 80px) !important;}"),
    leafletOutput("map")
  )
)


## Define Global Data
# Map Data
CityList = data.frame(
  "City" = character(4),
  "Latitude" = numeric(4),
  "Longitude" = numeric(4)
)

CityList$City = c("Mumbai","Chennai","Jakarta","Bangladesh")
CityList$Latitude = c(18.936614,13.079552,-6.210100,23.793007)
CityList$Longitude = c(72.836302,80.272049,106.846454,90.411765)

# Chart Data
Timeframe = 1520:2040
DataSize = length(Timeframe)
RandomWalkGDP = 300 + 2*Timeframe + rnorm(DataSize, mean = 0, sd = 100)
LMModel = lm(RandomWalkGDP~Timeframe)
SummaryData = summary(LMModel)
RandomWalkGDP[510:521] = NA # Empty data for forecast
GDPEstMean = predict(LMModel)
GDPEstHigh = predict(LMModel)
GDPEstHigh[510:521] = predict(LMModel)[510:521] + SummaryData$sigma*2
GDPEstLow = predict(LMModel)
GDPEstLow[510:521] = predict(LMModel)[510:521] - SummaryData$sigma*2


ChartData = data.table(
  "Year" = integer(DataSize),
  "GDP" = numeric(DataSize),
  "GDP Estimate" = numeric(DataSize),
  "GDP Estimate High" = numeric(DataSize),
  "GDP Estimate Low" = numeric(DataSize)
)
ChartData$Year = Timeframe
ChartData$GDP = RandomWalkGDP
ChartData$`GDP Estimate` = GDPEstMean
ChartData$`GDP Estimate High` = GDPEstHigh
ChartData$`GDP Estimate Low` = GDPEstLow


# Define server logic
server <- function(input, output) {
  # Plotly gdp with estimate
  output$plot = renderPlotly({
    plot_ly(
      data = ChartData, x = ~Year, y = ~`GDP Estimate High`,
      type = "scatter", mode = "line", name = "GDP High Estimate",
      showlegend = FALSE, line = list(color = 'transparent'), text = ~paste("GDP High Estimate: ", 
      sprintf("$%s", format((round(`GDP Estimate High`,0)), big.mark = ",", scientific = FALSE))),
      hoverinfo =  'text'
      ) %>%
      add_trace(y = ~`GDP Estimate`,
                type = "scatter", mode = "line", name = "GDP Mean Estimate",
                fill = 'tonexty', line = list(color = 'blue'), 
                fillcolor='rgba(120,120,120,0.2)', text = ~paste("GDP Mean Estimate: ", 
                sprintf("$%s", format((round(`GDP Estimate`,0)), big.mark = ",", scientific = FALSE))),
                hoverinfo =  'text'
      ) %>%
      add_trace(y = ~`GDP Estimate Low`,
                type = "scatter", mode = "line", name = "GDP Low Estimate",
                fill = 'tonexty', line = list(color = 'transparent'), 
                fillcolor='rgba(120,120,120,0.2)', text = ~paste("GDP Low Estimate: ", 
                sprintf("$%s", format((round(`GDP Estimate Low`,0)), big.mark = ",", scientific = FALSE))),
                hoverinfo =  'text'
      ) %>%
      add_trace(y = ~GDP,
                type = "scatter", mode = "markers", name = "GDP",
                color = "black", text = ~paste("GDP: ", 
                sprintf("$%s", format((round(GDP,2)), big.mark = ",", scientific = FALSE))),
                hoverinfo =  'text'
      )
  })
  
  # Table of forecast
  output$table = renderDataTable(
    ChartData[510:521,c(1,3,4,5)],
    options = list(dom = 't', searching = FALSE)
  )
  
  # Map with locations
  output$map = renderLeaflet({
    leaflet(CityList) %>%
      setView(lat = 8.2, lng = 88.8, zoom = 3) %>%
      addTiles() %>%
      addMarkers(lat = ~Latitude, lng = ~Longitude, popup = ~City)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

