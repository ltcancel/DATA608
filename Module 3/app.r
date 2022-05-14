library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

data <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")
filter_data <- data %>%
  filter(Year == 2010)

ui <- fluidPage(
  
  headerPanel("2010 Cause of Death by State"),
  
  sidebarPanel(
   selectInput('cause','Cause of Death', unique(filter_data$ICD.Chapter)) 
  ),
  
  mainPanel(plotlyOutput("plot1"))
)
server <- function(input,output){
  
  output$plot1 <- renderPlotly({
      
  filterdf <- filter_data %>% 
    filter(ICD.Chapter == input$cause)
  
  plot_ly(filterdf, x = ~State, y = ~Crude.Rate, type = 'bar') %>%
  layout(xaxis = list(title="State", tickangle = -45),
         yaxis = list(title = "Death Rate"))
  })
  
}

shinyApp(ui, server)

