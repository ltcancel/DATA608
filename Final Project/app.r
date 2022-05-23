library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

transformed_df <- read.csv("https://raw.githubusercontent.com/ltcancel/DATA608/main/Final%20Project/final_df.csv")

ui <- fluidPage(
  
  headerPanel("Food Sufficiency for Households: March 30-April 11"),
  
  sidebarPanel(
    selectInput('cause','Survey Question', unique(transformed_df$Question)) 
  ),
  
  fluidRow(
    column(10,plotOutput("plot1"))
    #column(6,tableOutput("table1"))    
  )
  
  
  #fluidRow(
  # mainPanel(plotOutput("plot1", width = "100%"), tableOutput("table1"))
  #)
  
  
  
  
)
server <- function(input,output){
  
  output$plot1 <- renderPlot({
    
    filterdf <- transformed_df %>% 
      filter(Question == input$cause)
    
    ggplot(filterdf, aes(fill = response, x = State, y = count)) +  
      geom_bar(position = "stack", stat = "identity") +
      facet_wrap(~Characteristics, ncol = 1) +
      theme(legend.position = "top", axis.text = element_text(size = 14), axis.text.x = element_text(angle = 90), axis.title = element_text(size = 14), text = element_text(size = 14)) + 
      labs(y = "Household Count", fill = "Survey Response")
    
  })
  
  output$table1 <- renderTable({
    filterdf <- transformed_df %>%
      filter(Question == input$cause)
    
    filterdf %>%
      na.omit() %>%
      group_by(State, response) %>%
      summarise(count = sum(count)) %>%
      spread(State, count)
    
  })
  
}

shinyApp(ui, server)