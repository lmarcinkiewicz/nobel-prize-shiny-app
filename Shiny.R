library(httr)
library(tidyverse)
library(lubridate)
library(tm)
library(stringr)
library(shiny)
library(ggthemes)

ui <- fluidPage(
  titlePanel("Nobel Prize"),
  selectInput(inputId = "category", label = "Select a Category", choices = c("chemistry", "economics", "literature", "medicine", "peace", "physics")),
  sliderInput("year", "Select Year", min = 1900, max = 2010, value = 1970),
 
   # Add plot output to display awards by prize category
  plotOutput('plot_awards')
  )

server <- function(input, output, session){
  
  
  output$plot_awards <- renderPlot({
    
    laureateURL <- "http://api.nobelprize.org/v1/laureate.csv"
    laureates <- read.csv(laureateURL)
    
    #number of awards by category by year
    
    awards_by_category <- laureates %>%
      filter(category == input$category) %>%
      group_by(year, gender) %>%
      count()

    ggplot(awards_by_category, aes(x = year, y=n)) +
      geom_col(aes(fill = as.factor(gender))) +
      labs(title="Prizes Awarded by Category",
           x ="Year", y = "Number of Prizes") +
      theme_classic()
      
    
  })
    
}

shinyApp(ui = ui, server = server)

