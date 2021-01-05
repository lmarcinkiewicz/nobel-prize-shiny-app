library(httr)
library(tidyverse)
library(lubridate)
library(tm)
library(stringr)
library(shiny)
library(ggthemes)
library(plotly)
library(data.table)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Nobel Prize"),
  themeSelector(),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "category", label = "Select a Category", choices = c("chemistry", "economics", "literature", "medicine", "peace", "physics")),
  sliderInput("year", "Select Year", min = 1900, max = 2010, value = 1970),
    ),
  mainPanel(
    tabsetPanel(
      tabPanel('Table',
    #Add DT output to display awards by prize category
    DT::DTOutput('table_awards_by_category')),
    tabPanel('Plot',
    # Add plotly output to display awards by prize category
    plotly::plotlyOutput('plotly_awards_by_category'))
  )
  )
  )
 )

server <- function(input, output, session){
  laureateURL <- "http://api.nobelprize.org/v1/laureate.csv"
  laureates <- read.csv(laureateURL)
  
  #Function that returns number of awards by category by year
  awards_by_category <- function(){
    awards_by_category <- laureates %>%
    filter(category == input$category) %>%
    group_by(year, gender) %>%
    count()
  }

  #Render a data table output named "table_awards_by_category"
   output$table_awards_by_category <- DT::renderDT({
    DT::datatable(awards_by_category())
  })
  
  #Render a plotly output named "plot_awards_by_category"
    output$plotly_awards_by_category <- plotly::renderPlotly({
      ggplot(awards_by_category(), aes(x = year, y=n)) +
        geom_col(aes(fill = as.factor(gender))) +
        labs(title="Prizes Awarded by Category",
             x ="Year", y = "Number of Prizes") +
        theme_classic()
  })
    
}

shinyApp(ui = ui, server = server)

