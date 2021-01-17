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
library(readr)
library(treemap)

ui <- fluidPage(
  titlePanel("Gender Analysis of Nobel Prize Winners"),
  themeSelector(),
  sidebarLayout(sidebarPanel(
    selectInput(
      inputId = "category",
      label = "Select a Category",
      choices = c(
        "all",
        "chemistry",
        "economics",
        "literature",
        "medicine",
        "peace",
        "physics"
      )
    ),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Prize Laureates',
        #Add DT output to display awards by prize category
        DT::DTOutput('table_awards_by_category'),
        #Add DownLoad button on DataTable tab
        downloadButton("download_data", "Download")
      ),
      tabPanel(
        'Winners by Gender Over Time',
        # Add plotly output to display awards by prize category
        plotly::plotlyOutput('plotly_awards_by_category')
      ),
      tabPanel('Country of Birth', plotOutput('laureates_by_birth_country')),
      tabPanel('Motivation Wordcloud')
      
    )
  ))
)

server <- function(input, output, session)
{
  laureateURL <- "http://api.nobelprize.org/v1/laureate.csv"
  laureates <- read.csv(laureateURL)
  
  
  #Reactive that returns number of awards by category by year
  awards_by_category <- reactive({
    if (input$category == 'all') {
      awards_by_category <- laureates
    } else{
      awards_by_category <- laureates %>%
        filter(category == input$category)
    }
  })
  
  
  #Render a data table output named "table_awards_by_category"
  output$table_awards_by_category <- DT::renderDT({
    awards_by_category_filtered <- awards_by_category() %>%
      select(firstname, surname, gender, year, category, motivation)
    
    DT::datatable(awards_by_category_filtered, style = 'bootstrap')
    
  })
  
  #Render a plotly output named "plot_awards_by_category"
  output$plotly_awards_by_category <- plotly::renderPlotly({
    count_awards_by_category <- awards_by_category() %>%
      group_by(year, gender) %>%
      count()
    
    ggplot(count_awards_by_category, aes(
      x = year,
      y = n,
      text = paste(
        'Gender: ',
        as.factor(gender),
        '<br>Year:  ',
        year,
        '<br>Number of Prize Winners',
        n
      )
    )) +
      geom_col(aes(fill = as.factor(gender))) +
      labs(title = "Prizes Awarded by Category",
           x = "Year", y = "Number of Prizes:") +
      labs(fill = "") +
      scale_fill_manual(values = c("#F1A3AD", "#32527B", "#D7BE69")) +
      theme_classic()
  })
  
  
  #Render a tree plot to represent country of birth"
  output$laureates_by_birth_country <- renderPlot({
    count_awards_by_birth_country <- awards_by_category() %>%
      group_by(bornCountry) %>%
      count()
    
    treemap(count_awards_by_birth_country, 
            index = "bornCountry",
            vSize = "n")
     
  })
  
  
  # Downloadable csv of selected dataset ----
  output$download_data <- downloadHandler(
    filename = function() {
      #paste(Sys.time(), ' Table.csv', sep='')},
      'Awards_by_Category.csv'
    },
    content = function(file) {
      write.csv(awards_by_category, file)
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server)
