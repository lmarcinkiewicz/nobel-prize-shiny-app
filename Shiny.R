#####################################################################################################################
# PROJECT:  Nobel Prize Shiny App
# PURPOSE:  Create an App to show the differences by gender of the Nobel Prize Laureates.
#           App includes the following visualizations:  # of prizes by year, by category, by gender (Bar Chart),
#                                                       Birth county of laureates by gender (Tree Map),
#                                                       Data table of winners (DataTable),
#                                                       Word Cloud of Motivation
# AUTHOR:  Lisa Marcinkiewicz
# LATEST:  JANUARY, 25, 2021
# DATA:    CSV file pulled from Nobel Prize API (http://api.nobelprize.org/v1/laureate.csv)
#####################################################################################################################

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
library(shinyinvoer)
library(wordcloud2)
library(stringi)



ui <- fluidPage(
  titlePanel("Gender Analysis of Nobel Prize Winners"),
  theme = shinytheme("journal"),
  sidebarLayout(sidebarPanel(
    selectInput(
      inputId = "category",
      label = "Select a Prize Category",
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
    uiOutput('button'),
    verbatimTextOutput('input_button')
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
      tabPanel(
        'Country of Birth',
        selectInput(
          inputId = "gender",
          label = "Gender",
          choices = c("all",
                      "female",
                      "male"),
          selected = "all"
        ),
        plotOutput('laureates_by_birth_country')
      ),
      tabPanel('Motivation Word Cloud',
               selectInput(
                 inputId = "gender_for_wordcloud",
                 label = "Gender",
                 choices = c("all",
                             "female",
                             "male"),
                 selected = "all"
               ),
               wordcloud2::wordcloud2Output('motivation_word_cloud'))
      
    )
  ))
)

server <- function(input, output, session)
{
  laureateURL <- "http://api.nobelprize.org/v1/laureate.csv"
  laureates <- read_csv(laureateURL)
  
  #Reactive that returns number of awards by category by year
  Awards_by_category <- reactive({
    if (input$category == 'all') {
      awards_by_category <- laureates
    } else{
      awards_by_category <- laureates %>%
        filter(category == input$category)
    }
  })
  

  #Render a data table output named "table_awards_by_category"
  output$table_awards_by_category <- DT::renderDT({
    awards_by_category_filtered <- Awards_by_category() %>%
      select(firstname, surname, gender, year, category)
    
    DT::datatable(
      awards_by_category_filtered,
      filter = 'top',
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        columnDefs = list(list(width = '100px', targets = c(0, 1)),
                          list(width = '100px', targets = c(3)),
                          list(width = '30px', targets = c(2, 4)))
      ),
      rownames = FALSE,
      style = 'bootstrap',
      colnames = c('First Name', 'Surname', 'Gender', 'Year', 'Prize Category'),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Data Source: ', htmltools::em('http://api.nobelprize.org/v1/laureate.csv'))
    )
    
    
    
    
  })
  
  #Render a plotly output named "plot_awards_by_category"
  output$plotly_awards_by_category <- plotly::renderPlotly({
    count_awards_by_category <- Awards_by_category() %>%
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
    awards_by_category_and_gender <-
      Awards_by_category_and_gender(input$gender)
    
    count_awards_by_birth_country <-
      awards_by_category_and_gender %>%
      group_by(bornCountry) %>%
      count()
    
    
    treemap(count_awards_by_birth_country,
            index = "bornCountry",
            vSize = "n")
    
  })
  
  #Create a function to return laureates for selected gender
  Awards_by_category_and_gender <- function(selected_gender) {
    if (selected_gender == "all") {
      Awards_by_category()
    } else{
      Awards_by_category() %>%
        filter(gender == selected_gender)
    }
  }
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    GetTermMatrix(input$gender_for_wordcloud)
  })
  
  
  # # Return Motiviation term matrix for selected gender
  GetTermMatrix <- function(gender_for_wordcloud) {
    laureateMotivation <- Awards_by_category_and_gender(gender_for_wordcloud)
    
    # join all the motivation text in a corpus
    motivationCorpus <-
      Corpus(VectorSource(laureateMotivation$motivation))
    

  motivationCorpus <- tm_map(motivationCorpus, removePunctuation)
    motivationCorpus <-
      tm_map(motivationCorpus, removeWords, c(stopwords("english")))
    motivationCorpus <- tm_map(motivationCorpus, stripWhitespace)
   motivationCorpus <-
     tm_map(motivationCorpus, content_transformer(tolower))
    motivationCorpus <- tm_map(motivationCorpus, removeNumbers)
    
   
   
    
    #motivationCorpus <- tm_map(motivationCorpus, content_transformer(gsub), pattern = "<e2><U+0092>", replacement = "")
   
    
    
    termMatrix <- as.matrix(TermDocumentMatrix(motivationCorpus))
    v <- sort(rowSums(termMatrix),decreasing=TRUE)
 
    
  }
  
  
  
  output$motivation_word_cloud <- renderWordcloud2({
    
    v <- terms()

    df <- data.frame(word = as.factor(names(v)), freq = v)
    top_100_words <- df %>% top_n(100)  # highest values

     wordcloud2(top_100_words)
    
    wordcloud2(top_100_words, size = 1, minSize = 0, gridSize =  0,
               fontFamily = 'Segoe UI', fontWeight = 'bold',
               color = 'random-dark', backgroundColor = "white",
               minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
               rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
               widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

   
    
  })
  
  
  
  # Downloadable csv of selected dataset ----
  output$download_data <- downloadHandler(
    filename = function() {
      #paste(Sys.time(), ' Table.csv', sep='')},
      'Awards_by_Category.csv'
    },
    content = function(file) {
      write_csv(awards_by_category, file)
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server)
