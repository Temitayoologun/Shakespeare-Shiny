library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}


## UI ##########################################################################

ui <- fluidPage(
  #theme = shinytheme("cerulean"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "books", label = "Choose a book:",
                  choices = books,
                  selected = "summer"),
      
      
      checkboxInput(inputId="stopwords", "Stopwords:", value=TRUE),
      
      
      
      actionButton(inputId = "update", 
                   label = "Rerun"),
      
      
      hr(),
      
      
      h3("Word Cloud Settings"),
      
      
      sliderInput(inputId = "maxwords", label = "Max # of Words:", 
                  value = 100, min = 10, max = 200, step = 10),
      
      
      sliderInput(inputId = "minwords", label = "Size of largest words:", 
                  value = 4, min = 1, max = 8),
      
      
      
      sliderInput(inputId = "sizewords", label = "Size of smallest words:", 
                  value = 0.5, min = 0.1, max = 4),
      
      
      hr(),
      
      
      h3("Word Count Settings"),
      
      
      sliderInput(inputId = "mincount", label = "Minimum words for Counts Chart:", 
                  value = 25, min = 10, max = 100, step = 9),
      
      
      sliderInput(inputId = "sizecount", label = "Word size for Counts Chart:", 
                  value = 14, min = 8, max = 30, step = 3),
      
      
    ),
    
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Word Cloud",
                           plotOutput('cloud', height = "600px", width="100%")),
                  tabPanel(title = "Word Counts",
                           plotOutput('freq', height = "600px", width="100%"))
                  
                  
      )
      
    )
  )
)


## SERVER ######################################################################
server <- function(input, output) {
  
  
  freq <- eventReactive (input$update, {
    ({
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$books, input$stopwords) # ... = replace with the two inputs from Task 2
      })
      
    })
  })
  
  output$cloud <- renderPlot ({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(0.5, 2),
          random.order = FALSE, 
          max.words = 100, 
          colors=pal))
  })
  
  
  
  output$freq <- renderPlot({
    v <- freq()
    v %>% filter(n > input$mincount) %>%
      ggplot(aes(reorder(word,n),n)) +
      geom_bar(stat = "identity") +
      theme(text = element_text(size = input$sizecount), 
            axis.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
}

## Run the application #########################################################
shinyApp(ui = ui, server = server)

