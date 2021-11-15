library("shiny")
library("tidytext")
library("tidyverse")
library("ggplot2")
library("wordcloud")
library("shinythemes")
library("RColorBrewer")
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

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


# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  sidebarLayout(
    sidebarPanel(
      selectInput("sidePanelSelectInput", "Select a Book", books),
      checkboxInput("sidePanelCheckBoxInput", "Remove Stop Words?", value=TRUE),
      actionButton("sidePanelActionButtonInput","Run"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("sidePanelSliderInput.Max", "Max Number of Words in the Cloud",10,200,100,10),
      sliderInput("sidePanelSliderInput.Largest", "Size of Largest Word",1,8,4),
      sliderInput("sidePanelSliderInput.Smallest", "Size of Smallest Word",.1,4,.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput("sidePanelSliderInput.Min", "Min Number of Words in the Chart",10,100,25),
      sliderInput("sidePanelSliderInput.Font", "Font Size of the Words",8,30,14)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud",
                 plotOutput("cloud", height = "600px")
                 ), 
        tabPanel("Word Counts",
                 plotOutput("freq", height = "600px")
                 )
      )
    )
  )
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  freq <-
    eventReactive(input$sidePanelActionButtonInput, {
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$sidePanelSelectInput, input$sidePanelCheckBoxInput)
      })
    })
  
  # task5: add in reactivity for getFreq function based on inputs
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8, "Dark2")
    
    v %>%
      with(wordcloud(
        word,
        n,
        scale = c(input$sidePanelSliderInput.Largest, input$sidePanelSliderInput.Smallest),
        random.order = FALSE,
        max.words = input$sidePanelSliderInput.Max,
        colors = pal
      ))
  })
  
  output$freq <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8, "Dark2")
    
    v %>%
      filter(n>input$sidePanelSliderInput.Min) %>%
      ggplot(v,mapping=aes(n, reorder(word,n)))+
      geom_col()+
      theme(text = element_text(size=input$sidePanelSliderInput.Font))+
      xlab('')+
      ylab('')
  })
}

shinyApp(ui = ui, server = server)
