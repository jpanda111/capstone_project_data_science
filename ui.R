library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("N-gram Word Prediction"),
  h4("John Hopkins Coursera Data Science Capstone Project", style="color:gray"),
  h5("Jpanda111", style="color:gray"),
  hr(), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("Text",label=h3("Input the sentence"), value="10th ave new York"),
       numericInput("n", h5("Numbers of predicted words"), value=2),
       radioButtons("radio", h5("Smoothing"), choices = list("Stupid Back-off"=1, "Katz Back-off"=2), selected = 1),
       helpText("Type in a sentence above, pick the corresponding modeling algorithm, press the 'Predict' button or simply hit enter, and the result will
                display to the right"),
       submitButton("Predict"),
       hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      br(),
      h2(textOutput("sentence"), align="center"),
      h1(textOutput("predicted"), align="center", style="color:blue"),
      hr(),
      tabsetPanel(
        type="tabs",
        tabPanel("Summary",
               h4("Preamble", style="color:blue"),
               p("Predictive text algorithm used to make next word predictions based on frequency of occurence of n-grams."),
               p("The ", a(href="http://en.wikipedia.org/wiki/N-gram","n-grams"), "model is generated from a dataset of Twitter, news 
                 and blogs. It uses both 'Stupid Backoff Algorithm' ", 
                 a(href="http://www.hlt.utdallas.edu/~sanda/courses/NLP/Lecture06.pdf","(Sanda Harabagiun 2003)"),
                 "and 'Katz Backoff Algorithm' ",
                 a(href="https://en.wikipedia.org/wiki/Katz%27s_back-off_model", "(Slava M. Katz, 1987)"), 
                 "to do the prediction. It also uses 'Good Turing Smoothing' ",
                 a(href="https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation", "(Geoffrey Sampson, 1990)"),
                 "technique to deal with unseen n-grams"),
               h4("How to use", style="color:blue"),
               p("Input a sentence in the topleft panel, then select the number of words you'd like to see, e.g. 3 words by default 
                 and try to find an algorithm, e.g. Stupid Back Off by default for the n-gram model. 
                 Then press the PREDICT button or simply hit the enter. You will see"),
               p('next predicted word/s or a WARNING.'),
               h4("Explanation", style="color:blue"),
               p("Procedure and Issues explained in the following tabs"),
               span('The Process',style = "color:gray"),
               br(),
               span('Shortcomings',style = "color:gray")
              ),
        tabPanel("Stupid Back Off Model",
                 h5("Display top 5 words predicted", align="center"),
                 div(tableOutput("alts1"), align="center")
                 ),
        tabPanel("Katz Back Off Model with Good-Turing Smoothing",
                 h5("Display top 5 words predicted", align="center"),
                 div(tableOutput("alts2"), align="center")
                 ),
        tabPanel('The Process',
                 h5('This is how it is done'),
                 p("View This: ", a("Text Mining process", 
                                    href = "http://www.r-bloggers.com/text-mining/")),
                 p("And This: ", a("ngrams", 
                                   href = "http://www.slideshare.net/ShangxuanZhang/introducing-nlp-with-r"))
                 ),
        tabPanel("Shortcomings",
                 h5("Kneser-Ney Smoothing is slow"),
                 p('Well, I suggested using Stupid Kick-off first as it fast and then using Kneser-Ney when necessary because it is usually slow.'),
                 h5("words!"),
                 p("Dirty words will be deleted from the final result."),
                 h5("Nothing return when you input something?"),
                 p('This will occur when you only input punctuation, numbers and some common words. The model will remove them in the input and nothing will return.'),
                 h4("Important"),
                 p("To make this model faster, I only extracted the terms occurred in the whole sources more than 5 times. ")              
        ))
    )
  )
))
