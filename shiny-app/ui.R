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
       textInput("Text",label=h3("Input the sentence"), value="hey you know"),
       numericInput("n", h5("Language Models used (bigram/trigram/quadgram) :"), value=3, min=2, max=4),
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
                 "to do the prediction. In Katz Backoff, it also uses 'Good Turing Smoothing' ",
                 a(href="https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation", "(Geoffrey Sampson, 1990)"),
                 "technique to deal with unseen n-grams"),
               h4("How to use", style="color:blue"),
               p("Input a sentence in the topleft panel, then select the language model you like to use, e.g. trigram by default 
                 and find an algorithm, e.g. Stupid Back Off by default for the n-gram model. 
                 Then press the PREDICT button or simply hit the enter. You will see"),
               p('next predicted word/s or most frequent unigram words if you do not input words.'),
               h4("Explanation", style="color:blue"),
               p("Procedure and Important Notes explained in the following tabs"),
               span('The Process',style = "color:gray"),
               br(),
               span('Important Notes',style = "color:gray")
              ),
        tabPanel("Stupid Back Off Model",
                 h5("Display top 3 words predicted"),
                 div(tableOutput("alts1"))
                 ),
        tabPanel("Katz Back Off Model with Good-Turing Smoothing",
                 h5("Display top 3 words predicted"),
                 div(tableOutput("alts2"))
                 ),
        tabPanel('The Process',
                 h5('This is how it is done'),
                 p("View This: ", a("Text Mining process", 
                                    href = "http://www.r-bloggers.com/text-mining/")),
                 p("And This: ", a("ngrams", 
                                   href = "http://www.slideshare.net/ShangxuanZhang/introducing-nlp-with-r"))
                 ),
        tabPanel("Important Notes",
                 h4("Speed", style="color:blue"),
                 p("Stupid Backoff is faster, I only extracted the terms occurred in the whole sources more than 4 times."),
                 p("Katz Backoff is slow, because we need to do conditional probability calculation, hence cannot remove low frequency terms"),
                 p('I suggested using Stupid Kick-off first as it fast and then using Katz Backoff when necessary because it is usually slow.'),
                 h4("Accuracy", style="color:blue"),
                 p("25% sampling size of training set is good enough to achieve 22% accuracy for TOP 3 predictions"),
                 h4("You have to enter words!", style="color:blue"),
                 p('If you only input punctuation, numbers and some common words. The model will remove them in the input and most frequent unigram will be returned.'),
                 h4("language models selection!", style="color:blue"),
                 p('The selection of bigram/trigram/quadgram is only applied for Stupid Backoff, the default is trigram with good enough accuracy and rather fast speed!'),
                 p('Katz Backoff is too slow for quadgram, so we make it trigram always!')
        ))
    )
  )
))
