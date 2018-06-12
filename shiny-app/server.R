library(shiny)
library(RSQLite)
library(stringr)
library(tm)
source('predict.R')
# attach("ngram.RData")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  db <- dbConnect(SQLite(), dbname="train_stupid_back_off.db")
  dbout1 <- reactive({ngram_stupid_backoff_sql(raw=input$Text, m=input$n, db=db)})
  dbout2 <- reactive({ngram_katz_backoff_sql(raw=input$Text)})
  output$sentence <- renderText({input$Text})
  output$predicted <- renderText({
    out <- dbout1()
    return(unlist(out)[1])
  })
  
  output$alts1 <- renderTable({dbout1()})
  output$alts2 <- renderTable({dbout2()})
})
