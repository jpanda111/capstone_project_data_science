library(shiny)
library(RSQLite)
source('predict.R')
# attach("ngram.RData")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  db1 <- dbConnect(SQLite(), dbname="train.db")
  dbout1 <- reactive({ngram_stupid_backoff(raw=input$Text, m=input$n, db=db1)})
  db2 <- dbConnect(SQLite(), dbname="train_katz_back_off_test_only3gram.db")
  dbout2 <- reactive({ngram_katz_back_off(raw=input$Text, m=input$n, db=db2)})
  output$sentence <- renderText({input$Text})
  output$predicted <- renderText({
    out <- dbout1()
    if (out[1] == "Sorry! Cannot Find it!") {
      return(out)
    } else {
      return(unlist(out)[1])
    }
  })
  
  output$alts1 <- renderTable({dbout1()})
  output$alts2 <- renderTable({dbout2()})
})
