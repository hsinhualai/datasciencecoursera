library(shiny)
library(tm)
library(RWeka)
library(SnowballC)
library(slam)
library(dplyr)

server <- function(input, output,session){
        source("Prediction.R") # load the prediction functions
        load("Text_model/300000_w_stopword_unigram90.RData",envir = .GlobalEnv) 
        load("Text_model/300000_w_stopword_bigram.RData",envir = .GlobalEnv)
        load("Text_model/300000_w_stopword_REDUCED_trigram.RData",envir = .GlobalEnv)
        
        text.in <- eventReactive(input$go,{as.character(input$text)})
        
        output$value <- renderText({
                #text.in <- as.character(input$text)
                ngram_predict(text.in())[1:5]})
        
}        