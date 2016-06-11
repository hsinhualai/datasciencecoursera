library(shiny)
library(tm)
library(RWeka)
library(SnowballC)
library(slam)
library(wordcloud)
library(dplyr)
server <- function(input, output,session){
        source("Prediction_5gram.R") # load the prediction functions
        load("Text_model/200000_w_stopword_unigram90.RData",envir = .GlobalEnv) 
        load("Text_model/200000_w_stopword_bigram.RData",envir = .GlobalEnv)
        load("Text_model/200000_w_stopword_REDUCED_trigram.RData",envir = .GlobalEnv)
        load("Text_model/200000_w_stopword_REDUCED_quadgram.RData",envir = .GlobalEnv)
        load("Text_model/200000_w_stopword_REDUCED_pentagram.RData",envir = .GlobalEnv)
        stopwords <- readLines("stopwords.txt", warn = FALSE)
        
        
        text.in <- eventReactive(input$go,{as.character(input$text)})
        
        a <- reactive(ngram_predict(text.in()))
        aper <- reactive(sort(a()[!(names(a()) %in% stopwords)], decreasing = T))

        output$value1 <- renderText({names(a())[1]})
        output$value2 <- renderText({names(a())[2]})
        output$value3 <- renderText({names(a())[3]})
        output$value4 <- renderText({names(a())[4]})
        output$value5 <- renderText({names(a())[5]})
        
        output$value6 <- renderText({names(aper())[1]})
        output$value7 <- renderText({names(aper())[2]})
        output$value8 <- renderText({names(aper())[3]})
        output$value9 <- renderText({names(aper())[4]})
        output$value10 <- renderText({names(aper())[5]})
        
        
        
}        