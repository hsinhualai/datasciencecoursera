library(shiny)

ui <- fluidPage(
        titlePanel("Word Prediction for Datascience Capstone Project Coursera"),
        sidebarLayout(
                column(4, wellPanel(
                textInput("text", label = "Enter a sentence",
                          value = "Enter text here"),
                actionButton(inputId = "go", label = "Submit"))),
               
               
               mainPanel(column(8,
                      h4("The most likely five words appearing next are") ,
                      verbatimTextOutput("value")
                )
                )
                )
        
)