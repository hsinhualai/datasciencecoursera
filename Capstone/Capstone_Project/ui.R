library(shiny)

ui <- fluidPage(
        titlePanel("Word Prediction for Datascience Capstone Project Coursera"),
        sidebarLayout(
                column(4, wellPanel(
                textInput("text", label = "Enter Partial Sentence",
                          value = "Enter texts here"),
                actionButton(inputId = "go", label = "Submit"),
                br(),
                h5("This app is developed for the Johns Hopkins University Data Science Specialization Capstone Project sponsored by Swiftkey")),
                br(),
                wellPanel(
                "Please wait for a while for making the word prediction for the first time using the application"
                )),
               
               mainPanel(column(8,
                      h4("If we inlude stop words, the most probable word appearing next is"),
                      verbatimTextOutput("value1"),
                      h4("The second word is"),
                      verbatimTextOutput("value2"),
                      h4("The third word is"),
                      verbatimTextOutput("value3"),
                      h4("The fourth word is"),
                      verbatimTextOutput("value4"),
                      h4("The fifth word is"),
                      verbatimTextOutput("value5")
                                ),
                      br(),
                      column(8,
                            h4("----------------------------------------------------"),
                            h4("If we ignore stop words, the most probable word appearing next is"),
                            verbatimTextOutput("value6"),
                            h4("The second word is"),
                            verbatimTextOutput("value7"),
                            h4("The third word is"),
                            verbatimTextOutput("value8"),
                            h4("The fourth word is"),
                            verbatimTextOutput("value9"),
                            h4("The fifth word is"),
                            verbatimTextOutput("value10")
                            )
                )
                )
        
)