
library(shiny)

ui <- navbarPage(title = "Central Limit Theorem",
                 tabPanel(title = "Random Exponential Distribution",
                          tags$h1("Proof of Central Limit Theorem"),
                          tags$h2("Random Exponential Distribution"),
                          tags$em("Here we start from a random exponential distribution with",tags$strong("rate = 0.2."), "We first generate",tags$strong("40"), "random values and obtain the their aveage. We keep repeatin the steps for many times and plot the distribution of the means of the values. The slide below gives the number of iterations we want to repeat the steps. We can see if the number of iterations becomes large, the distribution becomes Gaussian distribtion whose", tags$strong("theoretical mean is 1/rate = 5"), "and", tags$strong("theoretical variance is rate^(-2)/40 = 0.625"), "In the plot below, the vertical blue line locates the sample mean while the vertical red line locates the theoretical mean, which match with each other when number of iterations becomes large. The green line gives the theoretical Gaussian distribution"),
                          wellPanel(
                          sliderInput(inputId = "expnum", 
                                      label = "Number of iterations", 
                                      value = 25, min = 1, max = 10000)
                          ),
                          plotOutput("norm"),
                          tags$h5("Sample data mean, compared with the theoretical mean 1/rate = 5"),
                          verbatimTextOutput("mnorm"),
                          tags$h5("Sample data variance, compared with the theoretical variance of a Gaussian distribution, 1/rate^2/40 = 0.625"),
                          verbatimTextOutput("normvar")
                 ),
                 
                 tabPanel(title = "Random Poisson Distribution",
                          tags$h1("Proof of Central Limit Theorem"),
                          tags$h2("Random Poisson Distribution"),
                          tags$em("Here we start from a random Poisson distribution with",tags$strong("lambda = 0.2."), "We first generate",tags$strong("40"), "random values and obtain the their aveage. We keep repeatin the steps for many times and plot the distribution of the means of the values. The slide below gives the number of iterations we want to repeat the steps. We can see if the number of iterations becomes large, the distribution becomes Gaussian distribtion whose", tags$strong("theoretical mean is lambda = 0.2"), "and", tags$strong("theoretical variance is lambda/40 = 0.005"), "In the plot below, the vertical blue line locates the sample mean while the vertical red line locates the theoretical mean, which match with each other when number of iterations becomes large. The green line gives the theoretical Gaussian distribution"),
                          wellPanel(
                          sliderInput(inputId = "poisnum", 
                                      label = "Number of iterations", 
                                      value = 25, min = 1, max = 10000)
                          ),
                          plotOutput("pois"),
                          tags$h5("Sample data mean, compared with the theoretical mean lambda = 0.2"),
                          verbatimTextOutput("poismean"),
                          tags$h5("Sample data variance, compared with the theoretical variance of a Gaussian distribution, lambda/40 = 0.005 "),
                          
                          verbatimTextOutput("poisvar")
                 
                 ),
                 
                 tabPanel(title = "Random Uniform Distribution",
                          tags$h1("Proof of Central Limit Theorem"),
                          tags$h2("Random Uniform Distribution"),
                          tags$em("Here we start from a random uniform distribution. We first generate",tags$strong("40"), "random values and obtain the their aveage. We keep repeatin the steps for many times and plot the distribution of the means of the values. The slide below gives the number of iterations we want to repeat the steps. We can see if the number of iterations becomes large, the distribution becomes Gaussian distribtion whose", tags$strong("theoretical mean is 0.5"), "and", tags$strong("theoretical variance is 1/12/40 = 0.00208"), "In the plot below, the vertical blue line locates the sample mean while the vertical red line locates the theoretical mean, which match with each other when number of iterations becomes large. The green line gives the theoretical Gaussian distribution"),
                          wellPanel(
                          sliderInput(inputId = "unifnum", 
                                      label = "Number of iterations", 
                                      value = 25, min = 1, max = 10000)
                          ),
                          plotOutput("unif"),
                          tags$h5("Sample data mean, compared with the theoretical mean lambda = 0.5"),
                          verbatimTextOutput("unifmean"),
                          tags$h5("Sample data variance, compared with the theoretical variance of a Gaussian distribution, 1/12/40 = 0.00208"),
                          verbatimTextOutput("unifvar")
                 )
                 
)
