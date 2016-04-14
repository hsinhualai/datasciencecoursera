library(shiny)

server <- function(input, output) {
        lambda <- 0.2
        Tvarnorm <- 1/(lambda^2)/40
        Tsdnorm <- sqrt(Tvarnorm)
        
        Tvarpois <- lambda/40
        Tsdpois <- sqrt(Tvarpois)
        
        Tvarunif <- 1/12/40
        Tsdunif <- sqrt(Tvarunif)
        rv <- reactiveValues(
                norm = reactive({
                        maxiter<- input$expnum
                        normdata <- lapply(1:maxiter, function(i)
                        {mean(rexp(40,lambda))
                        })
                        return (as.numeric(normdata))}), 
                
                pois = reactive({
                        maxiter<- input$poisnum
                        poisdata <- lapply(1:maxiter, function(i)
                        {mean(rpois(40,lambda))
                        })
                        return (as.numeric(poisdata))}),
                
                unif = reactive({
                        maxiter<- input$unifnum
                        unifdata <- lapply(1:maxiter, function(i)
                        {mean(runif(40))
                        })
                        return (as.numeric(unifdata))}))
        
        gx <- reactiveValues(
                xtnorm = reactive({seq(1/lambda - 4 *Tsdnorm, 1/lambda + 4 * Tsdnorm,
                                       length = input$expnum)}),
                xtpois =reactive({seq(lambda - 4 *Tsdpois,
                                      lambda + 4 * Tsdpois,
                                      length = input$poisnum)}),
                xtunif = reactive({seq(1/2 - 4 *Tsdunif, 1/2 + 4 * Tsdunif,
                                       length = input$unifnum)})
        )
        
        output$norm <- renderPlot({
                hist(rv$norm(),breaks=20, col = "grey", border = "white",
                     xlab ="Means of rexp",
                     ylab ="Counts",
                     main = "Histogrm of Means of rexp")
                
                ytheo <- dnorm(gx$xtnorm(), mean = 1/lambda, sd = Tsdnorm)
                ytheo <- ytheo*
                        diff(hist(rv$norm(),breaks=20, col = "grey",
                                  border="white",
                                  xlab ="Means of rexp",
                                  ylab ="Counts",
                                  main = "Histogrm of Means of rexp")$mids[1:2])*
                        length(gx$xtnorm())
                
                lines(c(5,5), c(0,10000),col="red", lwd=3)
                
                lines(c(mean(rv$norm()),mean(rv$norm())),c(0,10000),
                      col = "blue", lwd = 3)
                
                lines(gx$xtnorm(), ytheo, col = "green", lwd = 3)
        })
        
        output$mnorm <- renderPrint({
                mean(rv$norm())
        })
        output$normvar <- renderPrint({
                var(rv$norm())
        })
        
        output$pois <- renderPlot({
                hist(rv$pois(), col = "grey", border = "white",
                     xlab ="Means of rpois",
                     ylab ="Counts",
                     main = "Histogrm of Means of rpois")
                
                ytpois <- dnorm(gx$xtpois(), mean = lambda, sd = Tsdpois)
                ytpois <- ytpois*diff(hist(rv$pois(), col = "grey",
                                           border="white",
                                           xlab ="Means of rpois",
                                           ylab ="Counts",
                                           main = "Histogrm of Means of rpois"
                )$mids[1:2])*length(gx$xtpois())
                
                lines(c(0.2,0.2), c(0,10000),col="red", lwd=3)
                
                lines(c(mean(rv$pois()),mean(rv$pois())),c(0,10000),
                      col = "blue", lwd = 3)
                
                lines(gx$xtpois(), ytpois, col = "green", lwd = 3)
        })
        
        output$poismean <- renderPrint({
                mean(rv$pois())
        })
        output$poisvar <- renderPrint({
                var(rv$pois())
        })
        
        output$unif <- renderPlot({
                hist(rv$unif(), breaks = 20, col = "grey", border = "white",
                     xlab ="Means of runif",
                     ylab ="Counts",
                     main = "Histogrm of Means of runif")
                
                ytunif <- dnorm(gx$xtunif(), mean = 1/2, sd = Tsdunif)
                ytunif <- ytunif*diff(hist(rv$unif(), breaks=20, col = "grey",
                                           border="white",
                                           xlab ="Means of runif",
                                           ylab ="Counts",
                                           main = "Histogrm of Means of runif"
                )$mids[1:2])*length(gx$xtunif())
                lines(c(0.5,0.5), c(0,10000),col="red", lwd=3)
                
                lines(c(mean(rv$unif()),mean(rv$unif())),c(0,10000),
                      col = "blue", lwd = 3)
                
                lines(gx$xtunif(), ytunif, col = "green", lwd = 3)
        })
        output$unifmean <- renderPrint({
                mean(rv$unif())
        })
        
        output$unifvar <- renderPrint({
                var(rv$unif())
        })
        
}