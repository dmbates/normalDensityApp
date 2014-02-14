library(shiny)
library(lattice)

shinyServer(function(input,output) {
    output$probstring <-
        renderText({paste("P(X < x) = ",
                          format(pnorm(input$x,input$mean,input$sd))
                          )
                })
    output$plot <- renderPlot({
        mu <- input$mean
        sd <- input$sd
        xv <- input$x
        xvals <- seq(mu - 5*sd, mu + 5*sd, length.out = 401)
        dvals <- dnorm(xvals,mu,sd)
        pl <- function(x, y, ...) {
            panel.lines(x, y, ...)
            ind <- which(x < xv)
            lpolygon(c(x[ind],xv), c(y[ind],0.), fill=TRUE, col="red", border="red")
            ltext(x[40],0.85*max(y),bquote(bolditalic(X) %~% bolditalic(N)),adj=c(1,0.5))
            ltext(x[40],0.85*max(y),paste("(",mu,",",sd,")"),adj=c(0,0.5))
        }
        print(xyplot(dvals ~ xvals,panel=pl,ylab="density",xlab=NULL))
    })
})
