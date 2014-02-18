library(shiny)
library(lattice)

shinyServer(function(input,output) {
    output$probstring <-
        renderText({
            paste(input$dist, "P(X < x) =",
                  format(switch(input$dist
                                ,Normal = pnorm(input$x,input$normmean,input$normsd)
                                ,Exponential = pexp(input$x, input$exprate)
                                ,Weibull = pweibull(input$x, input$weibshape, input$weibscale)
                                ,Uniform = punif(input$x,input$unifmin,input$unifmax)
                                ,Binomial = pbinom(input$x, input$ntrials,input$binomprob)
                                ,Poisson = ppois(input$x,input$poislambda)
                                )
                         )
                  )
        })
    output$densplot <- renderPlot({
        xv <- input$x
        switch(input$dist,
               Normal = {
                   mu <- input$normmean
                   sd <- input$normsd
                   xvals <- seq(mu - 5*sd, mu + 5*sd, length.out = 401)
                   dvals <- dnorm(xvals,mu,sd)
                   pr <- pnorm(xvals,mu,sd)
                   plcdf <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       xpr <- pnorm(xv,mu,sd)
                       lpoints(xv,xpr)
                       lsegments(xv,0,xv,xpr)
                   }
                   print(xyplot(pr ~ xvals,panel=plcdf,ylab="cumulative probability",xlab=NULL),
                         position = c(0,0.6,1,1), more = TRUE)
                   pldens <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       ind <- which(x < xv)
                       lpolygon(c(x[ind],xv), c(y[ind],0.), fill=TRUE, col="red", border="red")
                       ltext(x[40],0.85*max(y),bquote(bolditalic(X) %~% bolditalic(N)),adj=c(1,0.5))
                       ltext(x[40],0.85*max(y),paste("(",mu,",",sd,")"),adj=c(0,0.5))
                   }
                   print(xyplot(dvals ~ xvals,panel=pldens,ylab="density",xlab=NULL),
                         position = c(0,0,1,0.7))
               }
               ,Exponential = {
                   rate <- input$exprate
                   xvals <- seq(0, max(5/rate, 1.1*xv), length.out = 401)
                   dvals <- dexp(xvals,rate)
                   pr <- pexp(xvals,rate)
                   plcdf <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       xpr <- pexp(xv,rate)
                       lpoints(xv,xpr)
                       lsegments(xv,0,xv,xpr)
                   }
                   print(xyplot(pr ~ xvals,panel=plcdf,ylab="cumulative probability",xlab=NULL),
                         position = c(0,0.6,1,1), more = TRUE)
                   pldens <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       ind <- which(x < xv)
                       lpolygon(c(0,x[ind],xv), c(0,y[ind],0.), fill=TRUE, col="red", border="red")
                   }
                   print(xyplot(dvals ~ xvals,panel=pldens,ylab="density",xlab=NULL),
                         position = c(0,0,1,0.7))
               }
               ,Uniform = {
                   mn <- input$unifmin
                   mx <- input$unifmax
                   rng <- mx - mn
                   xvals <- seq(min(mn - 0.02*rng,xv), max(mx+0.02*rng, 1.02*xv), length.out = 401)
                   dvals <- dunif(xvals,mn,mx)
                   pr <- punif(xvals,mn,mx)
                   plcdf <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       xpr <- punif(xv,mn,mx)
                       lpoints(xv,xpr)
                       lsegments(xv,0,xv,xpr)
                   }
                   print(xyplot(pr ~ xvals,panel=plcdf,ylab="cumulative probability",xlab=NULL),
                         position = c(0,0.6,1,1), more = TRUE)
                   pldens <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       ind <- which(x < xv & x > mn)
                       if (length(ind) > 0) {
                           lpolygon(c(mn,mn,x[ind],xv), c(0,dunif(mn),y[ind],0.),
                                    fill=TRUE, col="red", border="red")
                       }
                   }
                   print(xyplot(dvals ~ xvals,panel=pldens,ylab="density",xlab=NULL),
                         position = c(0,0,1,0.7))
               }
               ,Poisson = {
                   lam <- input$poislambda
                   xvals <- seq(min(floor(xv),0),max(8, 2*lam,1.1*xv))
                   dvals <- c(0,dpois(xvals,lam))
                   pr <- c(0,ppois(xvals,lam))
                   xvals <- c(-1e-7,xvals)
                   plcdf <- function(x, y, ...) {
                       panel.lines(c(-0.001,0,x), c(0,0,y), type = "s", ...)
                       xpr <- ppois(xv,lam)
                       lpoints(xv,xpr)
                       lsegments(xv,0,xv,xpr)
                   }
                   print(xyplot(pr ~ xvals,panel=plcdf,ylab="cumulative probability",xlab=NULL),
                         position = c(0,0.6,1,1), more = TRUE)
                   pldens <- function(x, y, ...) {
                       panel.lines(x,y,type="h",...)
                       ind <- which(0 <= x & x <= xv)
                       xi <- x[ind]
                       panel.lines(x[ind], y[ind], type = "h", col = "red", lwd=3, ...)
                   }
                   print(xyplot(dvals ~ xvals,panel=pldens,ylab="probability mass",xlab=NULL),
                         position = c(0,0,1,0.7))
               }
               )
    })
})
