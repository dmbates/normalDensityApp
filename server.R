library(shiny)
library(lattice)

shinyServer(function(input,output) {
    output$probstring <-
        renderText({
            x <- input$x
            paste(input$dist, "P(X < x) =",
                  format(switch(input$dist
                                ,Binomial = pbinom(x, input$binomntrials,input$binomprob)
                                ,Exponential = pexp(x, input$exprate)
                                ,Geometric = pgeom(x, input$geomprob)
                                ,HyperGeometric = phyper(x, input$hyperm, input$hypern, input$hyperk)
                                ,NegativeBinomial = pnbinom(x, input$nbinomsize, input$nbinomprob)
                                ,Normal = pnorm(x,input$normmean,input$normsd)
                                ,Poisson = ppois(x,input$poislambda)
                                ,Uniform = punif(x,input$unifmin,input$unifmax)
                                ,Weibull = pweibull(x, input$weibshape, input$weibscale)
                                )
                         )
                  )
        })
    output$densplot <- renderPlot({
        xv <- input$x
        switch(input$dist
               ,Binomial = {
                   n <- input$binomntrials
                   pr <- input$binomprob
                   low <- min(floor(xv),qbinom(1e-4,n,pr))
                   lowl <- low - 0.001  # an x value slightly to the left of low
                   plowl <- pbinom(lowl,n,pr)
                   upp <- max(ceiling(xv),qbinom(1e-4,n,pr,lower=FALSE))
                   xvals <- low:upp
                   dvals <- dbinom(xvals,n,pr)
                   pvals <- pbinom(xvals,n,pr)
                   plcdf <- function(x, y, ...) {
                       panel.lines(c(lowl,low,x),c(plowl,plowl,y),type = "s",...)
                       xpr <- pbinom(xv,n,pr)
                       lpoints(xv,xpr)
                       lsegments(xv,0,xv,xpr)
                   }
                   print(xyplot(pvals ~ xvals, panel=plcdf, ylab="cumulative probability",xlab=NULL),
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
               ,Geometric = {
                   pr  <- input$geomprob
                   pxv <- pgeom(xv,pr)
                   low <- min(floor(xv),qgeom(1e-4,pr))
                   xvals <- low:max(ceiling(xv),qgeom(1e-4,pr,lower=FALSE))
                   dvals <- dgeom(xvals,pr)
                   xvp <- c(low-0.001,xvals)
                   pvals <- pgeom(xvp,pr)
                   plcdf <- function(x, y, ...) {
                       panel.lines(x,y,type = "s",...)
                       lpoints(xv,pxv)
                       lsegments(xv,0,xv,pxv)
                   }
                   print(xyplot(pvals ~ xvp, panel=plcdf, ylab="cumulative probability",xlab=NULL),
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
               ,Normal = {
                   mu <- input$normmean
                   sd <- input$normsd
                   xvals <- seq(min(xv,mu - 5*sd), max(xv,mu + 5*sd), length.out = 401)
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
               ,Poisson = {
                   lam <- input$poislambda
                   pxv <- ppois(xv,lam)
                   low <- min(floor(xv),qpois(1e-4,lam))
                   xvals <- low:max(ceiling(xv),qpois(1e-4,lam,lower=FALSE))
                   dvals <- dpois(xvals,lam)
                   xvp <- c(low-0.001,xvals)
                   pvals <- ppois(xvp,lam)
                   plcdf <- function(x, y, ...) {
                       panel.lines(x,y,type = "s", ...)
                       lpoints(xv,pxv)
                       lsegments(xv,0,xv,pxv)
                   }
                   print(xyplot(pvals ~ xvp,panel=plcdf,ylab="cumulative probability",xlab=NULL),
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
               ,Weibull = {
                   sh <- input$weibshape
                   sc <- input$weibscale
                   dxv <- dweibull(xv,sh,sc)
                   pxv <- pweibull(xv,sh,sc)
                   low <- min(0,xv,qweibull(1e-4,sh,sc))
                   upp <- max(xv,qweibull(1e-4,sh,sc,lower=FALSE))
                   xvals <- seq(low, upp, length.out = 401)
                   dvals <- dweibull(xvals,sh,sc)
                   pvals <- pweibull(xvals,sh,sc)
                   plcdf <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       lpoints(xv,pxv)
                       lsegments(xv,0,xv,pxv)
                   }
                   print(xyplot(pvals ~ xvals,panel=plcdf,ylab="cumulative probability",xlab=NULL),
                         position = c(0,0.6,1,1), more = TRUE)
                   pldens <- function(x, y, ...) {
                       panel.lines(x, y, ...)
                       ind <- which(x < xv)
                       if (length(ind) > 0) {
                           lpolygon(c(low,x[ind],xv), c(0,y[ind],0),
                                    fill=TRUE, col="red", border="red")
                       }
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
               )
    })
})
