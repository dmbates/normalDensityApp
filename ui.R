library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Probability Calculator"),
    sidebarPanel(
        numericInput("x","x",1)
        ,br()
        ,tabsetPanel(
            tabPanel("Normal"
                     ,numericInput("normmean","mean, μ",0)
                     ,numericInput("normsd","standard deviation, σ",1)
                     )
            ,tabPanel("Exponential"
                      ,numericInput("exprate","rate",1)
                      )
            ,tabPanel("Weibull"
                      ,numericInput("weibshape","shape",1)
                      ,numericInput("weibscale","scale",1)
                      )
            ,tabPanel("Uniform"
                      ,numericInput("unifmin","lower",0)
                      ,numericInput("unifmax","upper",1)
                      )
            ,tabPanel("Binomial"
                      ,numericInput("binomntrials","number of trials",10)
                      ,numericInput("binomprob","probability of success",0.5)
                      )
            ,tabPanel("Geometric"
                      ,numericInput("geomprob","probability of success",0.5)
                      )
            ,tabPanel("Poisson"
                      ,numericInput("poislambda","mean, λ",1)
                      )
            , id = "dist"
            , position = "left"
            )
        )
    ,mainPanel(
        textOutput("probstring")
        ,plotOutput("densplot")        
        )
    )
)
