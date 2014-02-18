library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Probability Calculator"),
    sidebarPanel(
        numericInput("x","x",1)
        ,br()
        ,tabsetPanel(
            tabPanel("Normal"
                     ,numericInput("normmean","mean",0)
                     ,numericInput("normsd","standard deviation",1)
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
                      ,numericInput("ntrials","number of trials",10)
                      ,numericInput("binomprob","probability of success",0.5)
                      )
            ,tabPanel("Poisson"
                      ,numericInput("poislambda","mean",1)
                      )
            , id = "dist"
            , position = "left"
            )
        )
    ,mainPanel(
        verbatimTextOutput("probstring")
        ,plotOutput("densplot")        
        )
    )
)
