library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Gaussian (normal) cumulative probability"),
    sidebarPanel(
        numericInput("mean","Distribution mean:",100),
        numericInput("sd","Standard deviation:",20),
        numericInput("x","x:",105)
        ),
    mainPanel(
        verbatimTextOutput("probstring"),
        plotOutput("plot")
        )
    )
)

        
