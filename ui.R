library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    theme = "stylesheet.css",
    withMathJax(),
    column(width = 12,
           column(width = 10, titlePanel("Distributions"))),
    column(width = 11, br()),
    column(
      width = 3,
      tabsetPanel(
        tabPanel(
          "General",
          fluidRow(
            helpText("Select a Distribution from below"),
            selectInput(
              'dist','Distribution',
              c(
                'Normal distribution','Log-normal distribution',
                'Exponential distribution','Beta distribution',
                'Binomial distribution','Chi-Square','Poisson distribution',
                't-distribution','F-distribution','Uniform distribution'
              )
            )
          ),
          fluidRow(uiOutput('dist.options'))
        ),
        tabPanel("Options",
                 fluidRow(
                   uiOutput('option.range'),
                   uiOutput('option.geom'),
                   uiOutput('option.sp')
                 )),
        tabPanel(
          "Hypothesis testing",
          checkboxInput("draw.Plot.Hypothesis",
                        "Show hypothesistesting", value = FALSE),
          numericInput(
            "hypothesis.p.value", "p value", 0.05, min = 0, max = 1, step = 0.01
          ),
          selectInput(
            'test.type','Select the type of test',
            c('Two-Sided', 'Left-Sided', 'Right-Sided')
          ),
          numericInput("hypothesis.crit.value", "Critical Value", 1.96, step = 0.01),
          actionButton("get.P.value","Calculate p value"),
          verbatimTextOutput('hypothesis.crit.to.p')
        )
      ),
      fluidRow(actionButton("draw.Plot","Draw distribution"))
    ),
    mainPanel(width = 9,
              plotOutput('dist.Plot'),
              br(),
              uiOutput('dist.Info'))
    
  )
)