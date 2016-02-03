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
        tabPanel(
          "Hypothesis testing",
          selectInput(
            'test.type','Select the type of test',
            c('Two-Sided', 'Left-Sided', 'Right-Sided')
          ),
          numericInput(
            "hypothesis.p.value", "p value", NA, min = 0, max = 1, step = 0.01
          ),
          numericInput("hypothesis.crit.value", "Critical Value", NA, step = 0.01)
        ),
        tabPanel("Options",
                 fluidRow(
                   uiOutput('option.range'),
                   uiOutput('option.geom'),
                   uiOutput('option.smoothing.points')
                 ))
      ),
      fluidRow(actionButton("draw.Plot","Draw distribution"))
    ),
    mainPanel(width = 9,
              plotOutput('dist.Plot'),
              br(),
              if (!is.null('dist.Plot')) {
                uiOutput('dist.Info')
              }),
    br(),
    htmlOutput('help.line'))
  )