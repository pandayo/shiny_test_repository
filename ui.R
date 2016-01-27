library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    titlePanel("View Distributions"),
    column(3,
      tabsetPanel(
        tabPanel("General",
          fluidRow(
            helpText("Select a Distribution from below"),
            selectInput('dist','Distribution',
                        c('Normal distribution','Log-normal distribution',
                          'Exponential distribution'))),
          fluidRow(
            uiOutput('options'))),
        tabPanel("Options",
          fluidRow(
            sliderInput("range", "",
                        min = -50, max = 50, value = c(-25, 25))
          )
        )
      )
    ),
    mainPanel(
      plotOutput('distplot')
    )
  )
)