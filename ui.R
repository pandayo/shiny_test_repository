library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = "stylesheet.css",
  titlePanel("Distributions"),
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
               ))
    ),
    fluidRow(actionButton("draw.Plot","Draw distribution")),
    fluidRow()
  ),
  mainPanel(width = 9,tabsetPanel(
    tabPanel("Plot",
             plotOutput('dist.Plot')), tabPanel(
               "Hypothesis testing",
               checkboxInput("draw.Plot.Hypothesis",
                             "Show hypothesistesting", FALSE),
               uiOutput('hypothesis.p'),
               uiOutput('hypothesis.crit'),
               selectInput(
                 'test.type','Select the type of test',
                 c('Two-Sided', 'Left-Sided', 'Right-Sided')
               )
             )
  ))
))