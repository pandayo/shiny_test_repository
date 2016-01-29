library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("View Distributions"),
  column(3,
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
         fluidRow(actionButton(
           "draw.Plot","Draw distribution"
         ))),
  mainPanel(plotOutput('dist.Plot'))
))