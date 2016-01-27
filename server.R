library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #output$plot <- renderPlot()
  output$options <- renderUI(
    switch(input$dist,
     'Normal distribution'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'mu',label = 'μ', value = 0),
       numericInput(inputId = 'sigma', label = 'σ²', value = 1)
      )},
     'Log-normal distribution'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'mu',label = 'μ', value = 0),
       numericInput(inputId = 'sigma', label = 'σ²', value = 1)
     )},
     'Exponential distribution'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'rate',label = 'rate', value = 1)
     )}
    )
  )
  
  output$distplot <- renderPlot(
    switch(input$dist,
           'Normal distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dnorm, args = 
                               list(mean = input$mu, sd = sqrt(input$sigma)))
           },
           'Log-normal distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dlnorm, args = 
                               list(mean = input$mu, sd = sqrt(input$sigma)))
           },
           'Exponential distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dexp, args = 
                               list(rate = input$rate))
           })
  )
})