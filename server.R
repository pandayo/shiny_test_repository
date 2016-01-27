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
       numericInput(inputId = 'sigma', label = 'σ', value = 1)
     )},
     'Exponential distribution'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'rate',label = 'rate', value = 1)
     )},
     'Beta distribution'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'shape1',label = 'p', value = 1),
       numericInput(inputId = 'shape2',label = 'q', value = 1)
     )},
     'Binomial distribution'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'size',label = 'Numbers of trials', value = 10),
       numericInput(inputId = 'prob',label = 'p', value = 0.5)
     )},
     'Chi-Square'={list(
       helpText("Enter the parameters below:"),
       numericInput(inputId = 'df',label = 'Degrees of Freedom', value = 1)
     )}
    )
  )
  
  output$distplot <- renderPlot(
    switch(input$dist,
           'Normal distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dnorm, args = 
                               list(mean = input$mu, sd = input$sigma),
                             geom = input$geom,
                             n = input$n)
           },
           'Log-normal distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dlnorm, args = 
                               list(mean = input$mu, sd = sqrt(input$sigma)),
                             geom = input$geom,
                             n = input$n)
           },
           'Exponential distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dexp, args = 
                               list(rate = input$rate),
                             geom = input$geom,
                             n = input$n)
           },
           'Beta distribution'={
             ggplot(data.frame(x=seq(input$range[1],input$range[2]),1), aes(x)) + 
               stat_function(fun = dbeta, args = 
                               list(shape1 = input$shape1,shape2 = input$shape2,
                                    ncp = 0),
                             geom = input$geom,
                             n = input$n)
           },
           'Binomial distribution'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dbinom, args = 
                               list(size = input$size, prob = input$prob), 
                             n = abs(input$range[2]-input$range[1])+1,
                             geom = "bar")
           },
           'Chi-Square'={
             ggplot(data.frame(x=c(input$range[1],input$range[2])), aes(x)) + 
               stat_function(fun = dchisq, args = 
                               list(df = input$df),
                             geom = input$geom,
                             n = input$n)
           })
  )
})