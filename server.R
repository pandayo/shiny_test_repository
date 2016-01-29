library(shiny)
library(ggplot2)


allowed.Ranges <- function(input) {
  return(switch(
    input$dist,
    'Normal distribution' = {
      c(-50,50,-5,5)
    },
    'Log-normal distribution' = {
      c(-1,100,0,5)
    },
    'Exponential distribution' = {
      c(-1,100,0,5)
    },
    'Beta distribution' = {
      c(-50,50,-5,5)
    },
    'Binomial distribution' = {
      c(0,100,0,input$size)
    },
    'Chi-Square' = {
      c(0,100,0,5)
    }
  ))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distoptions <- renderUI({
    switch(
      input$dist,
      'Normal distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'mu',label = 'μ', value = 0
          ),
          numericInput(
            inputId = 'sigma', label = 'σ', value = 1
          )
        )
      },
      'Log-normal distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'mu',label = 'μ', value = 0
          ),
          numericInput(
            inputId = 'sigma', label = 'σ', value = 1
          )
        )
      },
      'Exponential distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'rate',label = 'rate', value = 1
          )
        )
      },
      'Beta distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'shape1',label = 'p', value = 1
          ),
          numericInput(
            inputId = 'shape2',label = 'q', value = 1
          )
        )
      },
      'Binomial distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'size',label = 'Numbers of trials', value = 10
          ),
          numericInput(
            inputId = 'prob',label = 'p', value = 0.5
          )
        )
      },
      'Chi-Square' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'df',label = 'Degrees of Freedom', value = 1
          )
        )
      }
    )
  })
  
  output$option.range <- renderUI({
    allowed.Ranges <- allowed.Ranges(input);
    sI <-
      sliderInput(
        "range", "x-axis length", min = allowed.Ranges[1],
        max = allowed.Ranges[2],
        value = c(allowed.Ranges[3], allowed.Ranges[4])
      );
    return(sI);
  })
  
  output$option.geom <- renderUI({
    selectInput('geom','Stuff', c('line','point','bar'))
  })
  
  output$option.sp <- renderUI({
    if (input$dist != 'Binomial distribution')
      numericInput('n','Smoothing points',101)
  })
  
  output$distPlot <- renderPlot({
    nplot()
  })
  
  nplot <- eventReactive(input$draw, {
    if (is.null(input$range)) {
      outputrange <- allowed.Ranges(input)[3:4];
    }else{
      outputrange <- input$range;
    }
    if (is.null(input$geom)) {
      geom <- "line"
    }else{
      geom <- input$geom;
    }
    if (is.null(input$n)) {
      n <- ceiling(abs(outputrange[2] - outputrange[1]) * 10);
    }else{
      n <- input$n;
    }
    outplot <-
      ggplot(data.frame(x = c(outputrange[1],outputrange[2])), aes(x));
    switch(
      input$dist,
      'Normal distribution' = {
        outplot <- outplot +
          stat_function(
            fun = dnorm, args =
              list(mean = input$mu, sd = input$sigma),
            geom = geom,
            n = n
          )
      },
      'Log-normal distribution' = {
        outplot <- outplot +
          stat_function(
            fun = dlnorm, args =
              list(mean = input$mu, sd = sqrt(input$sigma)),
            geom = geom,
            n = n
          )
      },
      'Exponential distribution' = {
        outplot <- outplot +
          stat_function(
            fun = dexp, args =
              list(rate = input$rate),
            geom = geom,
            n = n
          )
      },
      'Beta distribution' = {
        outplot <- outplot +
          stat_function(
            fun = dbeta, args =
              list(
                shape1 = input$shape1,shape2 = input$shape2,
                ncp = 0
              ),
            geom = geom,
            n = n
          )
      },
      'Binomial distribution' = {
        outplot <- outplot +
          stat_function(
            fun = dbinom, args =
              list(size = input$size, prob = input$prob),
            n = abs(outputrange[2] - outputrange[1]) +
              1,
            geom = "bar"
          )
      },
      'Chi-Square' = {
        outplot <- outplot + stat_function(
          fun = dchisq, args =
            list(df = input$df),
          geom = geom,
          n = n
        )
      }
    );
    return(outplot)
  })
})