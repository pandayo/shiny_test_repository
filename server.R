library(shiny)
library(ggplot2)

# Define the ranges, the different distributions are allowed to use
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
    },
    'Poisson distribution' = {
      c(0,100,0,5)
    },
    't-distribution' = {
      c(-50,50,-5,5)
    },
    'F-distribution' = {
      c(0,50,0,5)
    },
    'Uniform distribution' = {
      c(-50,50,-5,5)
    }
  ))
}

shinyServer(function(input, output) {
  #render the options by the distribution
  output$dist.options <- renderUI({
    allowed.Ranges <- allowed.Ranges(input);
    return(switch(
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
            inputId = 'rate',label = 'λ', value = 1
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
      },
      'Poisson distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'lambda',label = 'λ', value = 1
          )
        )
      },
      't-distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'df',label = 'Degrees of Freedom', value = 1
          )
        )
      },
      'F-distribution' = {
        list(
          helpText("Enter the parameters below:"),
          numericInput(
            inputId = 'df1',label = 'Degrees of Freedom No. 1', value = 10
          ),
          numericInput(
            inputId = 'df2',label = 'Degrees of Freedom No. 2', value = 5
          )
        )
      },
      'Uniform distribution' = {
        list(
          helpText("Enter the parameters below:"),
          sliderInput(
            "dist.range", "distribution limit", min = allowed.Ranges[1]+1,
            max = allowed.Ranges[2]-1,
            value = c(allowed.Ranges[3]+1, allowed.Ranges[4]-1)
          )
        )
      }
    ))
  })
  
  ##############################################################################
  #                                                                            #
  #                           Advanced Options                                 #
  #                                                                            #
  ##############################################################################
  
  output$option.range <- renderUI({
    allowed.Ranges <- allowed.Ranges(input);
    sI <-
      sliderInput(
        "draw.range", "x-axis length", min = allowed.Ranges[1],
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
  
  ##############################################################################
  #                                                                            #
  #                           Plotting Distr.                                  #
  #                                                                            #
  ##############################################################################
  
  output$dist.Plot <- renderPlot({
    nplot()
  })
  
  # Plotting the distributions
  nplot <- eventReactive(input$draw.Plot, {
    if (is.null(input$draw.range)) {
      outputrange <- allowed.Ranges(input)[3:4];
    }else{
      outputrange <- input$draw.range;
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
      },
      'Poisson distribution' = {
        outplot <- outplot + stat_function(
          fun = dpois, args =
            list(lambda = input$lambda),
          geom = geom,
          n = n
        )
      },
      't-distribution' = {
        outplot <- outplot + stat_function(
          fun = dt, args =
            list(df = input$df),
          geom = geom,
          n = n
        )
      },
      'F-distribution' = {
        outplot <- outplot + stat_function(
          fun = df, args =
            list(df1 = input$df1, df2 = input$df2),
          geom = geom,
          n = n
        )
      },
      'Uniform distribution' = {
        outplot <- outplot + stat_function(
          fun = dunif, args =
            list(min = input$dist.range[1], max = input$dist.range[2]),
          geom = geom,
          n = n
        )
      }
    );
    return(outplot)
  })
  
  ##############################################################################
  #                                                                            #
  #                           Hypothesis Testing                               #
  #                                                                            #
  ##############################################################################
  
  output$hypothesis.p <- renderUI({
    list(numericInput("hypothesis.p.value", "p value", 0.05, min = 0, max = 1),
         checkboxInput("draw.hypothesis.p.value", "Use the p value", FALSE)
         )
  })
  output$hypothesis.crit <- renderUI({list(
    numericInput("hypothesis.crit.value", "Critical Value", 1.96, step = 0.01),
    checkboxInput("draw.hypothesis.p.value", "Use the critical value", FALSE)
  )
  })
})