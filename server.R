library(shiny)
library(ggplot2)

if (!exists("allowed.Ranges", mode = "function"))
  source("static.R")

##############################################################################
#                                                                            #
#                             Default Values                                 #
#                                                                            #
##############################################################################

smoothing.points <- 1001;
server.dist <- 'Normal distribution'
server.draw.ranges <- c(0,0,0,0)

##############################################################################
#                                                                            #
#                               Shiny Server                                 #
#                                                                            #
##############################################################################

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
            "dist.range", "distribution limit", min = allowed.Ranges[1] + 1,
            max = allowed.Ranges[2] - 1,
            value = c(allowed.Ranges[3] + 1, allowed.Ranges[4] - 1)
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
  
  output$option.smoothing.points <- renderUI({
    if (input$dist != 'Binomial distribution')
      numericInput('n','Smoothing points',smoothing.points)
  })
  
  ##############################################################################
  #                                                                            #
  #                           Plotting Distr.                                  #
  #                                                                            #
  ##############################################################################
  
  output$dist.Plot <- renderPlot({
    if (input$draw.Plot) {
      op <- nplot();
    }else{
      op <-
        ggplot(data.frame(x = seq(-5,5,10 / smoothing.points)), aes(x)) + 
                    stat_function(fun = dnorm,geom = "line");
    }
    return(op);
  })
  
  # Plotting the distributions
  nplot <- eventReactive(input$draw.Plot, {
    if (is.null(input$draw.ranges)) {
      outputrange <- allowed.Ranges(input)[3:4];
    }else{
      outputrange <- input$draw.ranges;
    }
    if (is.null(input$geom)) {
      geom <- "line"
    }else{
      geom <- input$geom;
    }
    if (is.null(input$n)) {
      n <- smoothing.points;
    }else{
      n <- input$n;
    }
    outplot <-
      ggplot(data.plot <-
               data.frame(x = seq(
                 outputrange[1],outputrange[2],abs(outputrange[1] - outputrange[2]) / n
               )), aes(x));
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
            list(
              min = input$dist.range[1], max = input$dist.range[2]
            ),
          geom = geom,
          n = n
        )
      }
    );
    if (!is.na(input$hypothesis.crit.value)) {
      y <- hypothesis.crit.plot(input);
      outplot <- outplot + y;
    }
    if (!is.na(input$hypothesis.p.value)){
      y <- hypothesis.plot(input);
      outplot <- outplot + y;
    }
    return(outplot)
  })
  
  ##############################################################################
  #                                                                            #
  #                        Distribution Information                            #
  #                                                                            #
  ##############################################################################
  
  output$dist.Info <- renderUI({
    withMathJax(ninfo())
  })
  
  ninfo <- eventReactive(input$draw.Plot, {
    switch(
      input$dist,
      'Normal distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Log-normal distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Exponential distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Beta distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Binomial distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Chi-Square' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Poisson distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      't-distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'F-distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      },
      'Uniform distribution' = {
        HTML(
          paste(
            'The drawn distribution is the normal distribution. It is defined by it\'s mean, \\(\\mu\\), and it\'s variance, \\(\\sigma^2\\).','For the usage of newline equations, use this: $$\\sigma^2.$$', sep =
              "<br/>"
          )
        )
      }
    )
  })
  
  ##############################################################################
  #                                                                            #
  #                             Helptext Output                                #
  #                                                                            #
  ##############################################################################
  
  output$help.line <- renderUI({
    tags$div(class = 'bottom', checked = NA, 
      list(
        tags$div(
          class = 'shiny-text-output', checked = NA, paste(
          "This is your help text, it will change depending on the distribution you selected. At the moment, you selected the",input$dist,"."
        )),
        tags$div(
          class = 'shiny-text-output', checked = NA, 'dist.text(input$dist)'
        )
      )
    )
  })
})