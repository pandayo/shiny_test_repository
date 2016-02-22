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

limitRange <- function(fun, min, max, ...) {
  function(x) {
    y <- fun(x, ...)
    y[x < min | x > max] <- NA
    return(y)
  }
}

border.norm <- function(alpha, mean, sd){
  return(qnorm(
      alpha, mean = mean, sd = sd
    ))
}

border.lnorm <- function(alpha, meanlog, sdlog){
  return(qlnorm(
    alpha, meanlog = meanlog, sdlog = sdlog
  ))
}

border.exp <- function(alpha, rate){
  return(qexp(alpha, rate = rate))
}

border.beta <- function(alpha, shape1, shape2){
  return(qbeta(alpha, shape1 = shape1, shape2 = shape2))
}

hypothesis.plot <- function(input,smoothing.points) {
  n <- smoothing.points;
  if (is.null(input$draw.range)) {
    outputrange <- allowed.Ranges(input)[3:4];
  }else{
    outputrange <- input$draw.range;
  }
  switch(
    input$test.type,
    'Two-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <- c(border.norm(input$hypothesis.los.value/2, input$mu, input$sigma),
                      border.norm(1-input$hypothesis.los.value/2, input$mu, input$sigma));
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dnorm, min = outputrange[1], max = border[1], mean = input$mu,
                  sd = input$sigma
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dnorm, max = outputrange[2], min = border[2], mean = input$mu,
                  sd = input$sigma
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Log-normal distribution' = {
          border <- c(border.lnorm(input$hypothesis.los.value/2, input$mu, input$sigma),
                      border.lnorm(1-input$hypothesis.los.value/2, input$mu, input$sigma));
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dlnorm, min = outputrange[1], max = border[1], meanlog = input$mu,
                  sdlog = input$sigma
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dlnorm, max = outputrange[2], min = border[2], meanlog = input$mu,
                  sdlog = input$sigma
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Exponential distribution' = {
          border <-
            c(border.exp(input$hypothesis.los.value/2, input$rate),
              border.exp(1-input$hypothesis.los.value/2, input$rate))
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dexp, min = outputrange[1], max = border[1], rate = input$rate
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dexp, max = outputrange[2], min = border[2], rate = input$rate
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Beta distribution' = {
          border <-
            c(border.beta((input$hypothesis.los.value / 2), shape1 = input$shape1, shape2 = input$shape2
              ),
              border.beta(
                1 - (input$hypothesis.los.value / 2), shape1 = input$shape1, shape2 = input$shape2
              )
            )
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dbeta, min = outputrange[1], max = border[1], shape1 = input$shape1, shape2 = input$shape2
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dbeta, max = outputrange[2], min = border[2], shape1 = input$shape1, shape2 = input$shape2
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Binomial distribution' = {
          border <-
            c(
              qbinom((input$hypothesis.los.value / 2), size = input$size, prob = input$prob
              ),
              qbinom(
                1 - (input$hypothesis.los.value / 2), size = input$size, prob = input$prob
              )
            )
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dbinom, min = outputrange[1], max = border[1], size = input$size, prob = input$prob
                ), geom = 'bar', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dbinom, max = outputrange[2], min = border[2], size = input$size, prob = input$prob
                ), geom = 'bar', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Chi-Square' = {
          border <- c(qchisq((input$hypothesis.los.value / 2), df = input$df),
                      qchisq(1 - (input$hypothesis.los.value / 2), df = input$df))
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dchisq, min = outputrange[1], max = border[1], df = input$df
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dchisq, max = outputrange[2], min = border[2], df = input$df
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Poisson distribution' = {
          border <- c(qpois((input$hypothesis.los.value / 2), lambda = input$lambda),
                      qpois(1 - (input$hypothesis.los.value / 2), lambda = input$lambda))
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dpois, min = outputrange[1], max = border[1], lambda = input$lambda
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dpois, max = outputrange[2], min = border[2], lambda = input$lambda
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        't-distribution' = {
          border <- c(qt((input$hypothesis.los.value / 2), df = input$df), 
                      qt(1 - (input$hypothesis.los.value / 2), df = input$df))
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dt, min = outputrange[1], max = border[1], df = input$df
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dt, max = outputrange[2], min = border[2], df = input$df
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'F-distribution' = {
          border <- c(qf((input$hypothesis.los.value / 2), df1 = input$df1, df2 = input$df2
          ),
          qf(
            1 -
              (input$hypothesis.los.value / 2), df1 = input$df1, df2 = input$df2
          ))
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  df, min = outputrange[1], max = border[1], df1 = input$df1,
                  df2 = input$df2
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  df, max = outputrange[2], min = border[2], df1 = input$df1,
                  df2 = input$df2
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        },
        'Uniform distribution' = {
          border <- c(
            qunif((input$hypothesis.los.value / 2), min = input$dist.range[1],
                  max = input$dist.range[2]
            ),
            qunif(
              1 -
                (input$hypothesis.los.value / 2), min = input$dist.range[1],
              max = input$dist.range[2]
            )
          )
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dunif, min = outputrange[1], max = border[1],
                  min = input$dist.range[1], max = input$dist.range[2]
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
              ),
              stat_function(
                fun = limitRange(
                  dunif, max = outputrange[2], min = border[2],
                  min = input$dist.range[1], max = input$dist.range[2]
                ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
              )
            );
        }
      ))
    },
    'Left-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <- border.norm(input$hypothesis.los.value, input$mu, input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dnorm, min = outputrange[1], max = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Log-normal distribution' = {
          border <-
            border.lnorm(input$hypothesis.los.value, input$mu, input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dlnorm, min = outputrange[1], max = border, meanlog = input$mu,
              sdlog = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Exponential distribution' = {
          border <-
            border.exp(input$hypothesis.los.value, input$rate)
          sf <-
            stat_function(
              fun = limitRange(
                dexp, min = outputrange[1], max = border, rate = input$rate
              ), geom = 'area', fill = 'blue', alpha = 0.2,  n = n
            );
        },
        'Beta distribution' = {
          border <- border.beta(input$hypothesis.los.value, 
                                shape1 = input$shape1, 
                                shape2 = input$shape2
                                )
          sf <- stat_function(
            fun = limitRange(
              dbeta, min = outputrange[1], max = border,
              shape1 = input$shape1, shape2 = input$shape2
            ), geom = 'area', fill = 'blue', alpha = 0.2,  n = n
          );
        },
        'Binomial distribution' = {
          border <-
            qbinom((input$hypothesis.los.value), size = input$size, prob = input$prob)
          sf <- stat_function(
            fun = limitRange(
              dbinom, min = outputrange[1], max = border, size = input$size, prob = input$prob
            ), geom = 'bar', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Chi-Square' = {
          border <- qchisq(input$hypothesis.los.value, df = input$df)
          sf <- stat_function(
            fun = limitRange(
              dchisq, min = outputrange[1], max = border, df = input$df
            ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Poisson distribution' = {
          border <- qpois(input$hypothesis.los.value, lambda = input$lambda)
          sf <- stat_function(
            fun = limitRange(
              dpois, min = outputrange[1],
              max = border, lambda = input$lambda
            ),
            geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        't-distribution' = {
          border <- qt(input$hypothesis.los.value, df = input$df)
          sf <- stat_function(
            fun = limitRange(
              dt, min = outputrange[1],
              max = border, df = input$df
            ),
            geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'F-distribution' = {
          border <- qf(input$hypothesis.los.value, df1 = input$df1,
                       df2 = input$df2)
          sf <- stat_function(
            fun = limitRange(
              df, min = outputrange[1],
              max = border, df1 = input$df1, df2 = input$df2
            ),
            geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Uniform distribution' = {
          border <-
            qunif(
              input$hypothesis.los.value, min = input$dist.range[1],
              max = input$dist.range[2]
            )
          sf <- stat_function(
            fun = limitRange(
              dunif, min = outputrange[1],
              max = border, min = input$dist.range[1],
              max = input$dist.range[2]
            ),
            geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        }
      ))
      
    },
    'Right-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <-
            border.norm(1-input$hypothesis.los.value, input$mu, input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dnorm, max = outputrange[2], min = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Log-normal distribution' = {
          border <-
            border.lnorm(1-input$hypothesis.los.value, input$mu, input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dlnorm, max = outputrange[2], min = border, meanlog = input$mu,
              sdlog = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Exponential distribution' = {
          border <-
            border.exp(1-input$hypothesis.los.value, input$rate)
          sf <-
            stat_function(
              fun = limitRange(
                dexp, max = outputrange[2], min = border, rate = input$rate
              ), geom = 'area', fill = 'blue', alpha = 0.2,  n = n
            );
        },
        'Beta distribution' = {
          border <- border.beta(1-input$hypothesis.los.value, 
                                shape1 = input$shape1, 
                                shape2 = input$shape2
          )
          sf <- stat_function(
            fun = limitRange(
              dbeta, max = outputrange[2], min = border,
              shape1 = input$shape1, shape2 = input$shape2
            ), geom = 'area', fill = 'blue', alpha = 0.2,  n = n
          );
        },
        'Binomial distribution' = {
          border <-
            qbinom((1 - input$hypothesis.los.value), size = input$size, prob = input$prob)
          sf <- stat_function(
            fun = limitRange(
              dbinom, max = outputrange[2], min = border, size = input$size, prob = input$prob
            ), geom = 'bar', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Chi-Square' = {
          border <- qchisq(1 - (input$hypothesis.los.value), df = input$df)
          sf <- stat_function(
            fun = limitRange(
              dchisq, max = outputrange[2], min = border, df = input$df
            ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
          );
        },
        'Poisson distribution' = {
          border <-
            qpois(1 - input$hypothesis.los.value, lambda = input$lambda)
          sf <-
            stat_function(
              fun = limitRange(
                dpois, max = outputrange[2],
                min = border, lambda = input$lambda
              ),
              geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
            );
        },
        't-distribution' = {
          border <- qt(1 - input$hypothesis.los.value, df = input$df)
          sf <-
            stat_function(
              fun = limitRange(
                dt, max = outputrange[2],
                min = border, df = input$df
              ),
              geom = 'area', fill = 'blue',
              alpha = 0.2 / 9, n = n
            );
        },
        'F-distribution' = {
          border <- qf(1 - input$hypothesis.los.value, df1 = input$df1,
                       df2 = input$df2)
          sf <-
            stat_function(
              fun = limitRange(
                df, max = outputrange[2],
                min = border, df1 = input$df1,
                df2 = input$df2
              ),
              geom = 'area', fill = 'blue',
              alpha = 0.2 / 9, n = n
            );
        },
        'Uniform distribution' = {
          border <- qunif(
            1 - input$hypothesis.los.value,
            min = input$dist.range[1], max = input$dist.range[2]
          )
          sf <-
            stat_function(
              fun = limitRange(
                dunif, max = outputrange[2],
                min = border, min = input$dist.range[1],
                max = input$dist.range[2]
              ),
              geom = 'area', fill = 'blue',
              alpha = 0.2 / 9, n = n
            );
        }
      ))
    }
  );
  return(sf);
}

crit.value.calculator <- function(input) {
  switch(
    input$dist,
    'Normal distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(border.norm(alpha = input$hypothesis.los.value/2,
                                           mean = input$mu, sd = input$sigma),
                               "on the left side and",
                               border.norm(alpha = 1-input$hypothesis.los.value/2,
                                           mean = input$mu, sd = input$sigma),
                               "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(border.norm(alpha = input$hypothesis.los.value,
                                           mean = input$mu, sd = input$sigma),
                               "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(border.norm(alpha = 1-input$hypothesis.los.value,
                                           mean = input$mu, sd = input$sigma),
                               "on the right side");
        }
      )
    },
    'Log-normal distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(border.lnorm(input$hypothesis.los.value/2,
                                            input$mu, input$sigma),
                               "on the left side and",
                               border.lnorm(1-input$hypothesis.los.value/2,
                                            input$mu, input$sigma),
                               "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(border.lnorm(input$hypothesis.los.value, input$mu,
                                            input$sigma),
                               "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(border.lnorm(1-input$hypothesis.los.value,input$mu,
                                            input$sigma),
                               "on the right side");
        }
      )
    },
    'Exponential distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(border.exp(input$hypothesis.los.value/2,
                                            input$rate),
                               "on the left side and",
                               border.exp(1-input$hypothesis.los.value/2,
                                            input$rate),
                               "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(border.exp(input$hypothesis.los.value,
                                          input$rate),
                               "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(border.lnorm(1-input$hypothesis.los.value,input$rate),
                               "on the right side");
        }
      )
    },
    'Beta distribution' = {
      switch(
      input$test.type,
      'Two-Sided'={
        output.text <- paste(
          border.beta(
            (input$hypothesis.los.value / 2), 
            shape1 = input$shape1, 
            shape2 = input$shape2
          ), "on the left side and",
          border.beta(
            1 - (input$hypothesis.los.value / 2), 
            shape1 = input$shape1, 
            shape2 = input$shape2
          ), "on the right side.");
      },
      'Left-Sided'={
        output.text <- paste(border.beta(
          input$hypothesis.los.value, 
          shape1 = input$shape1, 
          shape2 = input$shape2
        ), "on the left side");
      },
      'Right-Sided'={
        output.text <- paste(border.beta(
          1 - input$hypothesis.los.value, 
          shape1 = input$shape1, 
          shape2 = input$shape2
        ), "on the right side.");
      });
    },
    'Binomial distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(
            qbinom(
              (input$hypothesis.los.value / 2), 
              size = input$size, prob = input$prob
            ), "on the left side and",
            qbinom(
              1 - (input$hypothesis.los.value / 2), 
              size = input$size, prob = input$prob
            ), "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(qbinom(
            input$hypothesis.los.value, 
            size = input$size, prob = input$prob
          ), "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(qbinom(
            1 - input$hypothesis.los.value, 
            size = input$size, prob = input$prob
          ), "on the right side.");
        });
    },
    'Chi-Square' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(
            qchisq(
              (input$hypothesis.los.value / 2), 
              df = input$df
            ), "on the left side and",
            qchisq(
              1-(input$hypothesis.los.value / 2), 
              df = input$df
            ), "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(qchisq(
            input$hypothesis.los.value, 
            df = input$df
          ), "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(qchisq(
            1 - input$hypothesis.los.value, 
            df = input$df
          ), "on the right side.");
        });
    },
    'Poisson distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(
            qpois(
              (input$hypothesis.los.value / 2), 
              lambda = input$lambda
            ), "on the left side and",
            qpois(
              1 - (input$hypothesis.los.value / 2), 
              lambda = input$lambda
            ), "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(qpois(
            input$hypothesis.los.value, 
            lambda = input$lambda
          ), "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(qpois(
            1 - input$hypothesis.los.value, 
            lambda = input$lambda
          ), "on the right side.");
        });
    },
    't-distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(
            qt(
              (input$hypothesis.los.value / 2), 
              df = input$df
            ), "on the left side and",
            qt(
              1 - (input$hypothesis.los.value / 2), 
              df = input$df
            ), "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(qt(
            input$hypothesis.los.value, 
            df = input$df
          ), "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(qt(
            1 - input$hypothesis.los.value, 
            df = input$df
          ), "on the right side.");
        });
    },
    'F-distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(
            qf(
              (input$hypothesis.los.value / 2), 
              df1 = input$df1, df2 = input$df2
            ), "on the left side and",
            qf(
              1 - (input$hypothesis.los.value / 2), 
              df1 = input$df1, df2 = input$df2
            ), "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(qf(
            input$hypothesis.los.value, 
            df1 = input$df1, df2 = input$df2
          ), "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(qf(
            1 - input$hypothesis.los.value, 
            df1 = input$df1, df2 = input$df2
          ), "on the right side.");
        });
    },
    'Uniform distribution' = {
      switch(
        input$test.type,
        'Two-Sided'={
          output.text <- paste(
            qunif(
              (input$hypothesis.los.value / 2), 
              min = input$dist.range[1],
              max = input$dist.range[2]
            ), "on the left side and",
            qunif(
              1 - (input$hypothesis.los.value / 2), 
              min = input$dist.range[1],
              max = input$dist.range[2]
            ), "on the right side.");
        },
        'Left-Sided'={
          output.text <- paste(qunif(
            input$hypothesis.los.value, 
            min = input$dist.range[1],
            max = input$dist.range[2]
          ), "on the left side");
        },
        'Right-Sided'={
          output.text <- paste(qunif(
            1 - input$hypothesis.los.value, 
            min = input$dist.range[1],
            max = input$dist.range[2]
          ), "on the right side.");
        });
    }
  )
  return(output.text)
}