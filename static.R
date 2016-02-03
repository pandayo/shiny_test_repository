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

hypothesis.crit.plot <- function(input) {
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
          p_val <-
            1 - pnorm(input$hypothesis.crit.value, mean = input$mu, sd = input$sigma);
          border <- c(
            qnorm(p_val, mean = input$mu, sd = input$sigma),qnorm(1 - p_val, mean = input$mu, sd = input$sigma)
          )
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dnorm, min = outputrange[1], max = border[1], mean = input$mu,
                  sd = input$sigma
                ), geom = 'area', fill = 'red', alpha = '0.2'
              ),
              stat_function(
                fun = limitRange(
                  dnorm, max = outputrange[2], min = border[2], mean = input$mu,
                  sd = input$sigma
                ), geom = 'area', fill = 'red', alpha = '0.2'
              )
            );
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
    },
    'Left-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <-
            qnorm(
              pnorm(
                input$hypothesis.crit.value, mean = input$mu, sd = input$sigma
              ), mean = input$mu, sd = input$sigma
            );
          sf <- stat_function(
            fun = limitRange(
              dnorm, min = outputrange[1], max = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'red', alpha = '0.2'
          );
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
      
    },
    'Right-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <-
            qnorm(
              pnorm(
                input$hypothesis.crit.value, mean = input$mu, sd = input$sigma
              ), mean = input$mu, sd = input$sigma
            );
          sf <- stat_function(
            fun = limitRange(
              dnorm, max = outputrange[2], min = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'red', alpha = '0.2'
          );
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
  );
  return(sf);
}

hypothesis.plot <- function(input) {
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
          border <- c(
            qnorm(
              (input$hypothesis.p.value / 2), mean = input$mu, sd = input$sigma
            ),qnorm(
              1 -
                (input$hypothesis.p.value / 2), mean = input$mu, sd = input$sigma
            )
          )
          sf <- c();
          sf <-
            cbind(
              sf, stat_function(
                fun = limitRange(
                  dnorm, min = outputrange[1], max = border[1], mean = input$mu,
                  sd = input$sigma
                ), geom = 'area', fill = 'blue', alpha = '0.2'
              ),
              stat_function(
                fun = limitRange(
                  dnorm, max = outputrange[2], min = border[2], mean = input$mu,
                  sd = input$sigma
                ), geom = 'area', fill = 'blue', alpha = '0.2'
              )
            );
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
    },
    'Left-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <-
            qnorm(input$hypothesis.p.value, mean = input$mu, sd = input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dnorm, min = outputrange[1], max = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2'
          );
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
      
    },
    'Right-Sided' = {
      return(switch(
        input$dist,
        'Normal distribution' = {
          border <-
            qnorm(1 - input$hypothesis.p.value, mean = input$mu, sd = input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dnorm, max = outputrange[2], min = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2'
          );
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
  );
  return(sf);
}