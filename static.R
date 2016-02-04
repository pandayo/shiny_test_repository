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

hypothesis.plot <- function(input,smoothing.points) {
  if (is.null(input$n)) {
    n <- smoothing.points;
  }else{
    n <- input$n;
  }
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
          border <- c(
            qlnorm(
              (input$hypothesis.p.value / 2), meanlog = input$mu, sdlog = input$sigma
            ),qlnorm(
              1 -
                (input$hypothesis.p.value / 2), meanlog = input$mu, sdlog = input$sigma
            )
          )
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
            c(qexp((input$hypothesis.p.value / 2), rate = input$rate),
              qexp(1 - (input$hypothesis.p.value / 2), rate = input$rate))
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
            c(
              qbeta((input$hypothesis.p.value / 2), shape1 = input$shape1, shape2 = input$shape2
              ),
              qbeta(
                1 - (input$hypothesis.p.value / 2), shape1 = input$shape1, shape2 = input$shape2
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
              qbinom((input$hypothesis.p.value / 2), size = input$size, prob = input$prob
              ),
              qbinom(
                1 - (input$hypothesis.p.value / 2), size = input$size, prob = input$prob
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
          border <- c(qchisq((input$hypothesis.p.value / 2), df = input$df),
                      qchisq(1 - (input$hypothesis.p.value / 2), df = input$df))
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
          border <- c(qpois((input$hypothesis.p.value / 2), lambda = input$lambda),
                      qpois(1 - (input$hypothesis.p.value / 2), lambda = input$lambda))
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
          border <- c(qt((input$hypothesis.p.value / 2), df = input$df), 
                      qt(1 - (input$hypothesis.p.value / 2), df = input$df))
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
          border <- c(qf((input$hypothesis.p.value / 2), df1 = input$df1, df2 = input$df2
          ),
          qf(
            1 -
              (input$hypothesis.p.value / 2), df1 = input$df1, df2 = input$df2
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
            qunif((input$hypothesis.p.value / 2), min = input$dist.range[1],
                  max = input$dist.range[2]
            ),
            qunif(
              1 -
                (input$hypothesis.p.value / 2), min = input$dist.range[1],
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
          border <-
            qnorm(input$hypothesis.p.value, mean = input$mu, sd = input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dnorm, min = outputrange[1], max = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Log-normal distribution' = {
          border <-
            qlnorm(input$hypothesis.p.value, meanlog = input$mu, sdlog = input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dlnorm, min = outputrange[1], max = border, meanlog = input$mu,
              sdlog = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Exponential distribution' = {
          border <-
            qexp((input$hypothesis.p.value), rate = input$rate)
          sf <-
            stat_function(
              fun = limitRange(
                dexp, min = outputrange[1], max = border, rate = input$rate
              ), geom = 'area', fill = 'blue', alpha = 0.2,  n = n
            );
        },
        'Beta distribution' = {
          border <- qbeta((input$hypothesis.p.value), shape1 = input$shape1,
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
            qbinom((input$hypothesis.p.value), size = input$size, prob = input$prob)
          sf <- stat_function(
            fun = limitRange(
              dbinom, min = outputrange[1], max = border, size = input$size, prob = input$prob
            ), geom = 'bar', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Chi-Square' = {
          border <- qchisq(input$hypothesis.p.value, df = input$df)
          sf <- stat_function(
            fun = limitRange(
              dchisq, min = outputrange[1], max = border, df = input$df
            ), geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Poisson distribution' = {
          border <- qpois(input$hypothesis.p.value, lambda = input$lambda)
          sf <- stat_function(
            fun = limitRange(
              dpois, min = outputrange[1],
              max = border, lambda = input$lambda
            ),
            geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        't-distribution' = {
          border <- qt(input$hypothesis.p.value, df = input$df)
          sf <- stat_function(
            fun = limitRange(
              dt, min = outputrange[1],
              max = border, df = input$df
            ),
            geom = 'area', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'F-distribution' = {
          border <- qf(input$hypothesis.p.value, df1 = input$df1,
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
              input$hypothesis.p.value, min = input$dist.range[1],
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
            qnorm(1 - input$hypothesis.p.value, mean = input$mu, sd = input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dnorm, max = outputrange[2], min = border, mean = input$mu,
              sd = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Log-normal distribution' = {
          border <-
            qlnorm(1 - input$hypothesis.p.value, meanlog = input$mu, sdlog = input$sigma);
          sf <- stat_function(
            fun = limitRange(
              dlnorm, max = outputrange[2], min = border, meanlog = input$mu,
              sdlog = input$sigma
            ), geom = 'area', fill = 'blue', alpha = '0.2', n = n
          );
        },
        'Exponential distribution' = {
          border <-
            qexp((1 - input$hypothesis.p.value), rate = input$rate)
          sf <-
            stat_function(
              fun = limitRange(
                dexp, max = outputrange[2], min = border, rate = input$rate
              ), geom = 'area', fill = 'blue', alpha = 0.2,  n = n
            );
        },
        'Beta distribution' = {
          border <-
            qbeta((1 - input$hypothesis.p.value), shape1 = input$shape1,
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
            qbinom((1 - input$hypothesis.p.value), size = input$size, prob = input$prob)
          sf <- stat_function(
            fun = limitRange(
              dbinom, max = outputrange[2], min = border, size = input$size, prob = input$prob
            ), geom = 'bar', fill = 'blue', alpha = 0.2 / 9,  n = n
          );
        },
        'Chi-Square' = {
          border <- qchisq(1 - (input$hypothesis.p.value), df = input$df)
          sf <- stat_function(
            fun = limitRange(
              dchisq, max = outputrange[2], min = border, df = input$df
            ), geom = 'area', fill = 'blue', alpha = 0.2 / 9, n = n
          );
        },
        'Poisson distribution' = {
          border <-
            qpois(1 - input$hypothesis.p.value, lambda = input$lambda)
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
          border <- qt(1 - input$hypothesis.p.value, df = input$df)
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
          border <- qf(1 - input$hypothesis.p.value, df1 = input$df1,
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
            1 - input$hypothesis.p.value,
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
  return(" IS DIS ENUF OF A PLACEHOLDER?!")
}