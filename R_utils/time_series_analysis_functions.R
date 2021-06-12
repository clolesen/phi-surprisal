library(tsibble)
library(data.table)


time_series = function(base_variable, lag_variable){
  
  n_lags=16
  
    correlation_output = ccf(lag_variable,
                              base_variable,
                              lag = n_lags, correlation = TRUE, pl = FALSE)
    
    output = list(lag = correlation_output$lag[,1,1], cor = correlation_output$acf[,1,1])

  return(output)
}

