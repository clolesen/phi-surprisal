library(tsibble)
library(tidyr)
library(dplyr)
library(data.table)


time_series <- function(d, surprisal_states, surprisal_conditional, surprisal_relative, n_lags=16, lag_variable = "Phi"){
  
  #Get the string for the specific type of surprisal
  surprisal_type = paste0("surprisal_",
                          surprisal_states, "_",
                          surprisal_conditional, "_",
                          surprisal_relative)
  
  #Subset the data with the chosen surprisal and phi
  eval(parse(text = paste0(
    "d = select(d, agent_id, run, agent, trial, ", lag_variable, ", ", surprisal_type,")")))
  
  #Make empty array for populating with results
  result_correlations = array(NA, c(length(d$agent_id)*n_lags, 5))
  
  #Make agent ids with run, agent and trial
  d$agent_id <- paste(d$agent_id, "t", d$trial, sep="")
  
  #Set a counter for going through rows
  i = 1
  
  data_list = split(d, d$agent_id)
  
  for (d in data_list){
    
    #Split the id to get the specific run, agent and trial
    id = d$agent_id[1]
    split_id <- strsplit(id, "r|a|t")[[1]]
    run <- as.integer(split_id[2])
    agent <- as.integer(split_id[3])
    trial <- as.integer(split_id[4])
    
    #Make an index for selecting only the current run, agent and trial
    index = d$run == run & d$agent == agent & d$trial == trial
    
    d_sub = d[index,]
    
    #Cross-correlation is not defined if phi is always 0
    if (eval(parse(text= paste0("sum(d_sub$", lag_variable,")"))) > 0) {
      
      #Run the cross-correlation analysis between Phi and the specificed surprisal
      eval(parse(text = paste( "correlation_output <- ccf(d_sub$", lag_variable, ",
                         d_sub$", surprisal_type,",
                         lag = n_lags, correlation = TRUE, pl = FALSE)",
                               sep = "")))
      
      #Get out the list of lags and correlations
      lags <- correlation_output$lag
      correlations <- correlation_output$acf
      
      #Go through each of the lags
      for (current_lag in lags){
        
        #And append the data in long format
        result_correlations[i,] = c(run, agent, trial, current_lag, correlation_output[current_lag][[1]][1])
        
        #Go to next row
        i = i+1
      } #end idx for loop
    } #end if phi>0
  } #end id for loop
  
  #Only return the rows that were filled out
  result_correlations = result_correlations[complete.cases(result_correlations),]
  
  results = data.table(
    run = result_correlations[,1],
    agent = result_correlations[,2],
    trial = result_correlations[,3],
    lag = result_correlations[,4],
    cor = result_correlations[,5]
  )
  
  return (results)
}
