
smooth = function(input){
  
  window_size = 6
  output = 0:120
  
  if (!(all(input == output))){
    for (i in 1:length(input)) output[i] = ifelse(i>window_size, 
                                                  mean(input[(i-window_size):i]),
                                                  mean(input[1:i])
    )
  }
  
  return(output)
}

average_across_LODs = function(averaged_data, fitness_data, task) {
  
  agents = unique(averaged_data$agent)
  agents = agents[order(agents)]

  data_list = list()
  i = 1
  
  #Go through each agent one by one
  for (a in agents){
    
    # Message of progress
    cat("\rAgent: ",a,"     ")
    
    
    #Make a subset with data for that agent across all runs
    d = subset(averaged_data, agent == a)
    
    square_root_of_n = sqrt(length(d$agent))
    
    data_list[[i]] = data.frame(
      
      task=task, generation = a,
      
      fitness = mean(fitness_data[fitness_data$agent==a,]$fitness),
      fitness_se = sd(fitness_data[fitness_data$agent==a,]$fitness)/square_root_of_n,
      
      #Summarize
      Phi = mean(d$Phi_mean),
      Phi_se = sd(d$Phi_mean)/square_root_of_n,
      Phi_max = mean(d$Phi_max),
      Phi_max_se = sd(d$Phi_max)/square_root_of_n,
      
      n_concepts = mean(d$n_concepts_mean),
      n_concepts_se = sd(d$n_concepts_mean)/square_root_of_n,
      n_concepts_max = mean(d$n_concepts_max),
      n_concepts_max_se = sd(d$n_concepts_max)/square_root_of_n,
      
      surprisal = mean(d$surprisal_mean),
      surprisal_se = sd(d$surprisal_mean)/square_root_of_n,
      surprisal_max = mean(d$surprisal_max),
      surprisal_max_se = sd(d$surprisal_max)/square_root_of_n
    )
    
    i = i + 1
  } # end loop over agents
  
  averaged_LOD_data = do.call(rbind, data_list)
  
  cat("\rDone     ")
  
  return(averaged_LOD_data)
  
}
