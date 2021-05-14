average_timestep_data = function(data) {
  
  i = 1
  sum_data_list = list()
  
  runs = unique(data$run)
  agents = unique(data$agent)
  
  for (r in runs) {
    for (a in agents){
      
      # Give message of progress
      cat("\rRun:",r,"- Agent:",a,"     ")
      
      # subset data
      d = data[data$run==r & data$agent==a,] 
      
      # split concept phi string and convert to list of numbers
      ConPhi_list = c()
      for (x in d$concept_phis){
        ConPhi = as.numeric(strsplit(x, "-")[[1]])
        ConPhi_list = c(ConPhi_list,ConPhi)
      }
      
      # make data frame of mean and max values and put them in the list
      sum_data_list[[i]] = data.frame(run = r, agent = a,
                                      Phi_mean = mean(d$Phi), Phi_max = max(d$Phi),
                                      n_concepts_mean = mean(d$n_concepts), n_concepts_max = max(d$n_concepts),
                                      concept_phi_mean = mean(ConPhi_list), concept_phi_max = max(ConPhi_list),
                                      
                                      surprisal_mean = mean(d$surprisal),
                                      surprisal_max = max(d$surprisal)
                                      
                                      
      )
      
      i = i + 1
    } # end loop through agents
  } # end loop through runs
  
  # row bind all data frames in the list (to get one combined data frame)
  sum_data = do.call("rbind", sum_data_list)
  
  # take care of unwanted values 
  sum_data$concept_phi_mean[is.nan(sum_data$concept_phi_mean)] = 0
  sum_data$concept_phi_max[is.infinite(sum_data$concept_phi_max)] = 0
  
  sum_data = sum_data[complete.cases(sum_data),]
  
  # Give message of progress
  cat("\rDone :)                      ")
  
  return(sum_data)
}

smoothing_average_data = function(data, window_size = 6){
  
  #Make empty list for filling in the data
  data_list = list()
  runs = unique(data$run)
  runs = runs[order(runs)]
  
  #Go through each of the sub-data frames
  for (i in runs) {
    
    # Give message of progress
    cat("\rRun:",i,"     ")
    
    df_sub = subset(data, run == i)
    
    #Make a new empty version of it for filling in smoothed data
    df_sub_smooth <- df_sub[FALSE,]
    
    #Go through each row and all columns except the last 2 (task and generation)
    for (r in 1:nrow(df_sub)){
      for (c in 3:(ncol(df_sub))){
        
        #For those data points above the window size
        if (r>window_size){
          
          #Average within the window
          df_sub_smooth[r,c] = mean(df_sub[(r-window_size):r,c])
          
          #For others
        } else {
          
          #Average from beginning
          df_sub_smooth[r,c] = mean(df_sub[1:r,c])
        }
        
        #Also save the task and generation data
        df_sub_smooth[r, 1:2] = df_sub[r, 1:2]
      }
    }
    #Add the sub-data frame to the output
    data_list[[i+1]] = df_sub_smooth
  }
  
  smoothed_data = do.call(rbind, data_list)
  
  # Give message of progress
  cat("\rDone     ")
  
  return(smoothed_data)
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

add_fitness_groups = function(data, fitness_data, group_by, group_sizes){
  
  # Using mean fitness values
  if (group_by == "mean") {
    mean_list = c()
    for (r in 0:49){
      fitness = subset(fitness_data, run == r)$fitness  
      mean_list[r+1] = mean(fitness)
    }
    
    ordered_list = cbind(mean_list, 0:49)[order(mean_list),2]
    
  } else if (group_by == "end") {
    # USing end fitness values
    end_list = c()
    for (r in 0:49){
      fitness = subset(fitness_data, run == r & agent == 120)$fitness
      end_list[r+1] = fitness
    }
    
    ordered_list = cbind(end_list, 0:49)[order(end_list),2]
    
  } else if (group_by == "random"){
    ordered_list = sample(0:49, 50)
  }
 
  # Empty column 
  data$fitness_group = 0
  
  # Fill fitness group column
  i = 1
  for(level in 1:length(group_sizes)){
    runs = ordered_list[i:(i-1 + group_sizes[level])]
    data$fitness_group[data$run %in% runs] = level
    i = i + group_sizes[level]
  }
  
  return(data)
}

average_across_LODs_by_fitness_group = function(averaged_data, fitness_data, task, group_by, group_sizes = rep(10,5)){
  
  averaged_data = add_fitness_groups(averaged_data, fitness_data, group_by, group_sizes)

  split_data = split(averaged_data, averaged_data$fitness_group)

  LOD_data_list = list()
  i = 1
  for(split in split_data){
    runs = unique(split$run)
    fitness_group = unique(split$fitness_group)
    fitness_split = subset(fitness_data, run %in% runs)
    LOD_data = average_across_LODs(split, fitness_split, task)
    LOD_data$fitness_group = fitness_group
    LOD_data_list[[i]] = LOD_data
    i = i + 1
  }
  
  output_data = do.call(rbind, LOD_data_list)
  
  return(output_data)
}

