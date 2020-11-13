average_timestep_data = function(data) {
  
  i = 1
  sum_data_list = list()
  
  runs = order(unique(data$run))
  agents = order(unique(data$agent))
  
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
                                      
                                      # Surprisal relative to all states
                                      surprisal_system_uncond_all_mean = mean(d$surprisal_system_uncond_all),
                                      surprisal_system_uncond_all_max = max(d$surprisal_system_uncond_all),
                                      
                                      surprisal_system_cond1_all_mean = mean(d$surprisal_system_cond1_all),
                                      surprisal_system_cond1_all_max = max(d$surprisal_system_cond1_all),
                                      
                                      surprisal_system_cond2_all_mean = mean(d$surprisal_system_cond2_all),
                                      surprisal_system_cond2_all_max = max(d$surprisal_system_cond2_all),
                                      
                                      surprisal_blanket_uncond_all_mean = mean(d$surprisal_blanket_uncond_all),
                                      surprisal_blanket_uncond_all_max = max(d$surprisal_blanket_uncond_all),
                                      
                                      surprisal_blanket_cond1_all_mean = mean(d$surprisal_blanket_cond1_all),
                                      surprisal_blanket_cond1_all_max = max(d$surprisal_blanket_cond1_all),
                                      
                                      surprisal_blanket_cond2_all_mean = mean(d$surprisal_blanket_cond2_all),
                                      surprisal_blanket_cond2_all_max = max(d$surprisal_blanket_cond2_all),
                                      
                                      surprisal_internal_uncond_all_mean = mean(d$surprisal_internal_uncond_all),
                                      surprisal_internal_uncond_all_max = max(d$surprisal_internal_uncond_all),
                                      
                                      surprisal_internal_cond1_all_mean = mean(d$surprisal_internal_cond1_all),
                                      surprisal_internal_cond1_all_max = max(d$surprisal_internal_cond1_all),
                                      
                                      surprisal_internal_cond2_all_mean = mean(d$surprisal_internal_cond2_all),
                                      surprisal_internal_cond2_all_max = max(d$surprisal_internal_cond2_all),
                                      
                                      # Surprisal relative to LOD states
                                      surprisal_system_uncond_LOD_mean = mean(d$surprisal_system_uncond_LOD),
                                      surprisal_system_uncond_LOD_max = max(d$surprisal_system_uncond_LOD),
                                      
                                      surprisal_system_cond1_LOD_mean = mean(d$surprisal_system_cond1_LOD),
                                      surprisal_system_cond1_LOD_max = max(d$surprisal_system_cond1_LOD),
                                      
                                      surprisal_system_cond2_LOD_mean = mean(d$surprisal_system_cond2_LOD),
                                      surprisal_system_cond2_LOD_max = max(d$surprisal_system_cond2_LOD),
                                      
                                      surprisal_blanket_uncond_LOD_mean = mean(d$surprisal_blanket_uncond_LOD),
                                      surprisal_blanket_uncond_LOD_max = max(d$surprisal_blanket_uncond_LOD),
                                      
                                      surprisal_blanket_cond1_LOD_mean = mean(d$surprisal_blanket_cond1_LOD),
                                      surprisal_blanket_cond1_LOD_max = max(d$surprisal_blanket_cond1_LOD),
                                      
                                      surprisal_blanket_cond2_LOD_mean = mean(d$surprisal_blanket_cond2_LOD),
                                      surprisal_blanket_cond2_LOD_max = max(d$surprisal_blanket_cond2_LOD),
                                      
                                      surprisal_internal_uncond_LOD_mean = mean(d$surprisal_internal_uncond_LOD),
                                      surprisal_internal_uncond_LOD_max = max(d$surprisal_internal_uncond_LOD),
                                      
                                      surprisal_internal_cond1_LOD_mean = mean(d$surprisal_internal_cond1_LOD),
                                      surprisal_internal_cond1_LOD_max = max(d$surprisal_internal_cond1_LOD),
                                      
                                      surprisal_internal_cond2_LOD_mean = mean(d$surprisal_internal_cond2_LOD),
                                      surprisal_internal_cond2_LOD_max = max(d$surprisal_internal_cond2_LOD),
                                      
                                      
                                      # Surprisal relative to animat states
                                      surprisal_system_uncond_animat_mean = mean(d$surprisal_system_uncond_animat),
                                      surprisal_system_uncond_animat_max = max(d$surprisal_system_uncond_animat),
                                      
                                      surprisal_system_cond1_animat_mean = mean(d$surprisal_system_cond1_animat),
                                      surprisal_system_cond1_animat_max = max(d$surprisal_system_cond1_animat),
                                      
                                      surprisal_system_cond2_animat_mean = mean(d$surprisal_system_cond2_animat),
                                      surprisal_system_cond2_animat_max = max(d$surprisal_system_cond2_animat),
                                      
                                      surprisal_blanket_uncond_animat_mean = mean(d$surprisal_blanket_uncond_animat),
                                      surprisal_blanket_uncond_animat_max = max(d$surprisal_blanket_uncond_animat),
                                      
                                      surprisal_blanket_cond1_animat_mean = mean(d$surprisal_blanket_cond1_animat),
                                      surprisal_blanket_cond1_animat_max = max(d$surprisal_blanket_cond1_animat),
                                      
                                      surprisal_blanket_cond2_animat_mean = mean(d$surprisal_blanket_cond2_animat),
                                      surprisal_blanket_cond2_animat_max = max(d$surprisal_blanket_cond2_animat),
                                      
                                      surprisal_internal_uncond_animat_mean = mean(d$surprisal_internal_uncond_animat),
                                      surprisal_internal_uncond_animat_max = max(d$surprisal_internal_uncond_animat),
                                      
                                      surprisal_internal_cond1_animat_mean = mean(d$surprisal_internal_cond1_animat),
                                      surprisal_internal_cond1_animat_max = max(d$surprisal_internal_cond1_animat),
                                      
                                      surprisal_internal_cond2_animat_mean = mean(d$surprisal_internal_cond2_animat),
                                      surprisal_internal_cond2_animat_max = max(d$surprisal_internal_cond2_animat)
                                      
                                      
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
      for (c in 4:(ncol(df_sub))){
        
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
        df_sub_smooth[r, 1:3] = df_sub[r, 1:3]
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



average_across_LODs <- function(averaged_data, fitness_data, task) {
  
  agents = order(unique(averaged_data$agent))
  print(agents)
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
      
      #Summarize IIT measures
      Phi = mean(d$Phi_mean),
      Phi_se = sd(d$Phi_mean)/square_root_of_n,
      Phi_max = mean(d$Phi_max),
      Phi_max_se = sd(d$Phi_max)/square_root_of_n,
      
      n_concepts = mean(d$n_concepts_mean),
      n_concepts_se = sd(d$n_concepts_mean)/square_root_of_n,
      n_concepts_max = mean(d$n_concepts_max),
      n_concepts_max_se = sd(d$n_concepts_max)/square_root_of_n,
      
      # Surprisal relative to all states
      surprisal_system_uncond_all = mean(d$surprisal_system_uncond_all_mean),
      surprisal_system_uncond_all_se = sd(d$surprisal_system_uncond_all_mean)/square_root_of_n,
      
      surprisal_system_cond1_all = mean(d$surprisal_system_cond1_all_mean),
      surprisal_system_cond1_all_se = sd(d$surprisal_system_cond1_all_mean)/square_root_of_n,
      
      surprisal_system_cond2_all = mean(d$surprisal_system_cond2_all_mean),
      surprisal_system_cond2_all_se = sd(d$surprisal_system_cond2_all_mean)/square_root_of_n,
      
      surprisal_blanket_uncond_all = mean(d$surprisal_blanket_uncond_all_mean),
      surprisal_blanket_uncond_all_se = sd(d$surprisal_blanket_uncond_all_mean)/square_root_of_n,
      
      surprisal_blanket_cond1_all = mean(d$surprisal_blanket_cond1_all_mean),
      surprisal_blanket_cond1_all_se = sd(d$surprisal_blanket_cond1_all_mean)/square_root_of_n,
      
      surprisal_blanket_cond2_all = mean(d$surprisal_blanket_cond2_all_mean),
      surprisal_blanket_cond2_all_se = sd(d$surprisal_blanket_cond2_all_mean)/square_root_of_n,
      
      surprisal_internal_uncond_all = mean(d$surprisal_internal_uncond_all_mean),
      surprisal_internal_uncond_all_se = sd(d$surprisal_internal_uncond_all_mean)/square_root_of_n,
      
      surprisal_internal_cond1_all = mean(d$surprisal_internal_cond1_all_mean),
      surprisal_internal_cond1_all_se = sd(d$surprisal_internal_cond1_all_mean)/square_root_of_n,
      
      surprisal_internal_cond2_all = mean(d$surprisal_internal_cond2_all_mean),
      surprisal_internal_cond2_all_se = sd(d$surprisal_internal_cond2_all_mean)/square_root_of_n,
      
      # Surprisal relative to LOD states
      surprisal_system_uncond_LOD = mean(d$surprisal_system_uncond_LOD_mean),
      surprisal_system_uncond_LOD_se = sd(d$surprisal_system_uncond_LOD_mean)/square_root_of_n,
      
      surprisal_system_cond1_LOD = mean(d$surprisal_system_cond1_LOD_mean),
      surprisal_system_cond1_LOD_se = sd(d$surprisal_system_cond1_LOD_mean)/square_root_of_n,
      
      surprisal_system_cond2_LOD = mean(d$surprisal_system_cond2_LOD_mean),
      surprisal_system_cond2_LOD_se = sd(d$surprisal_system_cond2_LOD_mean)/square_root_of_n,
      
      surprisal_blanket_uncond_LOD = mean(d$surprisal_blanket_uncond_LOD_mean),
      surprisal_blanket_uncond_LOD_se = sd(d$surprisal_blanket_uncond_LOD_mean)/square_root_of_n,
      
      surprisal_blanket_cond1_LOD = mean(d$surprisal_blanket_cond1_LOD_mean),
      surprisal_blanket_cond1_LOD_se = sd(d$surprisal_blanket_cond1_LOD_mean)/square_root_of_n,
      
      surprisal_blanket_cond2_LOD = mean(d$surprisal_blanket_cond2_LOD_mean),
      surprisal_blanket_cond2_LOD_se = sd(d$surprisal_blanket_cond2_LOD_mean)/square_root_of_n,
      
      surprisal_internal_uncond_LOD = mean(d$surprisal_internal_uncond_LOD_mean),
      surprisal_internal_uncond_LOD_se = sd(d$surprisal_internal_uncond_LOD_mean)/square_root_of_n,
      
      surprisal_internal_cond1_LOD = mean(d$surprisal_internal_cond1_LOD_mean),
      surprisal_internal_cond1_LOD_se = sd(d$surprisal_internal_cond1_LOD_mean)/square_root_of_n,
      
      surprisal_internal_cond2_LOD = mean(d$surprisal_internal_cond2_LOD_mean),
      surprisal_internal_cond2_LOD_se = sd(d$surprisal_internal_cond2_LOD_mean)/square_root_of_n,
      
      
      # Surprisal relative to animat states
      surprisal_system_uncond_animat = mean(d$surprisal_system_uncond_animat_mean),
      surprisal_system_uncond_animat_se = sd(d$surprisal_system_uncond_animat_mean)/square_root_of_n,
      
      surprisal_system_cond1_animat = mean(d$surprisal_system_cond1_animat_mean),
      surprisal_system_cond1_animat_se = sd(d$surprisal_system_cond1_animat_mean)/square_root_of_n,
      
      surprisal_system_cond2_animat = mean(d$surprisal_system_cond2_animat_mean),
      surprisal_system_cond2_animat_se = sd(d$surprisal_system_cond2_animat_mean)/square_root_of_n,
      
      surprisal_blanket_uncond_animat = mean(d$surprisal_blanket_uncond_animat_mean),
      surprisal_blanket_uncond_animat_se = sd(d$surprisal_blanket_uncond_animat_mean)/square_root_of_n,
      
      surprisal_blanket_cond1_animat = mean(d$surprisal_blanket_cond1_animat_mean),
      surprisal_blanket_cond1_animat_se = sd(d$surprisal_blanket_cond1_animat_mean)/square_root_of_n,
      
      surprisal_blanket_cond2_animat = mean(d$surprisal_blanket_cond2_animat_mean),
      surprisal_blanket_cond2_animat_se = sd(d$surprisal_blanket_cond2_animat_mean)/square_root_of_n,
      
      surprisal_internal_uncond_animat = mean(d$surprisal_internal_uncond_animat_mean),
      surprisal_internal_uncond_animat_se = sd(d$surprisal_internal_uncond_animat_mean)/square_root_of_n,
      
      surprisal_internal_cond1_animat = mean(d$surprisal_internal_cond1_animat_mean),
      surprisal_internal_cond1_animat_se = sd(d$surprisal_internal_cond1_animat_mean)/square_root_of_n,
      
      surprisal_internal_cond2_animat = mean(d$surprisal_internal_cond2_animat_mean),
      surprisal_internal_cond2_animat_se = sd(d$surprisal_internal_cond2_animat_mean)/square_root_of_n
    )
    
    i = i + 1
  } # end loop over agents
  
  averaged_LOD_data = do.call(rbind, data_list)
  
  cat("\rDone     ")
  
  return(averaged_LOD_data)
  
}

