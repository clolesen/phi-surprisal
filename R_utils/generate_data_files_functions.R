library(data.table)
source("R_utils/averaging_functions.R")

generate_task_files = function(task){
  
  base_path = "processed_data/"
  
  # load data
  timestep_data_path = paste0(base_path, "timestep_data_task", task, ".csv")
  timestep_data = fread(timestep_data_path)
  
  fitness_data_path = paste0(base_path, "fitness_task", task, ".csv")
  fitness_data = fread(fitness_data_path)
  
  # PROCESS DATA
  
  # Phi binary variable
  timestep_data$Phi_binary = ifelse(timestep_data$Phi > 0, 1, 0)
  
  # Averaging
  average_data = timestep_data[,.(Phi_mean = mean(Phi), 
                                  surprisal_mean = mean(surprisal),
                                  Phi_median = median(Phi),
                                  Phi_binary_mean = mean(Phi_binary)
                                  ), by = .(run,agent)]
  average_data_path = paste0(base_path, "averaged_across_timestep_data_task", task, ".csv")
  fwrite(average_data, average_data_path)
  
  # Smoothing
  smoothed_data = average_data[,lapply(.SD, smooth), by = run]
  
  #LOD data
  LOD_smoothed = merge(smoothed_data, fitness_data, by = c("run", "agent"))[
    ,.(Phi = mean(Phi_mean), 
       Phi_se = sd(Phi_mean)/sqrt(.N),
       fitness = mean(fitness), 
       fitness_se = sd(fitness)/sqrt(.N),
       surprisal = mean(surprisal_mean), 
       surprisal_se = sd(surprisal_mean)/sqrt(.N),
       Phi_median = mean(Phi_median),
       Phi_median_se = sd(Phi_median)/sqrt(.N),
       Phi_binary = mean(Phi_binary_mean),
       Phi_binary_se = sd(Phi_binary_mean)/sqrt(.N)
        ), by = agent
    ]
  
  LOD_smoothed_data_path = paste0(base_path, "averaged_across_LOD_smoothed_data_task", task, ".csv")
  fwrite(LOD_smoothed, LOD_smoothed_data_path)
}


generate_full_average_file = function(){
  data_task1 = fread("processed_data/averaged_across_LOD_smoothed_data_task1.csv")
  data_task1$task = "Task 1"
  
  data_task4 = fread("processed_data/averaged_across_LOD_smoothed_data_task4.csv")
  data_task4$task = "Task 4"
  
  averaged_data = fread("processed_data/averaged_across_timestep_data_task4.csv")
  smoothed_data = averaged_data[,lapply(.SD, smooth), by = run]
  
  fitness_task4 = fread("processed_data/fitness_task4.csv")
  perfect_runs = unique(fitness_task4[fitness == 1 & agent == 120,run])
  
  perfect_averaged_data = merge(smoothed_data[run %in% perfect_runs], fitness_task4[run %in% perfect_runs], by = c("run", "agent"))
  perfect_LOD = perfect_averaged_data[,.(Phi = mean(Phi_mean), 
                                         Phi_se = sd(Phi_mean)/sqrt(.N),
                                         fitness = mean(fitness), 
                                         fitness_se = sd(fitness)/sqrt(.N),
                                         surprisal = mean(surprisal_mean), 
                                         surprisal_se = sd(surprisal_mean)/sqrt(.N),
                                         Phi_median = mean(Phi_median),
                                         Phi_median_se = sd(Phi_median)/sqrt(.N),
                                         Phi_binary = mean(Phi_binary_mean),
                                         Phi_binary_se = sd(Phi_binary_mean)/sqrt(.N)
  ), by = agent
  ]
  perfect_LOD$task = "Task 4 - Perfect"
  
  
  data = rbind(data_task1,data_task4, perfect_LOD)
  fwrite(data, "processed_data/full_average_data.csv")
}

