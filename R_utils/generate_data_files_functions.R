library(data.table)
source("R_utils/averaging_functions.R")

generate_task_files = function(task){
  
  base_path = "processed_data/"
  
  # load data
  timestep_data_path = paste0(base_path, "timestep_data_task", task, ".csv")
  timestep_data = fread(timestep_data_path)
  
  fitness_data_path = paste0("raw_data/fitness_task", task, ".csv")
  fitness_data = fread(fitness_data_path)
  
  # PROCESS DATA
  
  # Averaging
  average_data = average_timestep_data(timestep_data)
  average_data_path = paste0(base_path, "averaged_across_timestep_data_task", task, ".csv")
  fwrite(average_data, average_data_path)
  
  # Smoothing
  smoothed_data = smoothing_average_data(average_data)
  
  #LOD data
  LOD_smoothed = average_across_LODs(smoothed_data, fitness_data, paste0("Task ", task))
  LOD_smoothed_data_path = paste0(base_path, "averaged_across_LOD_smoothed_data_task", task, ".csv")
  fwrite(LOD_smoothed, LOD_smoothed_data_path)
}


generate_full_average_file = function(){
  data_task1 = fread("processed_data/averaged_across_LOD_smoothed_data_task1.csv")
  data_task4 = fread("processed_data/averaged_across_LOD_smoothed_data_task4.csv")
  
  averaged_data = fread("processed_data/averaged_across_timestep_data_task4.csv")
  smoothed_data = smoothing_average_data(as.data.frame(averaged_data))
  
  fitness_task4 = fread("raw_data/fitness_task4.csv")
  
  data_7fitest = average_across_LODs_by_fitness_group(smoothed_data, fitness_task4, "Task 4 - Perfect", "end", group_sizes = c(45,5))
  data_7fitest = subset(data_7fitest, fitness_group == 2)[,1:16]
  
  data = rbind(data_task1,data_task4, data_7fitest)
  fwrite(data, "processed_data/full_average_data.csv")
}
