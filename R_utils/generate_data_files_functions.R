library(data.table)
source("functions/averaging_functions.R")
source("functions/behavioural_measure_functions.R")


generate_task_files = function(task){
  
  base_path = paste0("data/")
  
  # load data
  timestep_data_path = paste0(base_path, "timestep_data_task", task, ".csv")
  timestep_data = fread(timestep_data_path)
  
  fitness_data_path = paste0("data/fitness_task", task, ".csv")
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
  data_task1 = fread("data/averaged_across_LOD_smoothed_data_task1.csv")
  data_task4 = fread("data/averaged_across_LOD_smoothed_data_task4.csv")
  
  averaged_data = fread("data/averaged_across_timestep_data_task4.csv")
  smoothed_data = smoothing_average_data(average_data)
  
  fitness_task4 = fread("data/fitness_task4.csv")
  
  data_7fitest = average_across_LODs_by_fitness_group(smoothed_data, fitness_task4, "Task 4 - 7 fittest", "end", group_sizes = c(43,7))
  data_7fitest = subset(LOD_7fitest, fitness_group == 2)[,1:66]
  
  data = rbind(data_task1,data_task4, data_7fitest)
  fwrite(data, "data/full_average_data.csv")
}


#### OLD STUFF #######

# 
# # PROCESS DATA
# 
# # Averaging
# average_data = average_timestep_data(timestep_data)
# average_data_path = paste0(base_path, "average_data_task", task, ".csv")
# fwrite(average_data, average_data_path)
# 
# print("Averageing done")
# 
# # Smoothing
# smoothed_data = smoothing_average_data(average_data)
# smoothed_average_data_path = paste0(base_path, "smoothed_average_data_task", task, ".csv")
# fwrite(smoothed_data, smoothed_average_data_path)
# 
# print("Smoothing done")
# 
# #LOD data
# LOD_smoothed = average_across_LODs(smoothed_data, fitness_data, paste0("Task ", task))
# LOD_smoothed_data_path = paste0(base_path, "LOD_smoothed_data_task", task, ".csv")
# fwrite(LOD_smoothed, LOD_smoothed_data_path)
# 
# print("LOD data done")
# 
# # Fitness groups
# LOD_fitness_group_mean = average_across_LODs_by_fitness_group(smoothed_data, fitness_data, paste0("Task ", task), "mean")
# LOD_fitness_group_mean_path = paste0(base_path, "LOD_fitness_group_mean_task", task, ".csv")
# fwrite(LOD_fitness_group_mean, LOD_fitness_group_mean_path)
# 
# LOD_fitness_group_end = average_across_LODs_by_fitness_group(smoothed_data, fitness_data, paste0("Task ", task), "end")
# LOD_fitness_group_end_path = paste0(base_path, "LOD_fitness_group_end_task", task, ".csv")
# fwrite(LOD_fitness_group_end, LOD_fitness_group_end_path)
# 
# print("Fitness groups done")