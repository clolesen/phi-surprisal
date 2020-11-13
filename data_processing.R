library(data.table)
source("functions/averaging_functions.R")

# load data
timestep_data_task4 = fread("data/timestep_data_task4.csv")
fitness_task4 = fread("data/fitness_task4.csv")

# Process task 4 data
average_data_task4 = average_timestep_data(timestep_data_task4)
fwrite(average_data_task4, "data/average_data_task4.csv")

smoothed_data_task4 = smoothing_average_data(average_data_task4)
fwrite(smoothed_data_task4, "data/smoothed_average_data_task4.csv")

LOD_smoothed_task4 = average_across_LODs(smoothed_data_task4, fitness_task4, "Task4")
fwrite(LOD_smoothed_task4, "data/LOD_smoothed_data_task4.csv")



