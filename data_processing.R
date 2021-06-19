library(data.table)
source("R_utils/generate_data_files_functions.R")
source("R_utils/time_series_analysis_functions.R")

generate_task_files(task = 1)
generate_task_files(task = 4)

# Make one grand LOD data frame including perfect in task 4
generate_full_average_file()

# Make cross correlation between surprisal and Phi
# Task 1
timestep_data_task1 = fread("processed_data/timestep_data_task1.csv")

time_series_surprisal_Phi = timestep_data_task1[, time_series2(surprisal, Phi), by = .(run, agent, trial)]
fwrite(time_series_surprisal_Phi, "processed_data/time_series_surprisal_Phi_task1.csv")

# Task 4
timestep_data_task4 = fread("processed_data/timestep_data_task4.csv")

time_series_surprisal_Phi = timestep_data_task4[, time_series2(surprisal, Phi), by = .(run, agent, trial)]
fwrite(time_series_surprisal_Phi, "processed_data/time_series_surprisal_Phi_task4.csv")

#Combine
timeseries_data_task1 = fread("processed_data/time_series_surprisal_Phi_task1.csv")
timeseries_data_task1$task = "Task 1"

timeseries_data_task4 = fread("processed_data/time_series_surprisal_Phi_task4.csv")
timeseries_data_task4$task = "Task 4"

timeseries_data_combined = rbind(timeseries_data_task1,timeseries_data_task4)
fwrite(timeseries_data_combined, "processed_data/time_series_surprisal_Phi_combined.csv")