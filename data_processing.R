library(data.table)
source("functions/generate_data_files_functions.R")
source("functions/time_series_analysis_functions.R")

generate_task_files(task = 1)
generate_task_files(task = 4)

# Make one grand LOD data frame with 7 fittest in task 4
generate_full_average_file()

# Make cross correlation between surprisal and Phi
timestep_data_task4 = fread("data/timestep_data_task4.csv")
time_series_surprisal_Phi = time_series(timestep_data_task4, 16, base_variable = "surprisal", lag_variable = "Phi")
fwrite(time_series_surprisal_Phi, "data/time_series_surprisal_Phi.csv")


