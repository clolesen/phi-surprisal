library(data.table)
source("functions/time_series_analysis_functions.R")


timestep_data_task1 = fread("data/generation_exclusion/timestep_data_task1.csv")
timestep_data_task4 = fread("data/generation_exclusion/timestep_data_task4.csv")

# Generate all time series files for task 4
for (relative in c("LOD", "animat", "all")){
  for (conditional in c("cond1")){
    for (states in c("system", "blanket", "internal")){
      time_series_analysis = time_series(timestep_data_task4, states, conditional, relative)
      
      filename = paste0("data/time_series_data_task4/time_series_",states, "_", conditional,"_",relative,".csv")
      fwrite(time_series_analysis, filename)
    }
  }
}



# Generate all time series files for task 1
for (relative in c("LOD", "animat", "all")){
  for (conditional in c("cond1")){
    for (states in c("system", "blanket", "internal")){
      time_series_analysis = time_series(timestep_data_task1, states, conditional, relative)
      
      filename = paste0("data/time_series_data_task1/time_series_",states, "_", conditional,"_",relative,".csv")
      fwrite(time_series_analysis, filename)
    }
  }
}
