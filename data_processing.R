library(data.table)
source("functions/averaging_functions.R")

timestep_data_task4 = fread("data/timestep_data_task4.csv")
test_data = fread("data/timestep_data_task4.csv")
fitness_task4 = fread("data/fitness_task4.csv")

View(subset(timestep_data_task4, run==0 & agent==1))


average_data_task4 = average_timestep_data(timestep_data_task4)
fwrite(average_data_task4, "data/average_data_task4.csv")


test = smoothing_average_data(average_data_task4)

