library(data.table)
source("functions/plotting_functions.R")

# Loading data 
# ADJUST WHEN ALL DATA IS DONE!
LOD_data = fread("data/LOD_smoothed_data_task4.csv")
average_data = fread("data/average_data_task4.csv")
timestep_data = fread("data/timestep_data_task4.csv")

# average plots
make_LOD_plot(LOD_data, "Phi", y_label = "Average Φ")
make_LOD_plot(LOD_data, "n_concepts", y_label = "Average number of concepts")
make_LOD_plot(LOD_data, "fitness", y_label = "Fitness")

make_surprisal_matrix_plot(LOD_data, "cond1")

make_all_LOD_phi_plot(average_data, "task4")


make_timestep_plot(
  data = timestep_data, 
  line_variables = c("Phi", "surprisal_internal_cond1_LOD"),
  tile_variables = c("S1", "S2", "M1", "M2"),
  facet = "run", 
  runs = 49, agents = 120, trials = 1
)









