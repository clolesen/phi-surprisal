library(ggplot2)
library(data.table)
library(ggpubr)

source("functions/plotting_functions.R")

# Loading data 
# ADJUST WHEN ALL DATA IS DONE!
LOD_data = fread("data/LOD_smoothed_data_task4.csv")
average_data = fread("data/average_data_task4.csv")

# average plots
make_LOD_plot(LOD_data, "Phi", y_label = "Average Φ")
make_LOD_plot(LOD_data, "n_concepts", y_label = "Average number of concepts")
make_LOD_plot(LOD_data, "fitness", y_label = "Fitness")

make_surprisal_matrix_plot(LOD_data, "cond1")

plot_all_LOD_phi(average_data, "task4")






