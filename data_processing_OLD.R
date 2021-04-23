library(data.table)
source("functions/generate_data_files_function.R")
source("functions/time_series_analysis_functions.R")

generate_data_files(task= 1, exclusion = "no")
generate_data_files(task= 4, exclusion = "no")
generate_data_files(task= 1, exclusion = "generation")
generate_data_files(task= 4, exclusion = "generation")

# Make one grand LOD data frame with 7 fittest in task 4
LOD_task1 = fread("data/no_exclusion/LOD_smoothed_data_task1.csv")
LOD_task4 = fread("data/no_exclusion/LOD_smoothed_data_task4.csv")

averaged_data = fread("data/no_exclusion/smoothed_average_data_task4.csv")
fitness_task4 = fread("data/fitness_task4.csv")
LOD_7fitest = average_across_LODs_by_fitness_group(averaged_data, fitness_task4, "Task 4 - 7 fittest", "end", group_sizes = c(43,7))
LOD_7fitest = subset(LOD_7fitest, fitness_group == 2)[,1:66]

LOD_data = rbind(LOD_task1,LOD_task4, LOD_7fitest)
fwrite(LOD_data, "data/LOD_data.csv")


# Make cross correlation between blanket and internal surprisal
timestep_data_task4 = fread("data/no_exclusion/behavioural_timestep_data_task4.csv")
surpisal_time_series = time_series(timestep_data_task4, "blanket", "cond1", "LOD", 16, "surprisal_internal_cond1_LOD")
fwrite(surpisal_time_series, "data/surprisal_time_series.csv")

# Make cross correlation averages
surprisal_time_series = fread("data/surprisal_time_series.csv")

data_list = list()
i = 1
for (a in unique(surprisal_time_series$agent)){
  print(a)
  for (r in unique(surprisal_time_series$run)){
    correlations = subset(surprisal_time_series, agent == a & run == r & lag == 0)$cor
    correlation_stats = mean_se(abs(correlations))
    data_list[[i]] = data.frame(x = a, y = correlation_stats[[1]], se_min = correlation_stats[[2]], se_max = correlation_stats[[3]], run = r)
    i = i + 1
  }
}
average_time_series_data = do.call(rbind, data_list)
fwrite(average_time_series_data, "data/average_surprisal_time_series_data.csv")



#### MIXED CODE ####
smoothed_data_task4 = fread("data/no_exclusion/smoothed_average_data_task4.csv")
LOD_fitness_group_end_task4_2 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "end", group_sizes = c(25,25))
LOD_fitness_group_end_task4_3 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "end", group_sizes = c(16,17,17))
LOD_fitness_group_end_task4_4 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "end", group_sizes = c(12,12,13,13))
LOD_fitness_group_end_task4_5 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "end")


LOD_fitness_group_ran_task4_2 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "random", group_sizes = c(25,25))
LOD_fitness_group_ran_task4_3 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "random", group_sizes = c(16,17,17))
LOD_fitness_group_ran_task4_4 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "random", group_sizes = c(12,12,13,13))
LOD_fitness_group_ran_task4_5 = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4", "random")


LOD_7fitest = average_across_LODs_by_fitness_group(smoothed_data_task4, fitness_task4, "Task4 - 7 fittest", "end", group_sizes = c(43,7))
LOD_7fitest = subset(LOD_7fitest, fitness_group == 2)[,1:66]
