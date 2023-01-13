library(data.table)
source("R_utils/plotting_functions.R")

#### DATA ####
timestep_data_task1 = fread("processed_data/timestep_data_task1.csv")
timestep_data_task4 = fread("processed_data/timestep_data_task4.csv")

# Combined data
averaged_data = fread("processed_data/full_average_data.csv")
time_series_data = fread("processed_data/time_series_surprisal_Phi_combined.csv")

# Goal prior
goal_prior_task4 = fread("processed_data/goal_prior_distribution_task4.csv", colClasses = c('sensory_state'='character'))

# Fitness data
fitness_task1 = fread("processed_data/fitness_task1.csv")
fitness_task4 = fread("processed_data/fitness_task4.csv")


#### AVERAGE LOD PLOTS ####

# convert task names
averaged_data[.(task = c("Task 1", "Task 4", "Task 4 - Perfect"), 
                to = c("Easy task", "Hard task", "Hard task - Perfect")), 
              on = "task", task := i.to]

# Average LOD plot
ggsave(
  "plots/average_LOD_plot.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(averaged_data, "fitness", y_label = "Average fitness", seperator = "task"),
    make_LOD_plot(averaged_data, "Phi", y_label = "Average Phi", seperator = "task"),
    make_LOD_plot(averaged_data, "surprisal", y_label = "Average surprisal", seperator = "task"),
    labels = "auto",
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3
)

#Constructing correlation profile data
neg_runs = time_series_data[task == "Task 4" & lag == 0, mean(cor), by = run][V1<(-0.1), run]
pos_runs = time_series_data[task == "Task 4" & lag == 0, mean(cor), by = run][V1>0.1, run]
neu_runs = time_series_data[task == "Task 4" & lag == 0, mean(cor), by = run][V1<0.1 & V1>(-.1), run]

averaged_data_cor_profile = timestep_data_task4[,.(Phi_mean = mean(Phi), surprisal_mean = mean(surprisal)), by = .(run,agent)][,lapply(.SD, smooth), by = run]
averaged_data_cor_profile$profile = NA
averaged_data_cor_profile$profile[averaged_data_cor_profile$run %in% neg_runs] = "Negative"
averaged_data_cor_profile$profile[averaged_data_cor_profile$run %in% pos_runs] = "Positive"
averaged_data_cor_profile$profile[averaged_data_cor_profile$run %in% neu_runs] = "Neutral"

averaged_data_cor_profile = averaged_data_cor_profile[complete.cases(averaged_data_cor_profile)]

averaged_data_cor_profile = merge(averaged_data_cor_profile, fitness_task4, by = c("run", "agent"))[
  ,.(Phi = mean(Phi_mean), 
     Phi_se = sd(Phi_mean)/sqrt(.N),
     fitness = mean(fitness), 
     fitness_se = sd(fitness)/sqrt(.N),
     surprisal = mean(surprisal_mean), 
     surprisal_se = sd(surprisal_mean)/sqrt(.N)
  ), by = .(agent, profile)
]


# Average LOD plot (hard task split by correlation profiles)
ggsave(
  "plots/average_LOD_plot_profile_split.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(averaged_data_cor_profile, "fitness", y_label = "Average fitness", seperator = "profile"),
    make_LOD_plot(averaged_data_cor_profile, "Phi", y_label = "Average Phi", seperator = "profile"),
    make_LOD_plot(averaged_data_cor_profile, "surprisal", y_label = "Average surprisal", seperator = "profile"),
    labels = "auto",
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3
)


# MEDIAN
median_data1 = timestep_data_task1[,.(Phi = median(Phi),surprisal = median(surprisal)), by = agent]
median_data1$task = "Easy task"
median_data4 = timestep_data_task4[,.(Phi = median(Phi),surprisal = median(surprisal)), by = agent]
median_data4$task = "Hard task"

perfect_runs = unique(fitness_task4[fitness == 1 & agent == 120,run])
median_data4_perfect = timestep_data_task4[run %in% perfect_runs,.(Phi = median(Phi),surprisal = median(surprisal)), by = agent]
median_data4_perfect$task = "Hard task - Perfect"

median_data = rbind(median_data1,median_data4,median_data4_perfect)
median_data$Phi_se = 0
median_data$surprisal_se = 0

ggsave(
  "plots/average_LOD_plot_median.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(median_data, "Phi", y_label = "Median Phi", seperator = "task"),
    make_LOD_plot(median_data, "surprisal", y_label = "Median surprisal", seperator = "task"),
    labels = "auto",
    ncol = 2, common.legend = T
  ), width = 7.5, height = 3
)


# Average LOD plot different Phi aggregates
ggsave(
  "plots/average_LOD_plot_Phi_dif.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(averaged_data, "Phi", y_label = "Average Phi", seperator = "task"),
    make_LOD_plot(averaged_data, "Phi_median", y_label = "Average median of Phi", seperator = "task"),
    make_LOD_plot(averaged_data, "Phi_binary", y_label = "Average % of non-zero Phi", seperator = "task"),
    #labels = "auto",
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3
)


#### TIMESTEP PLOTS ####

# Example selection plot
ggsave("plots/timestep_selection.jpg",
       make_timestep_multi_plot(data = timestep_data_task4, 
                                fitness_data = fitness_task4,
                                time_series_data = time_series_data,
                                trial_list = list(
                                  c(run = 26, agent = 120, trial = 55),
                                  c(run = 6, agent = 120, trial = 55),
                                  c(run = 3, agent = 120, trial = 103),
                                  c(run = 14, agent = 120, trial = 103),
                                  c(run = 36, agent = 120, trial = 87),
                                  c(run = 1, agent = 120, trial = 23)
                                )
       ), width = 7, height = 8
)


#### TIME SERIES PLOTS ####

# Convert task names
time_series_data[.(task = c("Task 1", "Task 4"), 
                to = c("Easy task", "Hard task")), 
              on = "task", task := i.to]

ggsave(
  "plots/time_series_density_-16_16.jpg",
  make_time_series_plot(time_series_data, range = -16:16, seperator = "task"),
  width = 7.5, height = 6
)

ggsave(
  "plots/time_series_density_-6_5.jpg",
  make_time_series_plot(time_series_data, range = -6:5, seperator = "task"),
  width = 7.5, height = 6
)


# Add "Hard task - Perfect"
perfects = fitness_task4[agent==120 & fitness==1, run]
time_series_data_perfect_task4 = time_series_data[run %in% perfects & task == "Hard task"]
time_series_data_perfect_task4$task = "Hard task - Perfect"
time_series_data_perfect_task4 = rbind(time_series_data, time_series_data_perfect_task4)

ggsave(
  "plots/time_series_density_-6_5_perfect_task4.jpg",
  make_time_series_plot(time_series_data_perfect_task4, range = -6:5, seperator = "task"),
  width = 8, height = 6
)


#### GOAL PRIOR PLOTS ####

ggsave(
  "plots/goal_prior_plot.jpg",
  make_goal_prior_plot(goal_prior_task4),
  width = 13, height = 14 
)



#### AVERAGE TRIAL PLOTS ####

# Reload data to avoid code break if data have been changed.
time_series_data = fread("processed_data/time_series_surprisal_Phi_combined.csv")

# Construct plots
average_trial_plot_list = make_average_trial_plot(timestep_data_task1, timestep_data_task4, fitness_task1, fitness_task4, time_series_data)

# Save plots
ggsave(
  "plots/average_trial_plot.jpg",
  average_trial_plot_list[[1]],
  width = 10, height = 5
)

ggsave(
  "plots/average_trial_plot_profile_split.jpg",
  average_trial_plot_list[[2]],
  width = 10, height = 4
)

# Plot that shows all runs across different groupings
ggsave(
  "plots/average_trial_all_runs_plot.jpg",
  average_trial_all_runs_plot(timestep_data_task1, timestep_data_task4, time_series_data, fitness_task4),
  width = 8, height = 8
)


# Plots showing all LODs
average_trial_plot_LOD = make_average_trial_plot_LOD(timestep_data_task4, fitness_task4, time_series_data)

ggsave(
  "plots/average_trial_all_runs_plot_profiles_Phi.jpg",
  average_trial_plot_LOD[[1]],
  width = 8, height = 12
)

ggsave(
  "plots/average_trial_all_runs_plot_profiles_surprisal.jpg",
  average_trial_plot_LOD[[2]],
  width = 8, height = 12
)



# MEDIAN plots
average_trial_plot_median_list = make_average_trial_plot(timestep_data_task1, timestep_data_task4, fitness_task1, fitness_task4, time_series_data, T)

ggsave(
  "plots/average_trial_plot_median.jpg",
  average_trial_plot_median_list[[1]],
  width = 10, height = 5
)

ggsave(
  "plots/average_trial_plot_profile_split_median.jpg",
  average_trial_plot_median_list[[2]],
  width = 10, height = 5
)


#### DISTRIBUTION PLOT ####

ggsave("plots/distribution_plot.jpg",
  distribution_plot(timestep_data_task1, timestep_data_task4, fitness_task4),
  width = 6, height = 4
)


