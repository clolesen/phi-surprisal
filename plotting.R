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


#### AVERAGE PLOTS ####

# Average LOD plot
ggsave(
  "plots/average_LOD_plot.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(averaged_data, "Phi", y_label = "Average Phi", seperator = "task"),
    make_LOD_plot(averaged_data, "surprisal", y_label = "Average surprisal", seperator = "task"),
    make_LOD_plot(averaged_data, "fitness", y_label = "Average fitness", seperator = "task"),
    labels = "auto",
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3
)

neg_runs = time_series_data[task == "Task 4" & lag == 0, mean(cor), by = run][V1<(-0.1), run]
pos_runs = time_series_data[task == "Task 4" & lag == 0, mean(cor), by = run][V1>0.1, run]
neu_runs = time_series_data[task == "Task 4" & lag == 0, mean(cor), by = run][V1<0.1 & V1>(-.1), run]

averaged_data2 = timestep_data_task4[,.(Phi_mean = mean(Phi), surprisal_mean = mean(surprisal)), by = .(run,agent)][,lapply(.SD, smooth), by = run]
averaged_data2$profile = NA
averaged_data2$profile[averaged_data2$run %in% neg_runs] = "Negative"
averaged_data2$profile[averaged_data2$run %in% pos_runs] = "Positive"
averaged_data2$profile[averaged_data2$run %in% neu_runs] = "Neutral"

averaged_data2 = averaged_data2[complete.cases(averaged_data2)]

averaged_data2 = merge(averaged_data2, fitness_task4, by = c("run", "agent"))[
  ,.(Phi = mean(Phi_mean), 
     Phi_se = sd(Phi_mean)/sqrt(.N),
     fitness = mean(fitness), 
     fitness_se = sd(fitness)/sqrt(.N),
     surprisal = mean(surprisal_mean), 
     surprisal_se = sd(surprisal_mean)/sqrt(.N)
  ), by = .(agent, profile)
]


task_plot = ggpubr::ggarrange(
  make_LOD_plot(averaged_data, "fitness", y_label = "Average fitness", seperator = "task"),
  make_LOD_plot(averaged_data, "Phi", y_label = "Average Phi", seperator = "task"),
  make_LOD_plot(averaged_data, "surprisal", y_label = "Average surprisal", seperator = "task"),
  labels = "auto",
  ncol = 3, common.legend = T
)

profile_plot = ggpubr::ggarrange(
  make_LOD_plot(averaged_data2, "fitness", y_label = "Average fitness", seperator = "profile"),
  make_LOD_plot(averaged_data2, "Phi", y_label = "Average Phi", seperator = "profile"),
  make_LOD_plot(averaged_data2, "surprisal", y_label = "Average surprisal", seperator = "profile"),
  labels = c("d", "e", "f"),
  ncol = 3, common.legend = T
)

ggsave(
  "plots/average_LOD_plot_w_profile.jpg",
  ggpubr::ggarrange(task_plot,profile_plot, nrow = 2), 
  width = 7.5, height = 6
)

#### TIMESTEP PLOTS ####

# Finding the worst and a perfect in task 4

end_fitness = subset(fitness_task4, agent == 120)

#worst
end_fitness[end_fitness$fitness==min(end_fitness$fitness)] # RUN 99

# Perfect
end_fitness[end_fitness$fitness==1] #  RUN 14 29 33 34 49 68 89 96

# Worst and best plot
ggsave("plots/timestep_worst_perfect.jpg",
       ggpubr::ggarrange(
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           runs = 99, agents = c(10,50,90)-1, trials = 36
         ),
         make_timestep_plot(
           data = timestep_data_task4,
           fitness_data = fitness_task4,
           runs = 49, agents =  c(10,50,90)-1, trials = 36
         ),
         ncol = 2, common.legend = T, 
         labels = c("Worst animat", "Perfect animat"), 
         font.label = list(size = 10, face = "bold"),
         label.x = 0.08
       ), width = 14, height = 10
)


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
       ), width = 14, height = 10.5
)


# Inspection plots for generation 120
for (r in 0:49){
  
  inspection_trial_list = list()
  i = 1
  for(t in seq(0,127,4)){
    inspection_trial_list[[i]] = c(run = r, agent = 120, trial = t)
    i = i + 1
  }
  
  # Task 1
  ggsave(paste0("plots/timestep_inspection_plots/task_1/generation120/LOD_", r, ".jpg"),
         make_timestep_multi_plot(data = timestep_data_task1, 
                                  fitness_data = fitness_task1,
                                  trial_list = inspection_trial_list,
                                  n_col = 8,
                                  n_row = 4
         ), 
         width = 56, height = 14, limitsize = F
  )
  
  # Task 4
  ggsave(paste0("plots/timestep_inspection_plots/task_4/generation120/LOD_", r, ".jpg"),
         make_timestep_multi_plot(data = timestep_data_task4, 
                                  fitness_data = fitness_task4,
                                  trial_list = inspection_trial_list,
                                  n_col = 8,
                                  n_row = 4
         ), 
         width = 56, height = 14, limitsize = F
  )
  
  
}

# Inspection plots for evolution
for (r in 0:49){
  
  inspection_trial_list = list()
  i = 1
  for(t in c(8, 28, 40, 60, 72, 92, 104, 124)){
    for(a in c(5,10,30,50,70,90,100,120)){
      inspection_trial_list[[i]] = c(run = r, agent = a, trial = t)
      i = i + 1
    }
  }
  
  # Task 1
  ggsave(paste0("plots/timestep_inspection_plots/task_1/evolution/LOD_", r, ".jpg"),
         make_timestep_multi_plot(data = timestep_data_task1, 
                                  fitness_data = fitness_task1,
                                  trial_list = inspection_trial_list,
                                  n_col = 8,
                                  n_row = 8
         ), 
         width = 56, height = 27, limitsize = F
  )
  
  # Task 4
  ggsave(paste0("plots/timestep_inspection_plots/task_4/evolution/LOD_", r, ".jpg"),
         make_timestep_multi_plot(data = timestep_data_task4, 
                                  fitness_data = fitness_task4,
                                  trial_list = inspection_trial_list,
                                  n_col = 8,
                                  n_row = 8
         ), 
         width = 56, height = 27, limitsize = F
  )
}


#### TIME SERIES PLOTS ####

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


perfects = fitness_task4[agent==120 & fitness==1, run]
time_series_data_perfect_task4 = time_series_data[run %in% perfects & task == "Task 4"]
time_series_data_perfect_task4$task = "Task 4 - Perfect"
time_series_data_perfect_task4 = rbind(time_series_data, time_series_data_perfect_task4)

ggsave(
  "plots/time_series_density_-6_5_perfect_task4.jpg",
  make_time_series_plot(time_series_data_perfect_task4, range = -6:5, seperator = "task"),
  width = 7.5, height = 6
)


#### GOAL PRIOR PLOTS ####

ggsave(
  "plots/goal_prior_plot.jpg",
  make_goal_prior_plot(goal_prior_task4),
  width = 13, height = 14 
)



#### AVERAGE TRIAL PLOTS ####

ggsave(
  "plots/average_trial_plot.jpg",
  make_average_trial_plot(timestep_data_task1, timestep_data_task4, fitness_task1, fitness_task4, time_series_data),
  width = 15, height = 6
)

ggsave(
  "plots/average_trial_all_runs_plot.jpg",
  average_trial_all_runs_plot(timestep_data_task4, time_series_data),
  width = 15, height = 12
)


#Stop
##### MIXED CODE ####

ggarrange(
  make_LOD_plot(as.data.table(LOD_fitness_group_end_task4), "Phi", y_label = "Average Phi", x_label = " ", title = "Grouped by end fitness", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_mean_task4), "Phi", y_label = " ",x_label = " ", title = "Grouped by mean fitness", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_end_task4), "fitness", y_label = "Fitness", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_mean_task4), "fitness", y_label = " ", seperator = "fitness_group"),
  common.legend = T
)

ggsave(
  "plots/fitness_groups.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_2), "Phi", y_label = "Average Phi",x_label = " ", title = "2 groups",seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_3), "Phi", x_label = " ", title = "3 groups", seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_4), "Phi", x_label = " ", title = "4 groups", seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_5), "Phi", x_label = " ", title = "5 groups", seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_2), "fitness", y_label = "Fitness", seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_3), "fitness", seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_4), "fitness", seperator = "fitness_group"),
    make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_5), "fitness", x_label = " ", seperator = "fitness_group"),
    legend.grob = get_legend(make_LOD_plot(as.data.table(LOD_fitness_group_end_task4_5), "fitness", seperator = "fitness_group")),
    legend = "right",
    ncol=4, nrow=2
  ), width = 14, height = 8
)



ggarrange(
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_2), "Phi", y_label = "Average Phi",x_label = " ", title = "Random: 2 groups",seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_3), "Phi", x_label = " ", title = "3 groups", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_4), "Phi", x_label = " ", title = "4 groups", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_5), "Phi", x_label = " ", title = "5 groups", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_2), "fitness", y_label = "Fitness", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_3), "fitness", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_4), "fitness", seperator = "fitness_group"),
  make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_5), "fitness", seperator = "fitness_group"),
  legend.grob = get_legend(make_LOD_plot(as.data.table(LOD_fitness_group_ran_task4_5), "fitness", seperator = "fitness_group")),
  legend = "right",
  ncol=4, nrow=2
)



make_LOD_plot(LOD_data, "Phi", y_label = "Average Phi", seperator = "task")
make_LOD_plot(LOD_data, "n_concepts", y_label = "Average number of concepts")
make_LOD_plot(LOD_fitness_group_task1, "fitness", y_label = "Fitness", seperator = "fitness_group")



make_all_LOD_phi_plot(average_data_task1, "task1")



