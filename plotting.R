library(data.table)
source("R_utils/plotting_functions.R")

#### DATA ####
timestep_data_task4 = fread("processed_data/timestep_data_task4.csv")
time_series_data = fread("processed_data/time_series_surprisal_Phi.csv")
# Combined data
averaged_data = fread("processed_data/full_average_data.csv")

# Goal priors
goal_prior_task1 = fread("goal_priors/goal_prior_distribution_task1.csv", colClasses = c('sensory_state'='character'))
sub_goal_prior_task1 = subset(goal_prior_task1, run %in% c(0,1,10,11,12))

goal_prior_task4 = fread("goal_priors/goal_prior_distribution_task4.csv", colClasses = c('sensory_state'='character'))

#### AVERAGE PLOTS ####

# Replication plot
ggsave(
  "plots/replicate_plot.jpg",
  ggpubr::ggarrange(
    make_LOD_plot(averaged_data, "Phi", y_label = "Average Phi", seperator = "task"),
    make_LOD_plot(averaged_data, "n_concepts", y_label = "Average number of concepts", seperator = "task"),
    make_LOD_plot(averaged_data, "fitness", y_label = "Average fitness", seperator = "task"),
    labels = "auto",
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3
)

# Surprisal plot
ggsave(
  "plots/averaged_surprisal.jpg",
  make_LOD_plot(averaged_data, "surprisal", y_label = "Average surprisal", seperator = "task"),
  width = 4, height = 3
)


#### TIMESTEP PLOTS ####

# Finding the worst and a perfect in task 4

end_fitness = subset(fitness_task4, agent == 120)

#worst
end_fitness[end_fitness$fitness==min(end_fitness$fitness)] # RUN 10

# Perfect
end_fitness[end_fitness$fitness==1] #  RUN 14 29 33 34 49




ggsave("plots/timestep_worst_perfect.jpg",
       ggpubr::ggarrange(
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent",
           runs = 10, agents = c(10,50,90)-1, trials = 36
         ),
         make_timestep_plot(
           data = timestep_data_task4,
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent",
           runs = 49, agents =  c(10,50,90)-1, trials = 36
         ),
         ncol = 2, common.legend = T, 
         labels = c("Worst animat", "Perfect animat"), 
         font.label = list(size = 10, face = "bold"),
         label.x = 0.08
       ), width = 14, height = 10
)







ggsave("plots/timestep_selection.jpg",
       ggpubr::ggarrange(
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 26, agents = 120, trials = 55
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 6, agents = 120, trials = 55
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 3, agents = 120, trials = 103
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 14, agents = 120, trials = 103
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 36, agents = 120, trials = 87
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           fitness_data = fitness_task4,
           line_variables = c("surprisal", "Phi"),
           line_names = c("Surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 1, agents = 120, trials = 23
         ),
         ncol = 2, nrow = 3,common.legend = T 
       ), width = 14, height = 10.5
)







fitness_list = subset(fitness_task4, agent==120)$fitness

# Inspection plots for generation 120
for (i in 0:49){
  ggsave(paste0("plots/time_step_plots_inspection/generation120/LOD_", i,"_fitness_", fitness_list[i+1], ".jpg"),
         ggpubr::ggarrange(
           make_timestep_plot(
             data = timestep_data_task4, 
             fitness_data = fitness_task4,
             line_variables = c("surprisal", "Phi"),
             line_names = c("Surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~trial", 
             runs = i, agents = 120, trials = c(4,36,68,100)+3
           ),
           make_timestep_plot(
             data = timestep_data_task4, 
             fitness_data = fitness_task4,
             line_variables = c("surprisal", "Phi"),
             line_names = c("Surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~trial", 
             runs = i, agents = 120, trials = c(20,52,84,116)+3
           ),
           ncol = 2, common.legend = T
         ), width = 14, height = 13
  )
}

# Inspection plots for evolution
for (i in 0:49){
  ggsave(paste0("plots/time_step_plots_inspection/evolution/LOD_", i,"_fitness_", fitness_list[i+1], ".jpg"),
         ggpubr::ggarrange(
           make_timestep_plot(
             data = timestep_data_task4, 
             fitness_data = fitness_task4,
             line_variables = c("surprisal", "Phi"),
             line_names = c("Surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~agent", 
             runs = i, agents = c(5,10,30,50), trials = 124
           ),
           make_timestep_plot(
             data = timestep_data_task4, 
             fitness_data = fitness_task4,
             line_variables = c("surprisal", "Phi"),
             line_names = c("Surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~agent", 
             runs = i, agents = c(70,90,100,120), trials = 124
           ),
           ncol = 2, common.legend = T
         ), width = 14, height = 13
  )
}


#### TIME SERIES PLOTS ####

ggsave(
  "plots/time_series_density_-16_16.jpg",
  make_time_series_plot(time_series_data, range = -16:16),
  width = 7.5, height = 6
)

ggsave(
  "plots/time_series_density_-6_5.jpg",
  make_time_series_plot(time_series_data, range = -6:5),
  width = 7.5, height = 6
)

make_time_series_plot(time_series_data, seperator = "agent_groups")

for (run in 0:49){
  path = paste0("plots/time_series/agent_groups/",run,".jpg")
  ggsave(path, make_time_series_plot(time_series_data_task4, range = -2:3, runs = run, seperator = "agent_groups"), width = 12, height = 6)
}



fitness_groups = get_fitness_groups(subset(averaged_data, task == "Task 4"), fitness_task4, "end", c(9,9,9,9,9,5))
make_time_series_plot(time_series_data, seperator = "fitness_groups", fitness_groups = fitness_groups)


ggplot(timestep_data_task4, aes(x = agent)) +
  geom_bar(aes(fill = as.factor(animat_follow))) +
  facet_wrap(~fitness_group)

fitness_order = get_fitness_groups(subset(averaged_data, task == "Task 4"), fitness_task4, "end", 50)
data_list = list()
for (i in 1:50){
  runs = fitness_order[[1]][i]
  cors = subset(time_series_data, run %in% runs & lag == 0)$cor
  data_list[[i]] = data.frame(cor = cors, fit_group = i)
}
timeseries_fit_group_data = do.call(rbind, data_list)

ggsave(
  "plots/density_split_by_fitness_lag0.jpg",
  ggplot(timeseries_fit_group_data, aes(x = cor)) +
    geom_density(aes(color=as.factor(fit_group), fill = as.factor(fit_group)), alpha = 0.1, size = 0.5) +
    theme_classic(),
  width = 12, height = 5
)




#### GOAL PRIOR PLOTS ####

ggsave(
  "plots/goal_prior_plot.jpg",
  make_goal_prior_plot(goal_prior_task4),
  width = 13, height = 10 
)


stop

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



