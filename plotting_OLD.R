library(data.table)
source("functions/plotting_functions.R")

#### DATA ####

# Task 1
LOD_data_task1 = fread("data/no_exclusion/LOD_smoothed_data_task1.csv")
average_data_task1 = fread("data/no_exclusion/average_data_task1.csv")
timestep_data_task1 = fread("data/timestep_data_task1.csv")
time_series_data_task1 = fread("data/no_exclusion/time_series_data_task1/time_series_internal_cond1_LOD.csv")

fitness_task1 = fread("data/fitness_task1.csv")

# Task 4
LOD_data_task4 = fread("data/no_exclusion/LOD_smoothed_data_task4.csv")
average_data_task4 = fread("data/no_exclusion/average_data_task4.csv")
timestep_data_task4 = fread("data/timestep_data_task4.csv")
time_series_data_task4 = fread("data/no_exclusion/time_series_data_task4/time_series_internal_cond1_LOD.csv")

fitness_group_data_task4 = fread("data/no_exclusion/LOD_fitness_group_end_task4.csv")

time_series_surprisal = fread("data/average_surprisal_time_series_data.csv")

fitness_task4 = fread("data/fitness_task4.csv")

# Combined data
LOD_data = fread("data/LOD_data.csv")

time_series_data_task1$task = "Task 1"
time_series_data_task4$task = "Task 4"
time_series_data = rbind(time_series_data_task1,time_series_data_task4)

colnames(time_series_surprisal) = c("agent", "surprisal_correlation", "surprisal_correlation_se_min", "surprisal_correlation_se_max", "run")
surprisal_time_series_phi = merge(time_series_surprisal, average_data_task4, by = c("agent", "run"))

#### AVERAGE PLOTS ####


# Surprisal plot matrices
ggsave(
  "plots/surprisal_plot_matrix.jpg",
  make_surprisal_matrix_plot(LOD_data, "cond1"),
  width = 7.5, height = 6
)


# Replication plot
ggsave(
  "plots/replicate_plot.jpg",
  ggarrange(
    make_LOD_plot(LOD_data, "Phi", y_label = "Average Phi", seperator = "task"),
    make_LOD_plot(LOD_data, "n_concepts", y_label = "Average number of concepts", seperator = "task"),
    make_LOD_plot(LOD_data, "fitness", y_label = "Average fitness", seperator = "task"),
    labels = "auto",
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3
)

# PATH PLOTS
# All LOD
ggsave(
  "plots/Phi_surprisal_path_plot.jpg",
  ggpubr::ggarrange(
    make_generation_path_plot(LOD_data_task4,"surprisal_internal_cond1_LOD", "Phi", "Internal surprisal", "Phi"),
    make_generation_path_plot(LOD_data_task4,"surprisal_blanket_cond1_LOD", "Phi", "Blanket surprisal", "Phi"),
    make_generation_path_plot(LOD_data_task4,"surprisal_blanket_cond1_LOD", "surprisal_internal_cond1_LOD", "Blanket surprisal", "Internal surprisal"),
    ncol = 3, common.legend = T
  ), width = 7.5, height = 3.5
)

make_generation_path_plot(LOD_data_task1,"surprisal_internal_cond1_LOD", "Phi", "internal surprisal", "Phi")
make_generation_path_plot(LOD_data_task1,"surprisal_blanket_cond1_LOD", "Phi", "blanket surprisal", "Phi")
make_generation_path_plot(LOD_data_task1,"surprisal_blanket_cond1_LOD", "surprisal_internal_cond1_LOD", "blanket surprisal", "internal surprisal")

# Individual animats
make_generation_path_plot(average_data_task4,"surprisal_internal_cond1_LOD_mean", "Phi_mean", "internal surprisal", "Phi", color_by = "agent", facet = "~run")
make_generation_path_plot(average_data_task4,"surprisal_blanket_cond1_LOD_mean", "Phi_mean", "blanket surprisal", "Phi", color_by = "agent", facet = "~run")
make_generation_path_plot(average_data_task4,"surprisal_blanket_cond1_LOD_mean", "surprisal_internal_cond1_LOD_mean", "blanket surprisal", "internal surprisal", color_by = "agent", facet = "~run")


#### TIMESTEP PLOTS ####

# Finding the worst and a perfect in task 4

end_fitness = subset(fitness_task4, agent == 120)

#worst
end_fitness[end_fitness$fitness==min(end_fitness$fitness)] # RUN 10

# Perfect
end_fitness[end_fitness$fitness==1] #  RUN 14 29 33 34 49

t = 4
for (i in 1:8){
  
  # Plot with Worst
  ggsave(paste0("plots/time_step_plots/worst_animat_trial",t ,".jpg"),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S1", "S2", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 10, agents = c(10,50,90), trials = t
         ), width = 8, height = 10
  )
  
  
  # Plot with Perfect
  ggsave(paste0("plots/time_step_plots/perfect_animat_trial",t ,".jpg"),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S1", "S2", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 49, agents =  c(10,50,90), trials = t
         ), width = 8, height = 10
  )
  
  t = t + 16
}

ggsave("plots/time_step_plots/worst_perfect/timestep_worst_perfect.jpg",
       ggpubr::ggarrange(
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 10, agents = c(10,50,90)-1, trials = 36
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
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







ggsave("plots/time_step_plots/timestep_selection.jpg",
       ggpubr::ggarrange(
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 26, agents = 120, trials = 55
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 6, agents = 120, trials = 55
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 3, agents = 120, trials = 103
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 14, agents = 120, trials = 103
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
           tile_variables = c("S2", "S1", "M1", "M2"),
           tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
           facet = "~agent", 
           runs = 36, agents = 120, trials = 87
         ),
         make_timestep_plot(
           data = timestep_data_task4, 
           line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
           line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
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
  ggsave(paste0("plots/time_step_plots/generation120/LOD_", i,"_fitness_", fitness_list[i+1], ".jpg"),
         ggpubr::ggarrange(
           make_timestep_plot(
             data = timestep_data_task4, 
             line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
             line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~trial", 
             runs = i, agents = 120, trials = c(4,36,68,100)+3
           ),
           make_timestep_plot(
             data = timestep_data_task4, 
             line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
             line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
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
  ggsave(paste0("plots/time_step_plots/evolution/LOD_", i,"_fitness_", fitness_list[i+1], ".jpg"),
         ggpubr::ggarrange(
           make_timestep_plot(
             data = timestep_data_task4, 
             line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
             line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~agent", 
             runs = i, agents = c(5,10,30,50), trials = 124
           ),
           make_timestep_plot(
             data = timestep_data_task4, 
             line_variables = c("surprisal_internal_cond1_LOD", "surprisal_blanket_cond1_LOD", "Phi"),
             line_names = c("Internal surprisal", "Blanket surprisal", "Phi"),
             tile_variables = c("S2", "S1", "M1", "M2"),
             tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
             facet = "~agent", 
             runs = i, agents = c(70,90,100,120), trials = 124
           ),
           ncol = 2, common.legend = T
         ), width = 14, height = 13
  )
}

# TEST PLOT
ggsave("plots/test_timestep.jpg",
       make_timestep_plot(
         data = timestep_data_task4, 
         line_variables = c("surprisal_blanket_cond1_LOD", "Phi"),
         line_names = c("Blanket surprisal", "Phi", "Internal surprisal"),
         tile_variables = c("S1", "S2", "M1", "M2"),
         tile_names = c("Sensory left", "Sensory right", "Motor left", "Motor right"),
         facet = "~agent", 
         runs = 37, agents = c(30,60,90), trials = 4
       ), width = 8, height = 10
)

 
make_timestep_plot(
  data = timestep_data_task4, 
  line_variables = c("Phi", "surprisal_blanket_cond1_LOD"),
  line_names = c("Phi", "Surprisal"),
  tile_variables = c("S1", "S2", "M1", "M2"),
  tile_names = c("Sensory left", "Sensory right", "Motor left", "Motor right"),
  facet = "~agent", 
  runs = 37, agents = c(30,60,90), trials = 4
)

#### TIME SERIES PLOTS ####

ggsave(
  "plots/time_series_density_internal.jpg",
  make_time_series_plot(time_series_data, seperator = "task"),
  width = 7.5, height = 6
)

ggsave(
  "plots/time_series_density_surprisal.jpg",
  make_time_series_plot(time_series_surprisal, range = -16:16),
  width = 7.5, height = 6
)

make_time_series_plot(time_series_data_task4, seperator = "agent_groups")

for (run in 0:49){
  path = paste0("plots/time_series/agent_groups/",run,".jpg")
  ggsave(path, make_time_series_plot(time_series_data_task4, range = -2:3, runs = run, seperator = "agent_groups"), width = 12, height = 6)
}



fitness_groups = get_fitness_groups(LOD_data_task4, fitness_task4, "end", rep(10,5))
make_time_series_plot(time_series_data_task4, seperator = "fitness_groups", fitness_groups = fitness_groups)





ggplot(timestep_data_task4, aes(x = agent)) +
  geom_bar(aes(fill = as.factor(animat_follow))) +
  facet_wrap(~fitness_group)


time_series_surprisal_generation_plot(time_series_surprisal)

time_series_surprisal_phi_plot(surprisal_time_series_phi)



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



