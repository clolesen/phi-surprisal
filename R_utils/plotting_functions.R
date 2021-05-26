library(ggplot2)
library(data.table)
library(ggpubr)
library(tidyverse)
library(egg)
library(plyr)
library(scales)

# colors used in plots
# 1: Black
# 2: Blue
# 3: Red
# 4: Green
# 5: Yellow
# 6: Purple
color_palette = c("black", "#3498DB", "#E53935", "#27AE60", "#ff6700", "#490092")
colors = scale_color_manual(values = color_palette)
fills = scale_fill_manual(values = color_palette)


#### AVERAGE PLOTS ####
make_average_plot_data = function(data, variable, seperator){
  
  y = data[,..variable][[1]]
  
  # se = standard error
  variable_se = paste0(variable, "_se")
  se = data[,..variable_se][[1]]
  
  
  plot_data = data.table(
    x = data$generation * 500,
    y = y,
    se_min = y - se,
    se_max = y + se,
    seperator = as.factor(data[,..seperator][[1]])
  )
  
  return(plot_data)
}

LOD_plot = function(plot_data, y_label, x_label, title){
  
  plot = ggplot(plot_data, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = se_min, ymax = se_max, fill = seperator), color = F, alpha = 0.1) +
    geom_line(aes(color = seperator), size = 1) +
    theme_classic() +
    labs(y = y_label, x = x_label, title = title, color = " ", fill = " ") +
    colors + fills
  
  return(plot)
}

make_LOD_plot = function(data, variable, y_label = " ", x_label = "Generation", title =" ",  seperator = "task"){
  
  plot_data = make_average_plot_data(data, variable, seperator)
  
  plot = LOD_plot(plot_data, y_label, x_label, title)
  
  return(plot)
}

make_surprisal_matrix_plot = function(data, cond, seperator = "task"){
  
  plot = ggarrange(
    make_LOD_plot(data, paste0("surprisal_system_",cond,"_all"), y_label = "Surprisal - All runs", x_label = " ", title = "Full system states",  seperator),
    make_LOD_plot(data, paste0("surprisal_blanket_",cond,"_all"), x_label = " ", title = "Blanket states",  seperator = seperator),
    make_LOD_plot(data, paste0("surprisal_internal_",cond,"_all"), x_label = " ", title = "Internal states",  seperator = seperator),
    
    make_LOD_plot(data, paste0("surprisal_system_",cond,"_LOD"), y_label = "Surprisal - LOD", x_label = " ",  seperator = seperator),
    make_LOD_plot(data, paste0("surprisal_blanket_",cond,"_LOD"), x_label = " ", seperator = seperator),
    make_LOD_plot(data, paste0("surprisal_internal_",cond,"_LOD"), x_label = " ",  seperator = seperator),
    
    make_LOD_plot(data, paste0("surprisal_system_",cond,"_animat"), y_label = "Surprisal - Animat",  seperator = seperator),
    make_LOD_plot(data, paste0("surprisal_blanket_",cond,"_animat"),  seperator = seperator),
    make_LOD_plot(data, paste0("surprisal_internal_",cond,"_animat"), seperator =  seperator),
    
    labels = "auto",
    common.legend = T, align = "v"
  )
  
  return(plot)
}

make_all_LOD_phi_plot = function(data, task){
  
  plot_data = subset(data, task == task) 
  
  if (task == "task1") title = "Task 1" else title = "Task 4"
  
  plot = ggplot(plot_data, aes(x = agent, y = Phi_mean))+
    geom_line() +
    theme_minimal() +
    facet_wrap(~run, ncol = 5) +
    labs(y = "Average Phi", x = "Generation", title = paste0(title,": Average Phi over time for all agents seperatly")) 
  
  return(plot)
}

make_generation_path_plot = function(data,x_variable,y_variable,x_label = " ",y_label = " ", title = " ", color_by = "generation", color_by_label = "Generation" ,facet = F){
  
  
  
  plot = ggplot(data, aes_string(x = x_variable, y = y_variable, color = color_by))+
    #geom_smooth() +
    geom_point(size = 0.5) +
    theme_classic() +
    labs(y = y_label, x = x_label, title = title, color = color_by_label) +
    scale_color_gradient(low = "#555555", high = color_palette[2]) +
    geom_path(size = 0.4)
  
  if (!(isFALSE(facet))){
    plot = plot + facet_wrap(as.formula(facet))
  }
  
  return(plot)
}

#### TIMESTEP PLOTS ####

make_timestep_plot_data = function(data, variables, runs, agents, trials){
  
  sub_data = subset(data, run %in% runs & agent %in% agents & trial %in% trials)
  
  columns = c("run", "agent", "trial", "timestep", "block_movement", "task_type", "block_size", variables)
  
  plot_data = sub_data[,..columns]
  
  #plot_data$run = paste("LOD", plot_data$run)
  #plot_data$agent = paste("Animat", plot_data$agent)
  #plot_data$trial = paste("Trial", plot_data$trial)
  
  plot_data$block_movement[plot_data$block_movement == -1] = "Left"
  plot_data$block_movement[plot_data$block_movement == 1] = "Right"
  
  plot_data$task_type[plot_data$task_type == "catch"] = "Catch"
  plot_data$task_type[plot_data$task_type == "avoid"] = "Avoid"
  
  return(plot_data)
}

normalize_surprisal = function(plot_data){
  
  n_columns = ncol(plot_data)
  
  # check for surprisal columns and normalize
  for (column in 5:n_columns){
    name = names(plot_data[,..column]) 
    split_name = strsplit(name, "_")[[1]]
    if (split_name[1] == "surprisal") plot_data[,column] = plot_data[,..column]/2
  }
  
  return(plot_data)
}

timestep_line_plot = function(plot_data, facet, variables, line_names) {
  
  # Covert data to long format
  plot_data = gather(plot_data, y, value, variables)
  plot_data$y = factor(plot_data$y, levels = variables)

  plot = ggplot(plot_data, aes(x = timestep)) +
    geom_line(aes(y = value, color = y), size = 1.2) +
    facet_wrap(as.formula(facet), scales = "free", ncol = 1) +
    scale_color_manual(values = c(color_palette[5],color_palette[4]), labels = line_names) +
    labs(color = " ", x = "Timestep", y = " ")
  
  return(plot)
}

convert_to_tile_plot_data = function(plot_data, variables){
  
  # Covert data to long format
  tile_plot_data = gather(plot_data, y, value, variables)
  
  # convert to y axis values
  scale_step = .2 # Size of vertical space for each block 
  y_val = -.1 
  for (var in variables){
    y_val = y_val - scale_step
    tile_plot_data[tile_plot_data[,"y"]==var,"y"] = y_val
  }
  tile_plot_data$y = as.numeric(tile_plot_data$y)
  
  return(tile_plot_data)
}

add_tile_to_timestep_plot = function(plot, plot_data, variables, tile_names){
  
  tile_plot_data = convert_to_tile_plot_data(plot_data, variables)
  
  y_values = unique(tile_plot_data$y)
  
  for (value in y_values){
    plot = plot + geom_segment(y = value, yend = value, x = 0, xend = 34, colour = "gray")
  }
  
  plot = plot + 
    annotate("rect", xmin=0, xmax=34, ymin=min(y_values)-.15, ymax=-0.12, alpha=0.3, fill= color_palette[2]) +
    geom_vline(xintercept=c(0,34), linetype="solid", color = "black") +
    geom_segment(y =-.12, yend =-.12, x = 0, xend = 34, color = "black", size = 0.5) +
    geom_segment(y = 3.2, yend = 3.2, x = 0, xend = 34, color = "black", size = 0.5) +
    geom_segment(y = min(y_values)-.15, yend = min(y_values)-.15, x = 0, xend = 34, color = "black", size = 0.5) +
    theme_void() +
    theme(
      panel.grid.minor.x = element_line(colour = "lightgray", linetype = "dotted"),
      #strip.background = element_rect(colour = "white", fill = "#eeeeee"),
      strip.text.x = element_blank(), #element_text(margin = margin(3,0,3,0)),
      panel.spacing = unit(1, "lines"),
      plot.margin=unit(c(.5,.5,.5,.5), "cm"),
      axis.text.x = element_text(size = 6,  margin = margin(t = -8)),
      axis.text.y = element_text(size = 8, margin = margin(r = -20), hjust = 1)) +
    scale_alpha_continuous(range = c(0,1), guide=FALSE) +
    scale_y_continuous(breaks = y_values, 
                       labels = tile_names, 
                       limits = c(min(y_values)-0.15, 3) # -0.15 to make room for the last row
                       ) +
    scale_x_continuous(breaks = seq(1,33,1)) +
    geom_tile(data = tile_plot_data,
              aes(y = y, alpha = value),
              height = .1) +
    annotate(geom="text", x=-0.3, y=0.04, label="0 -", size=3) 
                     
  return(plot)           
}

make_timestep_plot = function(data, fitness_data, 
                              line_variables = c("surprisal", "Phi"),
                              line_names = c("Surprisal", "Phi"),
                              tile_variables = c("S2", "S1", "M1", "M2"),
                              tile_names = c("Sensory right", "Sensory left", "Motor right", "Motor left"),
                              facet = "~agent", 
                              runs = 0:49, agents = 0:120, trials = 0:127){
  
  
  
  #LINE PLOT
  line_plot_data = make_timestep_plot_data(data, line_variables, runs, agents, trials)
  
  line_plot_data = normalize_surprisal(line_plot_data)
  
  line_plot = timestep_line_plot(line_plot_data, facet, line_variables, line_names)
  
  # TILE PLOT
  if(is.character(tile_variables[1])){
    
    tile_plot_data = make_timestep_plot_data(data, tile_variables, runs, agents, trials)
    
    full_plot = add_tile_to_timestep_plot(line_plot, tile_plot_data, tile_variables, tile_names)
    
  } else {
    full_plot = line_plot + theme_minimal()
  }
  
  facet_variables = str_split(facet, "~")[[1]]
  if (facet_variables[1]=="") facet_variables = facet_variables[2]
  tags = 0
  if (length(facet_variables) == 1){
    data_list = split(line_plot_data, eval(parse(text=paste0("line_plot_data$", facet_variables))) )
    
    i = 1
    data_text = data.frame()
    for (data in data_list){
      direction = data$block_movement[1]
      size = data$block_size[1]
      type = data$task_type[1]
      agent = data$agent[1] #str_split(data$agent[1], " ")[[1]][2]
      run = data$run[1] #str_split(data$run[1], " ")[[1]][2]
      trial = data$trial[1] #str_split(data$trial[1], " ")[[1]][2]
      LOD_fitness = round(subset(fitness_data, agent==120 & run==data$run[1])$fitness, 2)
      animat_fitness = round(subset(fitness_data, agent==data$agent[1] & run==data$run[1])$fitness, 2)
      
      var = eval(parse(text=paste0("data$", facet_variables, "[1]")))
      
      row = data.frame(
        label1 = paste("Trial:", trial,
                       "\nTrial type:", type,
                       "\nBlock direction:", direction, 
                       "\nBlock size:", size
        ),
        label2 = paste("LOD:", run+1,
                       "\nGeneration:", agent*500,
                       "\nAnimat fitness:", animat_fitness,
                       "\nEnd of LOD fitness:", LOD_fitness
                      ),
        var = var
      )
      data_text = rbind(data_text,row)
      i = i + 1
    }
    
    colnames(data_text) = c("label1", "label2", facet_variables)
    
    full_plot = full_plot + 
      annotate("rect", xmin=0.5, xmax=21.3, ymin=1.6, ymax=3, alpha=0.4, fill= "lightgray") +
      geom_text(
        data    = data_text,
        mapping = aes(x = 0.75, y = 0.6, label = label1),
        hjust   = 0,
        vjust   = -1
      ) + 
      geom_text(
        data    = data_text,
        mapping = aes(x = 10.75, y = 0.6, label = label2),
        hjust   = 0,
        vjust   = -1
      )
    
  } # end if 1 facet variable
  
  return(full_plot)
}


make_timestep_multi_plot = function(data, fitness_data, trial_list, n_col = 2, n_row = 3){
  
  plot_list = list()
  
  i = 1
  for(trial in trial_list){
    plot_list[[i]] = make_timestep_plot(
      data = data, 
      fitness_data = fitness_data, 
      runs = trial[1], agents = trial[2], trials = trial[3]
    )
    
    i = i + 1
  }
  
  plot = ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = n_col, nrow = n_row,common.legend = T 
  )
  
  return(plot)
}


#

#### TIME SERIES PLOTS ####

time_series_plot_data = function(data, range, runs, agents, trials, fitness_groups){
  
  
  plot_data = subset(data,
                     lag %in% range &
                       run %in% runs &
                       agent %in% agents &
                       trial %in% trial
  )
  
  if (is.list(fitness_groups)){
    plot_data$fitness_groups = 0
    
    for (i in 1:length(fitness_groups)){
      plot_data$fitness_groups[plot_data$run %in% fitness_groups[[i]]] = i
    }
  }
  
  return(plot_data)
}

add_agent_groups = function(data) {
  data$agent_groups = 0
  intervals = seq(25,120,24)
  for (i in 1:4){
    group = intervals[i]:(intervals[i]+23)
    data$agent_groups[data$agent %in% group] = i
  }
  return(data)
}

time_series_plot = function(data, seperator){
  
  if(isFALSE(seperator)){
    density = geom_density()
  } else {
    data$seperator = as.factor(data[,..seperator][[1]])
    density = geom_density(aes(color = seperator, fill = seperator), size = 1, alpha = .3)
  }
  
  plot = ggplot(data, aes(x = cor)) +
    facet_wrap(~lag) +
    theme_minimal() +
    #theme(legend.position = "none") +
    geom_vline(xintercept=0) + 
    labs(y = " ", x = "Correlation coeficient", color = " ", fill = " ") +
    xlim(c(-1,1)) +
    density + colors + fills
  
  return(plot)
}

make_time_series_plot = function(data, range = -6:5, seperator = F, runs = 0:49, agents = 0:120, trials = 0:127, fitness_groups = F) {
  
  plot_data = time_series_plot_data(data, range, runs, agents, trials, fitness_groups)
  
  if (seperator == "agent_groups") {
    plot_data = add_agent_groups(plot_data)
  }
  
  plot = time_series_plot(plot_data, seperator)
  
  
  return(plot)
}

get_fitness_groups = function(data, fitness_data, group_by, group_sizes) {
  if (group_by == "mean") {
    mean_list = c()
    for (r in 0:49){
      fitness = subset(fitness_data, run == r)$fitness  
      mean_list[r+1] = mean(fitness)
    }
    
    ordered_list = cbind(mean_list, 0:49)[order(mean_list),2]
    
  } else if (group_by == "end") {
    # USing end fitness values
    end_list = c()
    for (r in 0:49){
      fitness = subset(fitness_data, run == r & agent == 120)$fitness
      end_list[r+1] = fitness
    }
    
    ordered_list = cbind(end_list, 0:49)[order(end_list),2]
    
  } else if (group_by == "random"){
    ordered_list = sample(0:49, 50)
  }
  
  # Empty list 
  data_list = list()
  
  # Fill fitness group column
  i = 1
  for(level in 1:length(group_sizes)){
    runs = ordered_list[i:(i-1 + group_sizes[level])]
    data_list[[level]] = runs
    i = i + group_sizes[level]
  }
  
  return(data_list)
}

time_series_surprisal_generation_plot = function(data){
  
  plot = ggplot(data, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = se_min, ymax = se_max), color = F, alpha = 0.1) +
    geom_line(size = 1) +
    theme_classic() +
    facet_wrap(~run)
    labs(y = "Mean surprisal correlations", x = "Generation", title = " ", color = " ", fill = " ") +
    colors + fills
  
  return(plot)
}

time_series_surprisal_phi_plot = function(data){
  
  plot = ggplot(data, aes(x = surprisal_correlation, y = Phi_mean)) +
    geom_point() +
    theme_classic() +
    labs(y = "Average Phi", x = "Average surprisal correlations", title = " ", color = " ", fill = " ") +
    colors + fills
  
  return(plot)
}

#### GOAL PRIOR PLOT ####

make_sub_goal_prior_plot = function(data){
  
  title = data$split_column[1]
  
  plot = ggplot(data, aes(x = as.factor(timestep), y = sensory_state, fill = Probability)) +
    geom_tile() +
    facet_grid(run ~ .) +
    labs(title = title, x = " ", y = " ") +
    theme_pubclean() +
    scale_fill_gradientn(colors = c("gray", "white", color_palette[5], color_palette[6]),
                         values=rescale(c(0,0.25,0.75,1)),
                         limits=c(0,1)
                         ) + 
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size = 10, face = "bold", color = "#333333"),
          legend.title = element_text(color = "#333333", size = 10, vjust = 0.80),
          legend.text = element_text(size = 8)
          )
  
  return(plot)
}

make_goal_prior_plot_data = function(data){
  
  data$task_type = mapvalues(data$task_type, from = c("avoid", "catch"), to = c("Avoid","Catch"))
  data$block_movement = mapvalues(data$block_movement, from = c(-1, 1), to = c("Left", "Right"))
  
  data$run = paste("LOD:", data$run+1)
  
  data$Probability = exp(data$surprisal * -1)
  
  data$split_column = paste(data$task_type, "/", data$block_movement)
  
  split_data = split(data, data$split_column)
  
  return(split_data)
}

make_goal_prior_plot = function(data){
  
  plot_data = make_goal_prior_plot_data(data)
  
  plot = ggpubr::ggarrange(
    make_sub_goal_prior_plot(plot_data[[1]]),
    make_sub_goal_prior_plot(plot_data[[2]]),
    make_sub_goal_prior_plot(plot_data[[3]]),
    make_sub_goal_prior_plot(plot_data[[4]]),
    ncol = 2, nrow = 2, common.legend = T
  )
  
  return(plot)
}

#### AVERAGE TRIAL PLOT ####

average_across_trials = function(data){
  
  timestep_min = min(data$event_timestep)
  
  i = 1
  data_list = list()
  for (timestep in timestep_min:33){
    
    index_event = which(data$event_timestep == timestep)
    sub_data_event = data[index_event,]
    
    index_timestep = which(data$timestep == timestep)
    sub_data_timestep = data[index_timestep,]
    
    sqrt_n_event = sqrt(length(sub_data_event$Phi))
    sqrt_n_timestep= sqrt(length(sub_data_timestep$Phi))
    
    
    data_list[[i]] = data.frame(
      # Normal timestep measures
      Phi_mean = mean(sub_data_timestep$Phi),
      Phi_se = sd(sub_data_timestep$Phi)/sqrt_n_timestep,
      Phi_sd = sd(sub_data_timestep$Phi),
      Phi_75percentile = quantile(sub_data_timestep$Phi, .75),
      Phi_25percentile = quantile(sub_data_timestep$Phi, .25),
      surprisal_mean = mean(sub_data_timestep$surprisal),
      surprisal_se = sd(sub_data_timestep$surprisal)/sqrt_n_timestep,
      surprisal_sd = sd(sub_data_timestep$surprisal),
      surprisal_75percentile = quantile(sub_data_timestep$surprisal, .75),
      surprisal_25percentile = quantile(sub_data_timestep$surprisal, .25),
      
      # Event measures
      Phi_mean_event = mean(sub_data_event$Phi),
      Phi_se_event = sd(sub_data_event$Phi)/sqrt_n_event,
      Phi_sd_event = sd(sub_data_event$Phi),
      Phi_75percentile_event = quantile(sub_data_event$Phi, .75),
      Phi_25percentile_event = quantile(sub_data_event$Phi, .25),
      surprisal_mean_event = mean(sub_data_event$surprisal),
      surprisal_se_event = sd(sub_data_event$surprisal)/sqrt_n_event,
      surprisal_sd_event = sd(sub_data_event$surprisal),
      surprisal_75percentile_event = quantile(sub_data_event$surprisal, .75),
      surprisal_25percentile_event = quantile(sub_data_event$surprisal, .25),
      
      timestep = timestep
    )
    
    i = i + 1
  }
  
  averaged_data = do.call(rbind, data_list)
  
  return(averaged_data)
}

make_average_trial_data = function(data, fitness_data, time_series_data, task_number){
  
  data = subset(data, agent == 120)
  time_series_data = subset(time_series_data, agent == 120 & task == paste("Task",task_number) & lag == 0)
  
  # make event-related timestep column
  data$trial_id = paste0("r", data$run, "a", data$agent, "t", data$trial)
  split_data = split(data, data$trial_id)
  
  for (i in 1:length(split_data)){
    split_data[[i]]$event_timestep = split_data[[i]]$timestep - which(split_data[[i]]$first_sight==1)
  }
  
  data = do.call(rbind, split_data)
  
  profile_data = dplyr::summarise(group_by(time_series_data, run), cor = mean(cor))
  profile_data$profile = NA
  profile_data$profile[profile_data$cor<(-0.1)] = "Negative"
  profile_data$profile[profile_data$cor>(-0.1)&profile_data$cor<0.1] = "Neutral"
  profile_data$profile[profile_data$cor>0.1] = "Positive"
  
  # Make averages
  data_list = list()
  i = 1
  
  for(group in c("all", "perfect", "Negative", "Neutral", "Positive")){
    
    use_data = data
    
    if(group == "perfect"){
      runs = fitness_data[fitness_data$agent == 120 & fitness_data$fitness == 1,]$run
      use_data = subset(use_data, run %in% runs)
    }
    
    if(group == "Negative"){
      runs = profile_data[profile_data$profile == "Negative",]$run
      use_data = subset(use_data, run %in% runs)
    }
    
    if(group == "Neutral"){
      runs = profile_data[profile_data$profile == "Neutral",]$run
      use_data = subset(use_data, run %in% runs)
    }
    
    if(group == "Positive"){
      runs = profile_data[profile_data$profile == "Positive",]$run
      use_data = subset(use_data, run %in% runs)
    }
    
    average_data = average_across_trials(use_data)
    
    average_data$group = group
    
    data_list[[i]] = average_data
    
    i = i + 1
    
    
  }
  
  data = do.call(rbind, data_list)
  
  return(data)
}

average_trial_plot = function(data, variable, event, title, subtitle, profile = F){
  
  if (variable == "surprisal"){
    y_limit = c(0.15,2.4)
    y_label = "Average surprisal"
  } else {
    y_limit = c(0.1,0.8)
    y_label = "Average Phi"
  } 
  if (event == T){
    x_limit = c(-7,30)
    x_label = "Relative timestep"
    line_variable = paste0(variable,"_mean_event")
    ribbon_variable_min = paste0(line_variable, "-", variable,"_se_event")
    ribbon_variable_max = paste0(line_variable, "+", variable,"_se_event")
    #ribbon_variable_min = paste0(variable,"_25percentile_event")
    #ribbon_variable_max= paste0(variable,"_75percentile_event")
  } else {
    x_limit = c(1,33)
    x_label = "Timestep"
    line_variable = paste0(variable,"_mean")
    ribbon_variable_min = paste0(line_variable, "-", variable,"_se")
    ribbon_variable_max= paste0(line_variable, "+",variable,"_se")
    #ribbon_variable_min = paste0(variable,"_25percentile")
    #ribbon_variable_max= paste0(variable,"_75percentile")
  }
  
  plot = ggplot(data, aes(x = timestep)) +
    geom_vline(xintercept=0, linetype = "dashed", color = "darkgray") +
    geom_ribbon(aes_string(ymin = ribbon_variable_min, ymax = ribbon_variable_max, fill = "seperator"), alpha = .2) +
    geom_line(aes_string(y = line_variable,color = "seperator"), size = 1) +
    lims(x = x_limit, y = y_limit) +
    theme_classic() +
    labs(title = title, x = x_label, y = y_label, color = " ", fill = " ", subtitle = subtitle)
    if (isFALSE(profile)){
      plot = plot + colors + fills
    } else {
      plot = plot +
        scale_color_manual(values = c("#6B00B9","#989898","#8D391E")) +
        scale_fill_manual(values = c("#6B00B9","#989898","#8D391E"))
    }
    
  
  return(plot)
}

make_average_trial_plot = function(timestep_data_task1, timestep_data_task4, fitness_task1, fitness_task4, time_series_data){
  
  task1_data = make_average_trial_data(timestep_data_task1, fitness_task1, time_series_data, 1)
  task4_data = make_average_trial_data(timestep_data_task4, fitness_task4, time_series_data, 4)
  
  profile_data = subset(task4_data, group %in% c("Negative", "Neutral", "Positive"))
  profile_data$seperator = profile_data$group
  
  task1_data= subset(task1_data, group == "all" )
  task4_data_perfect = subset(task4_data, group == "perfect" )
  task4_data= subset(task4_data, group == "all" )
  
  task1_data$seperator = "Task 1"
  task4_data$seperator = "Task 4"
  task4_data_perfect$seperator = "Task 4 - Perfect"
  
  task_data = rbind(task1_data, task4_data, task4_data_perfect)
  
  plot = ggpubr::ggarrange(
    average_trial_plot(task_data, "surprisal", event=T, title = "Surprisal", subtitle = "Relative to first sight event"),
    average_trial_plot(task_data, "Phi", event=T, title = "Phi", subtitle = "Relative to first sight event"),
    average_trial_plot(task_data, "surprisal", event=F, title = "Surprisal", subtitle = "Averaged over trials"),
    average_trial_plot(profile_data, "Phi", event=T, profile = T, title = "Phi", subtitle = "Relative to first sight event \nTask 4 split into correlation profiles"),
    ncol = 2, nrow = 2
  )
  
  return(plot)
}

average_trial_all_runs_plot = function(data, time_series_data){
  
  
  data = subset(data, agent == 120)
  
  
  data$trial_id = paste0("r", data$run, "a", data$agent, "t", data$trial)
  split_data = split(data, data$trial_id)
  
  for (i in 1:length(split_data)){
    split_data[[i]]$event_timestep = split_data[[i]]$timestep - which(split_data[[i]]$first_sight==1)
  }
  
  data = do.call(rbind, split_data)
  
  time_series_data = subset(time_series_data, agent == 120 & task == "Task 4" & lag == 0)
  
  profile_data = dplyr::summarise(group_by(time_series_data, run), cor = mean(cor))
  profile_data$profile = NA
  profile_data$profile[profile_data$cor<(-0.1)] = "Negative"
  profile_data$profile[profile_data$cor>(-0.1)&profile_data$cor<0.1] = "Neutral"
  profile_data$profile[profile_data$cor>0.1] = "Positive"
  
  data = dplyr::summarise(group_by(data, run, event_timestep), Phi = mean(Phi), surprisal = mean(surprisal))
  
  plot_data = merge(data, profile_data, by = "run")
  
  plot = ggplot(plot_data, aes(x = event_timestep, y = Phi, color = profile)) +
    geom_vline(xintercept=0, linetype = "dashed", color = "darkgray") +
    lims(x = c(-7,30)) +
    theme_classic() +
    geom_line(size = 1.5) +
    facet_wrap(~run) +
    scale_color_manual(values = c("#6B00B9","#989898","#8D391E"))
  return(plot)
}


profile_data[profile_data$profile=="positive",]$run[subset(fitness_task4, run %in% profile_data[profile_data$profile=="positive",]$run & agent == 120)$fitness==1]
