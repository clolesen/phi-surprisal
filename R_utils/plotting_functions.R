library(ggplot2)
library(data.table)
library(ggpubr)
library(tidyverse)
library(egg)
library(plyr)
library(scales)
library(gghalves)

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
    x = data$agent * 500,
    y = y,
    se_min = y - se,
    se_max = y + se,
    seperator = as.factor(data[,..seperator][[1]])
  )
  
  return(plot_data)
}

LOD_plot = function(plot_data, y_label, x_label, title, seperator){
  
  if(seperator == "profile"){
    color_palette = c("#6B00B9","#989898","#8D391E")
    colors = scale_color_manual(values = color_palette)
    fills = scale_fill_manual(values = color_palette)
  }
  
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
  
  plot = LOD_plot(plot_data, y_label, x_label, title, seperator)
  
  return(plot)
}


#### TIMESTEP PLOTS ####

make_timestep_plot_data = function(data, variables, runs, agents, trials){
  
  sub_data = subset(data, run %in% runs & agent %in% agents & trial %in% trials)
  
  columns = c("run", "agent", "trial", "timestep", "block_movement", "task_type", "block_size", variables)
  
  plot_data = sub_data[,..columns]
  
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
  
  x_labels = c(rbind(as.character(seq(1,33,2)), rep("",17)))[1:33]
  
  plot = plot + 
    annotate("rect", xmin=0, xmax=34, ymin=min(y_values)-.15, ymax=-0.12, alpha=0.3, fill= color_palette[2]) +
    geom_vline(xintercept=c(0,34), linetype="solid", color = "black") +
    geom_segment(y =-.12, yend =-.12, x = 0, xend = 34, color = "black", size = 0.5) +
    geom_segment(y = 3.2, yend = 3.2, x = 0, xend = 34, color = "black", size = 0.5) +
    geom_segment(y = min(y_values)-.15, yend = min(y_values)-.15, x = 0, xend = 34, color = "black", size = 0.5) +
    theme_void() +
    theme(
      panel.grid.minor.x = element_line(colour = "gray", linetype = "dotted"),
      strip.text.x = element_blank(), #element_text(margin = margin(3,0,3,0)),
      panel.spacing = unit(1, "lines"),
      plot.margin=unit(c(.5,.5,.5,.5), "cm"),
      axis.text.x = element_text(size = 7,  margin = margin(t = -4)),
      axis.text.y = element_text(size = 8, margin = margin(r = -10), hjust = 1)) +
    scale_alpha_continuous(range = c(0,1), guide=FALSE) +
    scale_y_continuous(breaks = y_values, 
                       labels = tile_names, 
                       limits = c(min(y_values)-0.15, 3) # -0.15 to make room for the last row
                       ) +
    scale_x_continuous(breaks = seq(1,33,1), labels = x_labels) +
    geom_tile(data = tile_plot_data,
              aes(y = y, alpha = value),
              height = .1) +
    annotate(geom="text", x=-0.3, y=0.04, label="0 -", size=3) 
                     
  return(plot)           
}

make_timestep_plot = function(data, fitness_data, time_series_data,
                              line_variables = c("surprisal", "Phi"),
                              line_names = c("Surprisal", "Phi"),
                              tile_variables = c("S2", "S1", "M1", "M2"),
                              tile_names = c("SR", "SL", "MR", "ML"),
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
      agent = data$agent[1]
      run = data$run[1] 
      trial = data$trial[1] 
      correlation = round(time_series_data[run==data$run[1] & trial == data$trial[1] & lag==0 & task == "Task 4", cor], 2)
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
                       "\nCorrelation:", correlation
                      ),
        var = var
      )
      data_text = rbind(data_text,row)
      i = i + 1
    }
    
    colnames(data_text) = c("label1", "label2", facet_variables)
    
    full_plot = full_plot + 
      annotate("rect", xmin=0.5, xmax=28.5, ymin=1.5, ymax=3, alpha=0.4, fill= "lightgray") +
      geom_text(
        data    = data_text,
        mapping = aes(x = 0.75, y = 0.4, label = label1),
        hjust   = 0,
        vjust   = -1,
        size    = 3
      ) + 
      geom_text(
        data    = data_text,
        mapping = aes(x = 15.5, y = 0.4, label = label2),
        hjust   = 0,
        vjust   = -1,
        size    = 3
      )
    
  } # end if 1 facet variable
  
  return(full_plot)
}


make_timestep_multi_plot = function(data, fitness_data, time_series_data, trial_list, n_col = 2, n_row = 3){
  
  plot_list = list()
  
  i = 1
  for(trial in trial_list){
    plot_list[[i]] = make_timestep_plot(
      data = data, 
      fitness_data = fitness_data, 
      time_series_data = time_series_data,
      runs = trial[1], agents = trial[2], trials = trial[3]
    )
    
    i = i + 1
  }
  
  plot = ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = n_col, nrow = n_row,common.legend = T, labels = "auto" 
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

time_series_plot = function(data, seperator){
  
  if(isFALSE(seperator)){
    density = geom_density()
  } else {
    data$seperator = as.factor(data[,..seperator][[1]])
    density = geom_density(aes(color = seperator, fill = seperator), size = 1, alpha = .3)
  }
  
  plot = ggplot(data, aes(x = cor)) +
    facet_wrap(~lag, labeller = label_both) +
    theme_minimal() +
    geom_vline(xintercept=0) + 
    labs(y = " ", x = "Correlation coeficient", color = " ", fill = " ") +
    xlim(c(-1,1)) +
    density + colors + fills
  
  return(plot)
}

make_time_series_plot = function(data, range = -6:5, seperator = F, runs = 0:49, agents = 0:120, trials = 0:127, fitness_groups = F) {
  
  plot_data = time_series_plot_data(data, range, runs, agents, trials, fitness_groups)
  
  plot = time_series_plot(plot_data, seperator)
  
  return(plot)
}

#### GOAL PRIOR PLOT ####

make_goal_prior_plot_data = function(data){
  
  data$task_type = mapvalues(data$task_type, from = c("avoid", "catch"), to = c("Avoid","Catch"))
  data$block_movement = mapvalues(data$block_movement, from = c(-1, 1), to = c("Left", "Right"))
  
  data$run = paste0("", data$run+1)
  
  data$Probability = exp(data$surprisal * -1)
  
  data$split_column = paste(data$task_type, "/", data$block_movement)
  
  split_data = split(data, data$split_column)
  
  return(split_data)
}

make_sub_goal_prior_plot = function(data){
  
  title = data$split_column[1]
  
  x_labels = c(rbind(as.character(seq(1,33,2)), rep("",17)))[1:33]
  
  plot = ggplot(data, aes(x = as.factor(timestep), y = sensory_state, fill = Probability)) +
    geom_tile() +
    facet_grid(run ~ .) +
    labs(title = title, x = " ", y = " ") +
    theme_pubclean() +
    scale_fill_gradientn(colors = c("gray", "white", color_palette[5], color_palette[6]),
                         values=rescale(c(0,0.25,0.75,1)),
                         limits=c(0,1)
                         ) + 
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 8),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size = 10, face = "bold", color = "#333333"),
          legend.title = element_text(color = "#333333", size = 10, vjust = 0.80),
          legend.text = element_text(size = 8)
          ) +
    scale_x_discrete(breaks = seq(1,33,1), labels = x_labels)
  
  return(plot)
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
      
      Phi_median_mean = median(sub_data_timestep$Phi),
      Phi_median_se = 0,
      surprisal_median_mean = median(sub_data_timestep$surprisal),
      surprisal_median_se = 0,
      
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
      
      Phi_median_mean_event = median(sub_data_event$Phi),
      Phi_median_se_event = 0,
      surprisal_median_mean_event = median(sub_data_event$surprisal),
      surprisal_median_se_event = 0,
      
      timestep = timestep
    )
    
    i = i + 1
  }
  
  averaged_data = do.call(rbind, data_list)
  
  return(averaged_data)
}

make_average_trial_data = function(data, fitness_data, time_series_data, task_number, 
                                   group_list = c("all", "perfect", "Negative", "Neutral", "Positive", "trial_Negative", "trial_Neutral", "trial_Positive")){
  
  data = data[agent==120]
  time_series_data = time_series_data[agent == 120 & task == paste("Task",task_number) & lag == 0]
  
  # make event-related timestep column
  data$trial_id = paste0("r", data$run, "a", data$agent, "t", data$trial)
  split_data = split(data, data$trial_id)

  for (i in 1:length(split_data)){
    split_data[[i]]$event_timestep = split_data[[i]]$timestep - which(split_data[[i]]$first_sight==1)
  }
  
  data = do.call(rbind, split_data)
  
  data_merge = merge(data, time_series_data, by = c("run", "trial"))

  data_merge$trial_profile = NA
  data_merge$trial_profile[data_merge$cor<(-0.1)] = "trial_Negative"
  data_merge$trial_profile[data_merge$cor>(-0.1)&data_merge$cor<0.1] = "trial_Neutral"
  data_merge$trial_profile[data_merge$cor>0.1] = "trial_Positive"
  
  profile_data = time_series_data[,.(cor=mean(cor, na.rm=T)), by = run]
  profile_data$profile = NA
  profile_data$profile[profile_data$cor<(-0.1)] = "Negative"
  profile_data$profile[profile_data$cor>(-0.1)&profile_data$cor<0.1] = "Neutral"
  profile_data$profile[profile_data$cor>0.1] = "Positive"
  
  # Make averages
  data_list = list()
  i = 1
  
  for(group in group_list){
    
    if(group %in% c("all","perfect")){
      use_data = data
    } else {
      use_data = data_merge
    }
    
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
    
    if(group %in% c("trial_Negative", "trial_Neutral", "trial_Positive")){
      use_data = subset(use_data, trial_profile == group)
    }
    
    if(nrow(use_data)>0){
      average_data = average_across_trials(use_data)
      
      average_data$group = group
      
      data_list[[i]] = average_data
      
      i = i + 1
    }
    
    
    
  }
  
  data = do.call(rbind, data_list)
  
  return(data)
}

average_trial_plot = function(data, variable, event, title, subtitle, profile = F, median){
  
  if (variable == "surprisal_median"){
    y_limit = c(0,2.6)
    y_label = "Median surprisal"
  } 
  if (variable == "surprisal") {
    y_limit = c(0.15,2.4)
    y_label = "Average surprisal"
  }
  
  if (variable == "Phi_median"){
    y_limit = c(0,0.6)
    y_label = "Median Phi"
  } 
  if (variable == "Phi"){
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

make_average_trial_plot = function(timestep_data_task1, timestep_data_task4, fitness_task1, fitness_task4, time_series_data, median = F){
  
  group_list = c("all", "perfect", "trial_Negative", "trial_Neutral", "trial_Positive")
  
  task1_data = make_average_trial_data(timestep_data_task1, fitness_task1, time_series_data, 1, group_list)
  task4_data = make_average_trial_data(timestep_data_task4, fitness_task4, time_series_data, 4, group_list)
  
  trial_profile_data = subset(task4_data, group %in% c("trial_Negative", "trial_Neutral", "trial_Positive"))
  trial_profile_data$seperator = trial_profile_data$group
  
  trial_profile_data = as.data.table(trial_profile_data)
  
  # renaming seperator values
  trial_profile_data[.(seperator = c("trial_Negative", "trial_Neutral", "trial_Positive"), 
                       to = c("Negative", "Neutral", "Positive")), 
                     on = "seperator", seperator := i.to]
  
  
  task1_data = subset(task1_data, group == "all" )
  task4_data_perfect = subset(task4_data, group == "perfect" )
  task4_data = subset(task4_data, group == "all" )
  
  task1_data$seperator = "Easy task"
  task4_data$seperator = "Hard task"
  task4_data_perfect$seperator = "Hard task - Perfect"
  
  task_data = rbind(task1_data, task4_data, task4_data_perfect)
  
  if (median){
    Phi = "Phi_median"
    surprisal = "surprisal_median"
  } else {
    Phi = "Phi"
    surprisal = "surprisal"
  }
  
  task_split_plot = ggpubr::ggarrange(
    average_trial_plot(task_data, surprisal, event=F, title = "Surprisal", subtitle = "Averaged over trials", median=median),
    average_trial_plot(task_data, surprisal, event=T, title = "Surprisal", subtitle = "Relative to first sight event", median=median),
    average_trial_plot(task_data, Phi, event=F, title = "Phi", subtitle = "Averaged over trials", median=median),
    average_trial_plot(task_data, Phi, event=T, title = "Phi", subtitle = "Relative to first sight event", median=median),
    ncol = 2, nrow = 2, labels = "auto", common.legend = T
  )
  
  profile_split_plot = ggpubr::ggarrange(
    average_trial_plot(trial_profile_data, surprisal, event=F, title = "Surprisal", subtitle = "Averaged over trials", profile = T, median=median),
    average_trial_plot(trial_profile_data, surprisal, event=T, title = "Surprisal", subtitle = "Relative to first sight event", profile = T, median=median),
    average_trial_plot(trial_profile_data, Phi, event=F, title = "Phi", subtitle = "Averaged over trials", profile = T, median=median),
    average_trial_plot(trial_profile_data, Phi, event=T, title = "Phi", subtitle = "Relative to first sight event", profile = T, median=median),
    ncol = 2, nrow = 2, labels = "auto", common.legend = T
  )
  
  return(
    list(
      task_split_plot,
      profile_split_plot
    )
  )
}

average_trial_all_runs_plot = function(timestep_data_task1, timestep_data_task4, time_series_data, fitness_data){
  
  
  data = subset(timestep_data_task4, agent == 120)
  
  
  data$trial_id = paste0("r", data$run, "a", data$agent, "t", data$trial)
  split_data = split(data, data$trial_id)
  
  for (i in 1:length(split_data)){
    split_data[[i]]$event_timestep = split_data[[i]]$timestep - which(split_data[[i]]$first_sight==1)
  }
  
  data = do.call(rbind, split_data)
  
  
  time_series_data = subset(time_series_data, task == "Task 4" & lag == 0)
  
  profile_data = dplyr::summarise(group_by(time_series_data, run), cor = mean(cor))
  profile_data$profile = NA
  profile_data$profile[profile_data$cor<(-0.1)] = "Negative"
  profile_data$profile[profile_data$cor>(-0.1)&profile_data$cor<0.1] = "Neutral"
  profile_data$profile[profile_data$cor>0.1] = "Positive"
  
  data = dplyr::summarise(group_by(data, run, event_timestep), Phi = mean(Phi), surprisal = mean(surprisal))
  
  plot_data = merge(data, profile_data, by = "run")
  
  plot1 = ggplot(plot_data, aes(x = event_timestep, y = Phi, color = as.character(run))) +
    geom_vline(xintercept=0, linetype = "dashed", color = "darkgray") +
    lims(x = c(-7,30)) +
    theme_classic() +
    geom_line(size = 0.3, alpha=0.3) +
    facet_wrap(~profile) +
    theme(legend.position='none') +
    labs(x = "Relative timestep") 
  
  plot2 = ggplot(plot_data, aes(x = event_timestep, y = surprisal, color = as.character(run))) +
    geom_vline(xintercept=0, linetype = "dashed", color = "darkgray") +
    lims(x = c(-7,30)) +
    theme_classic() +
    geom_line(size = 0.3, alpha=0.3) +
    facet_wrap(~profile) +
    theme(legend.position='none') +
    labs(x = "Relative timestep") 
  
  
  data1 = subset(timestep_data_task1, agent == 120)
  data1$trial_id = paste0("r", data1$run, "a", data1$agent, "t", data1$trial)
  split_data1 = split(data1, data1$trial_id)
  for (i in 1:length(split_data1)){
    split_data1[[i]]$event_timestep = split_data1[[i]]$timestep - which(split_data1[[i]]$first_sight==1)
  }
  data1 = do.call(rbind, split_data1)
  data1 = dplyr::summarise(group_by(data1, run, event_timestep), Phi = mean(Phi), surprisal = mean(surprisal))

  
  data$task = "Hard task"
  data1$task = "Easy task"
  
  perfect_runs = fitness_data[fitness_data$agent == 120 & fitness_data$fitness == 1,]$run
  data_perfect = subset(data, run %in% perfect_runs)
  data_perfect$task = "Hard task - Perfect"
  
  plot_data = rbind(data, data1, data_perfect)
  
  plot3 = ggplot(plot_data, aes(x = event_timestep, y = Phi, color = as.character(run))) +
    geom_vline(xintercept=0, linetype = "dashed", color = "darkgray") +
    lims(x = c(-7,30)) +
    theme_classic() +
    geom_line(size = 0.3, alpha=0.3) +
    facet_wrap(~task) +
    theme(legend.position='none') +
    labs(x = "Relative timestep") 
  
  plot4 = ggplot(plot_data, aes(x = event_timestep, y = surprisal, color = as.character(run))) +
    geom_vline(xintercept=0, linetype = "dashed", color = "darkgray") +
    lims(x = c(-7,30)) +
    theme_classic() +
    geom_line(size = 0.3, alpha=0.3) +
    facet_wrap(~task) +
    theme(legend.position='none') +
    labs(x = "Relative timestep") 
  
  
  final_plot = ggpubr::ggarrange(
    plot1, plot2, plot3, plot4,
    ncol = 1, nrow = 4, labels = "auto"
  )
  
    
  return(final_plot)
}

make_average_trial_plot_LOD = function(timestep_data_task4, fitness_task4, time_series_data){
  
  data_list= list()
  i = 1
  for (r in unique(timestep_data_task4$run)){
    temp_fitness = fitness_task4[run==r]
    temp_data = timestep_data_task4[run==r]
    temp_time_series = time_series_data[run==r & task == "Task 4"]
    if (nrow(temp_time_series) > 0){
      task4_data = make_average_trial_data(temp_data, temp_fitness, temp_time_series, 4, c("trial_Negative", "trial_Neutral", "trial_Positive"))
      task4_data$run = r
      data_list[[i]] = task4_data
      i = 1 + i
    }
  }
  
  plot_data = do.call(rbind, data_list)
  
  trial_profile_data = subset(plot_data, group %in% c("trial_Negative", "trial_Neutral", "trial_Positive"))
  trial_profile_data$seperator = trial_profile_data$group
  
  trial_profile_data = as.data.table(trial_profile_data)
  
  #renaming seperator values
  trial_profile_data[.(seperator = c("trial_Negative", "trial_Neutral", "trial_Positive"), 
                       to = c("Negative", "Neutral", "Positive")), 
                     on = "seperator", seperator := i.to]
  
  
  
  plot = average_trial_plot(trial_profile_data, "Phi", event=T, title = " ", subtitle = " ", profile = T, median=F) +
    facet_wrap(~run, ncol=6) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    lims(y = c(0,3))
  
  plot2 = average_trial_plot(trial_profile_data, "surprisal", event=T, title = " ", subtitle = " ", profile = T, median=F) +
    facet_wrap(~run, ncol=6) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    lims(y = c(0,3.6))
  
  return(list(plot,plot2))
}



#### DISTRIBUTION PLOT ####

distribution_plot = function(timestep_data_task1,timestep_data_task4, fitness_data){
  
  data1 = timestep_data_task1[agent==120, c("Phi", "surprisal")]
  data1$Task = "Easy task"
  data4 = timestep_data_task4[agent==120, c("Phi", "surprisal")]
  data4$Task = "Hard task"
  
  perfect_runs = fitness_data[agent == 120 & fitness == 1, run]
  data4_perfect = timestep_data_task4[agent==120 & run %in% perfect_runs, c("Phi", "surprisal")]
  data4_perfect$Task = "Hard task\nperfect"
  
  data = rbind(data1,data4,data4_perfect)
  
  boxplot = geom_half_boxplot(aes(fill = Task), alpha = 0.3, side = "r", outlier.size = 0.1, width = .3)
  
  plot1 = ggplot(data, aes(y = Phi, x = Task)) +
    
    stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 1, linetype = "solid", color = "#333333", size = .3) +
    
    geom_half_violin(aes(fill = Task), color = "white", width = 2) +
    
    boxplot +
    
    stat_summary(fun="mean",color="darkgray", size = .2) +
    
    theme_classic() +
    theme(legend.position="none") +
    labs(x = " ", color = " ", fill = " ") +
    fills
  
  plot2 = ggplot(data, aes(y = surprisal, x = Task)) +
    
    stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 1, linetype = "solid", color = "#333333", size = .3) +
    
    geom_half_violin(aes(fill = Task), color = "white", width = 2) +
    
    boxplot + 
    
    stat_summary(fun="mean",color="darkgray", size = .2) +
    
    theme_classic() + 
    theme(legend.position="none") +
    labs(x = " ", y = "Surprisal", color = " ", fill = " ") +
    fills
    
  plot = ggpubr::ggarrange(
    plot1,plot2,
    ncol = 2, nrow = 1, labels = "auto"
  )

  return(plot)
}

