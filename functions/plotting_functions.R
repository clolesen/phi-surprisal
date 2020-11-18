library(ggplot2)
library(data.table)
library(ggpubr)
library(tidyverse)


# colors used in plots
# 1: Black
# 2: Blue
# 3: Red
# 4: Green
# 5: Yellow
# 6: Purple
color_palette = c("black", "#3498DB", "#E53935", "#27AE60", "#f0e442", "#490092")
colors = scale_color_manual(values=palette)
fills = scale_fill_manual(values=palette)


#### AVERAGE PLOTS ####
make_average_plot_data = function(data, variable){
  
  y = data[,..variable][[1]]
  
  # se = standard error
  variable_se = paste0(variable, "_se")
  se = data[,..variable_se][[1]]
  
  
  plot_data = data.table(
    x = data$generation,
    y = y,
    se_min = y - se,
    se_max = y + se,
    task = data$task
  )
  
  return(plot_data)
}

LOD_plot = function(plot_data, y_label, x_label, title){
  
  plot = ggplot(plot_data, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = se_min, ymax = se_max, fill = task), color = F, alpha = 0.1) +
    geom_line(aes(color = task)) +
    theme_classic() +
    labs(y = y_label, x = x_label, title = title, legend = "Task") +
    colors + fills
  
  return(plot)
}

make_LOD_plot = function(data, variable, y_label = " ", x_label = "Generation", title =" "){
  
  plot_data = make_average_plot_data(data, variable)
  
  plot = LOD_plot(plot_data, y_label, x_label, title)
  
  return(plot)
}

make_surprisal_matrix_plot = function(data, cond){
  
  plot = ggarrange(
    make_LOD_plot(data, paste0("surprisal_system_",cond,"_all"), y_label = "Surprisal - All runs", x_label = " ", title = "Full system states"),
    make_LOD_plot(data, paste0("surprisal_blanket_",cond,"_all"), x_label = " ", title = "Blanket states"),
    make_LOD_plot(data, paste0("surprisal_internal_",cond,"_all"), x_label = " ", title = "Internal states"),
    
    make_LOD_plot(data, paste0("surprisal_system_",cond,"_LOD"), y_label = "Surprisal - LOD", x_label = " "),
    make_LOD_plot(data, paste0("surprisal_blanket_",cond,"_LOD"), x_label = " "),
    make_LOD_plot(data, paste0("surprisal_internal_",cond,"_LOD"), x_label = " "),
    
    make_LOD_plot(data, paste0("surprisal_system_",cond,"_animat"), y_label = "Surprisal - Animat"),
    make_LOD_plot(data, paste0("surprisal_blanket_",cond,"_animat")),
    make_LOD_plot(data, paste0("surprisal_internal_",cond,"_animat")),
    
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


#### TIMESTEP PLOTS ####

make_timestep_plot_data = function(data, variables, runs, agents, trials){
  
  sub_data = subset(data, run %in% runs & agent %in% agents & trial %in% trials)
  
  columns = c("run", "agent", "trial", "timestep", variables)
  
  plot_data = sub_data[,..columns]
  
  return(plot_data)
}

normalize_surprisal = function(plot_data){
  
  n_columns = ncol(plot_data)
  
  # check for surprisal columns and divide them by 10
  for (column in 5:n_columns){
    name = names(plot_data[,..column]) 
    split_name = strsplit(name, "_")[[1]]
    if (split_name[1] == "surprisal") plot_data[,column] = plot_data[,..column]/5
  }
  
  return(plot_data)
}

timestep_line_plot = function(plot_data, facet, variables) {
  
  n_columns = ncol(plot_data)
  
  plot = ggplot(plot_data, aes(x = timestep)) +
    geom_segment(y =-.1, yend =-.1, x = 0, xend = 34, color = "black", size = 1) +
    facet_wrap(as.formula(paste("~", facet)), scales = "free") +
    theme_void() +
    theme(
      panel.grid.minor.x = element_line(colour = "lightgray", linetype = "dotted"),
      strip.background = element_rect(colour = "white", fill = "#eeeeee"),
      strip.text.x = element_text(margin = margin(3,0,3,0)),
      panel.spacing = unit(1, "lines"),
      plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
    scale_x_continuous(breaks = seq(1,33,1))

  i = 2
  for (var in variables){
    plot = plot + geom_line(aes_string(y = var), color = color_palette[i], size=1)
    i = i + 1
  }
  
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

add_tile_to_timestep_plot = function(plot, plot_data, variables){
  
  tile_plot_data = convert_to_tile_plot_data(plot_data, variables)
  
  y_values = unique(tile_plot_data$y)
  
  for (value in y_values){
    plot = plot + geom_segment(y = value, yend = value, x = -1.5, xend = 34, colour = "gray")
  }
  
  plot = plot + 
    geom_vline(xintercept=c(0,34), linetype="solid", color = "black") +
    theme(legend.position = "none", axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 8)) +
    scale_alpha_continuous(range = c(0,1)) +
    scale_y_continuous(breaks = y_values, labels = variables, limits = c(min(y_values), 3)) +
    geom_tile(data = tile_plot_data,
              aes(y = y, alpha = value),
              height = .1)
                     
  return(plot)           
}

make_timestep_plot = function(data, line_variables, tile_variables, facet, runs = 0:49, agents = 0:120, trials = 0:127){
  
  #LINE PLOT
  line_plot_data = make_timestep_plot_data(data, line_variables, runs, agents, trials)
  
  line_plot_data = normalize_surprisal(line_plot_data)
  
  line_plot = timestep_line_plot(line_plot_data, facet, line_variables)
  
  # TILE PLOT
  tile_plot_data = make_timestep_plot_data(data, tile_variables, runs, agents, trials)
  
  full_plot = add_tile_to_timestep_plot(line_plot, tile_plot_data, tile_variables)
  
  return(full_plot)
}

