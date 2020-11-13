colors = scale_color_manual(values=c("black", "#3498DB", "#E53935", "#27AE60", "#f0e442", "#490092"))
fills = scale_fill_manual(values=c("black", "#3498DB", "#E53935", "#27AE60", "#f0e442", "#490092"))

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

plot_all_LOD_phi = function(data, task){
  
  plot_data = subset(data, task == task) 
  
  if (task == "task1") title = "Task 1" else title = "Task 4"
  
  plot = ggplot(plot_data, aes(x = agent, y = Phi_mean))+
    geom_line() +
    theme_minimal() +
    facet_wrap(~run, ncol = 5) +
    labs(y = "Average Phi", x = "Generation", title = paste0(title,": Average Phi over time for all agents seperatly"))
  
  return(plot)
}
