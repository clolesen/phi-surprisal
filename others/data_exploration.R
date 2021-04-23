library(data.table)
#

#### Task 1 fitness ####
fitness_task1 = fread("data/fitness_task1.csv")

# Fitness on last agent
fitness_end = fitness_task1$fitness[fitness_task1$agent == 120]

# Number of agents haveing perfect fitness
sum(fitness_end == 1)

# The runs that have perfect fitness
perfect_runs = fitness_task1$run[fitness_task1$agent == 120][fitness_end == 1]

# Checking Phi on perfect runs (on last agent)
average_data_task1 = fread("data/no_exclusion/average_data_task1.csv")
mean_Phi_perfect = subset(average_data_task1, run %in% perfect_runs & agent == 120)$Phi_mean
n_perfect = length(mean_Phi_perfect)
sum(mean_Phi_perfect==0)
sum(mean_Phi_perfect<0.02)/n_perfect
sum(mean_Phi_perfect>0.02 & mean_Phi_perfect<0.3)/n_perfect
sum(mean_Phi_perfect>0.3 & mean_Phi_perfect<0.9)/n_perfect
sum(mean_Phi_perfect>0.9)/n_perfect
mean(mean_Phi_perfect)

# Checking Phi on all runs
mean_Phi = subset(average_data_task1, agent == 120)$Phi_mean
n = length(mean_Phi)
sum(mean_Phi<0.02)/n
sum(mean_Phi>0.02 & mean_Phi<0.3)/n
sum(mean_Phi>0.3 & mean_Phi<0.9)/n
sum(mean_Phi>0.9)/n
mean(mean_Phi)

# Checking Phi on non perfect runs
mean_Phi_non_perfect = subset(average_data_task1, !(run %in% perfect_runs) & agent == 120)$Phi_mean
n_non_perfect = length(mean_Phi_non_perfect)
sum(mean_Phi_non_perfect==0)
sum(mean_Phi_non_perfect<0.02)/n_non_perfect
sum(mean_Phi_non_perfect>0.02 & mean_Phi_non_perfect<0.3)/n_non_perfect
sum(mean_Phi_non_perfect>0.3 & mean_Phi_non_perfect<0.9)/n_non_perfect
sum(mean_Phi_non_perfect>0.9)/n_non_perfect
mean(mean_Phi_non_perfect)










#### Task 4 Phi level ####
average_data_task4 = fread("data/no_exclusion/average_data_task4.csv")
fitness_task4 = fread("data/fitness_task4.csv")

# Phi 0 agents
Phi0_task4 = subset(average_data_task4, Phi_mean == 0)

#Id
Phi0_task4$id = paste0("r",Phi0_task4$run,"a",Phi0_task4$agent)
fitness_task4$id = paste0("r",fitness_task4$run,"a",fitness_task4$agent)

# Maximum fitness while Phi is 0
max(subset(fitness_task4, id %in% Phi0_task4$id)$fitness)

# Mean fitness across all runs on last generation
mean(subset(fitness_task4, agent == 120)$fitness)

above_ids = subset(fitness_task4, !(id %in% Phi0_task4$id)& fitness > 0.93)$id
average_data_task4$id = paste0("r",average_data_task4$run,"a",average_data_task4$agent)
above_Phi = subset(average_data_task4, id %in% above_ids)$Phi_mean
plot(density(above_Phi, bw = .01))
plot(density(average_data_task4$Phi_mean, bw = .01))


#### Correlation ####

LOD_data_task4 = fread("data/no_exclusion/LOD_smoothed_data_task4.csv")
cor(LOD_data_task4$Phi, LOD_data_task4$fitness)

average_data_task4 = fread("data/no_exclusion/average_data_task4.csv")
Phi = subset(average_data_task4, agent == 120)$Phi_mean
fitness_task4 = fread("data/fitness_task4.csv")
fitness_task1 = fread("data/fitness_task1.csv")
fit = fitness_task4$fitness[fitness_task1$agent==120]
cor(Phi,fit, method = "spearman")


# BIG CORRELATION THING

cor_data_list = list()
i = 1

average_data_task1 = fread("data/no_exclusion/smoothed_average_data_task1.csv")
average_data_task4 = fread("data/no_exclusion/smoothed_average_data_task4.csv")

fitness_task1 = fread("data/fitness_task1.csv")
fitness_task4 = fread("data/fitness_task4.csv")

for(r in unique(average_data_task1$run)){
  d1 = subset(average_data_task1, run == r)
  d4 = subset(average_data_task4, run == r)
  
  f1 = subset(fitness_task1, run==r)
  f4 = subset(fitness_task4, run==r)
  
  cors= data.frame(
    fit_phi1 = cor(f1$fitness,d1$Phi_mean,method = "spearman"),
    fit_phi4 = cor(f4$fitness,d4$Phi_mean,method = "spearman"),
    
    fit_con1 = cor(f1$fitness,d1$n_concepts_mean,method = "spearman"),
    fit_con4 = cor(f4$fitness,d4$n_concepts_mean,method = "spearman"),
    
    fit_sur1 = cor(f1$fitness,d1$surprisal_system_cond1_all_mean,method = "spearman"),
    fit_sur4 = cor(f4$fitness,d4$surprisal_system_cond1_all_mean,method = "spearman"),
    
    fit_b_sur1 = cor(f1$fitness,d1$surprisal_blanket_cond1_all_mean,method = "spearman"),
    fit_b_sur4 = cor(f4$fitness,d4$surprisal_blanket_cond1_all_mean,method = "spearman"),
    
    fit_i_sur1 = cor(f1$fitness,d1$surprisal_internal_cond1_all_mean,method = "spearman"),
    fit_i_sur4 = cor(f4$fitness,d4$surprisal_internal_cond1_all_mean,method = "spearman"),
    
    fit_a_sur1 = cor(f1$fitness,d1$surprisal_system_cond1_animat_mean,method = "spearman"),
    fit_a_sur4 = cor(f4$fitness,d4$surprisal_system_cond1_animat_mean,method = "spearman"),
    
    fit_a_b_sur1 = cor(f1$fitness,d1$surprisal_blanket_cond1_animat_mean,method = "spearman"),
    fit_a_b_sur4 = cor(f4$fitness,d4$surprisal_blanket_cond1_animat_mean,method = "spearman"),
    
    fit_a_i_sur1 = cor(f1$fitness,d1$surprisal_internal_cond1_animat_mean,method = "spearman"),
    fit_a_i_sur4 = cor(f4$fitness,d4$surprisal_internal_cond1_animat_mean,method = "spearman"),
    
    fit_r_sur1 = cor(f1$fitness,d1$surprisal_system_cond1_LOD_mean,method = "spearman"),
    fit_r_sur4 = cor(f4$fitness,d4$surprisal_system_cond1_LOD_mean,method = "spearman"),
    
    fit_r_b_sur1 = cor(f1$fitness,d1$surprisal_blanket_cond1_LOD_mean,method = "spearman"),
    fit_r_b_sur4 = cor(f4$fitness,d4$surprisal_blanket_cond1_LOD_mean,method = "spearman"),
    
    fit_r_i_sur1 = cor(f1$fitness,d1$surprisal_internal_cond1_LOD_mean,method = "spearman"),
    fit_r_i_sur4 = cor(f4$fitness,d4$surprisal_internal_cond1_LOD_mean,method = "spearman"),
    
    ###########
    
    phi_sur1 = cor(d1$Phi_mean,d1$surprisal_system_cond1_all_mean,method = "spearman"),
    phi_sur4 = cor(d4$Phi_mean,d4$surprisal_system_cond1_all_mean,method = "spearman"),
    
    phi_b_sur1 = cor(d1$Phi_mean,d1$surprisal_blanket_cond1_all_mean,method = "spearman"),
    phi_b_sur4 = cor(d4$Phi_mean,d4$surprisal_blanket_cond1_all_mean,method = "spearman"),
    
    phi_i_sur1 = cor(d1$Phi_mean,d1$surprisal_internal_cond1_all_mean,method = "spearman"),
    phi_i_sur4 = cor(d4$Phi_mean,d4$surprisal_internal_cond1_all_mean,method = "spearman"),
    
    phi_a_sur1 = cor(d1$Phi_mean,d1$surprisal_system_cond1_animat_mean,method = "spearman"),
    phi_a_sur4 = cor(d4$Phi_mean,d4$surprisal_system_cond1_animat_mean,method = "spearman"),
    
    phi_a_b_sur1 = cor(d1$Phi_mean,d1$surprisal_blanket_cond1_animat_mean,method = "spearman"),
    phi_a_b_sur4 = cor(d4$Phi_mean,d4$surprisal_blanket_cond1_animat_mean,method = "spearman"),
    
    phi_a_i_sur1 = cor(d1$Phi_mean,d1$surprisal_internal_cond1_animat_mean,method = "spearman"),
    phi_a_i_sur4 = cor(d4$Phi_mean,d4$surprisal_internal_cond1_animat_mean,method = "spearman"),
    
    phi_r_sur1 = cor(d1$Phi_mean,d1$surprisal_system_cond1_LOD_mean,method = "spearman"),
    phi_r_sur4 = cor(d4$Phi_mean,d4$surprisal_system_cond1_LOD_mean,method = "spearman"),
    
    phi_r_b_sur1 = cor(d1$Phi_mean,d1$surprisal_blanket_cond1_LOD_mean,method = "spearman"),
    phi_r_b_sur4 = cor(d4$Phi_mean,d4$surprisal_blanket_cond1_LOD_mean,method = "spearman"),
    
    phi_r_i_sur1 = cor(d1$Phi_mean,d1$surprisal_internal_cond1_LOD_mean,method = "spearman"),
    phi_r_i_sur4 = cor(d4$Phi_mean,d4$surprisal_internal_cond1_LOD_mean,method = "spearman"),
    
    blanket_internal_LOD_1 = cor(d1$surprisal_blanket_cond1_LOD_mean,d1$surprisal_internal_cond1_LOD_mean,method = "spearman"),
    blanket_internal_LOD_4 = cor(d4$surprisal_blanket_cond1_LOD_mean,d4$surprisal_internal_cond1_LOD_mean,method = "spearman"),
    
    blanket_internal_all_1 = cor(d1$surprisal_blanket_cond1_all_mean,d1$surprisal_internal_cond1_all_mean,method = "spearman"),
    blanket_internal_all_4 = cor(d4$surprisal_blanket_cond1_all_mean,d4$surprisal_internal_cond1_all_mean,method = "spearman"),
    
    blanket_internal_animat_1 = cor(d1$surprisal_blanket_cond1_animat_mean,d1$surprisal_internal_cond1_animat_mean,method = "spearman"),
    blanket_internal_animat_4 = cor(d4$surprisal_blanket_cond1_animat_mean,d4$surprisal_internal_cond1_animat_mean,method = "spearman")
    
    )
  
  cor_data_list[[i]] = cors
  i = i + 1
  
}

avg_cor_data = do.call(rbind, cor_data_list)

avg_cor_avg_list=list()
for(c in 1:46) {
  temp_data = avg_cor_data[,c]
  temp_data = temp_data[!(is.na(temp_data))] # only those that are not NA
  
  avg_cor_avg_list[[c]] = data.frame(
    name = names(avg_cor_data)[c],
    mean = mean(temp_data),
    se = sd(temp_data)/sqrt(length(temp_data))
  )
}
# one is removed from task 1 as it produces NA due to 0 phi all the way
avg_cor_avg = do.call(rbind, avg_cor_avg_list)
write.csv2(avg_cor_avg, "big_correltation_thing.csv")



# Looking at perfect animats in task 4
fitness_task4 = fread("data/fitness_task4.csv")

perfect_task4 = subset(fitness_task4, fitness == 1)
table(perfect_task4$run)








# CONSTRUCT DATA FRAME FOR NEW PIPELINE
fwrite(timestep_data_task1[,1:15], "data/data_for_new_pipeline_task1.csv")
fwrite(timestep_data_task4[,1:15], "data/data_for_new_pipeline_task4.csv")
fwrite(subset(timestep_data_task4[,1:15], agent == 100 & run %in% c(4,6)), "data/SMALL_data_for_new_pipeline_task4.csv")

