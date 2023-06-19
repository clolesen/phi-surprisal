library(data.table)
library(lme4)
library(lmerTest)

timestep_data_task4 = fread("processed_data/timestep_data_task4.csv")
last_gen = timestep_data_task4[agent==120]

Phi = last_gen$Phi
n = length(Phi)
last_gen$Phi_difference = abs(last_gen$Phi - c(0,Phi[1:n-1]))

suprisal = last_gen$surprisal
last_gen$surprisal_difference = abs(last_gen$surprisal - c(0,suprisal[1:n-1]))

anlysis_data = last_gen[timestep != 1]

model = lmer(Phi_difference ~ surprisal_difference + (1|trial) + (surprisal_difference|run),
             data = anlysis_data)
summary(model)
