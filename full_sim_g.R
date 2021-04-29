# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Load packages
source("packages.R")
# Simulate data
source("sim_data1.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

# Calculate the full model set using 0.1, 0.25, 0.5, 1, 5, and 10% at each iteration
#full_10K_sub_01 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.001)
#full_10K_sub_025 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.0025)
#full_10K_sub_05 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.05)
system.time(full_10Kg_sub_1 <- run_sim(million_x, as.vector(million_y_g), gaussian.loglik.aic.irlssgd, 10000, 1:1000, 0.01))
system.time(full_10Kg <- run_sim(million_x, million_y_g, gaussian.loglik.aic, 10000, 1:1000, 0.01))
#full_10K_sub_5 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.05)
#full_10K_sub_10 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.1)

load("data/full_10K_g.Rdata")

full_10Kg_sub_1_mat <- matrix(unlist(full_10Kg_sub_1), ncol=18, byrow=T)
full_10K_g_mat <- matrix(unlist(full_10Kg), ncol=18, byrow=T)

compare_g <- matrix(NA,1000,2)
compare_g[,1] <- full_10K_g_mat[1:1000,17]
compare_g[,2] <- full_10Kg_sub_1_mat[1:1000,17]

par(mfrow=c(1,1))
multiplot(compare_g)

cor(compare_g)

load("data/full_enumeration/logistic/full_1M_005.Rdata")
full_1M_005 <- unlist(full_1M_005, recursive = F)
save(full_1M_005, file="data/full_enumeration/logistic/full_1M_005.Rdata")


