# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load packages
source("packages.R")
# Simulate data
source("sim_data1.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

num_cores <- detectCores()

model_partitions <- matrix(1:full_model_count, 32, byrow=T)

full_1M_001 <- mclapply(1:32, function (x) {
  run_sim(logistic.loglik.aic.irlssgd, 1000000, model_partitions[x,], 0.0001)
}, mc.cores = num_cores)

save(full_1M_001, file="full_1M_001.Rdata")

full_1M_005 <- mclapply(1:32, function (x) {
  run_sim(logistic.loglik.aic.irlssgd, 1000000, model_partitions[x,], 0.005)
}, mc.cores = num_cores)

save(full_1M_005, file="full_1M_005.Rdata")

full_1M_01 <- mclapply(1:32, function (x) {
  run_sim(logistic.loglik.aic.irlssgd, 1000000, model_partitions[x,], 0.001)
}, mc.cores = num_cores)

save(full_1M_01, file="full_1M_01.Rdata")

full_1M_05 <- mclapply(1:32, function (x) {
  run_sim(logistic.loglik.aic.irlssgd, 1000000, model_partitions[x,], 0.005)
}, mc.cores = num_cores)

save(full_1M_05, file="full_1M_05.Rdata")

full_1M_1 <- mclapply(1:32, function (x) {
  run_sim(logistic.loglik.aic.irlssgd, 1000000, model_partitions[x,], 0.01)
}, mc.cores = num_cores)

save(full_1M_1, file="full_1M_1.Rdata")