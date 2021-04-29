# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load packages
source("packages.R")
# Simulate data
source("gauss_sim_data.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

num_cores <- detectCores()

options(warn=1)

model_partitions <- matrix(1:full_model_count, 32, byrow=T)

full_100Kg <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik, 100000, model_partitions[x,], 1)
}, mc.cores = num_cores)
full_100Kg <- unlist(full_100Kg, recursive = F)
save(full_100Kg, file="full_100K_g.Rdata")

full_100Kg_005 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.0005)
}, mc.cores = num_cores)
full_100Kg_005 <- unlist(full_100Kg_005, recursive = F)
save(full_100Kg_005, file="full_100Kg_005.Rdata")

full_100Kg_01 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.001)
}, mc.cores = num_cores)
full_100Kg_01 <- unlist(full_100Kg_01, recursive = F)
save(full_100Kg_01, file="full_100Kg_01.Rdata")

full_100Kg_025 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.0025)
}, mc.cores = num_cores)
full_100Kg_025 <- unlist(full_100Kg_025, recursive = F)
save(full_100Kg_025, file="full_100Kg_025.Rdata")

full_100Kg_05 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.005)
}, mc.cores = num_cores)
full_100Kg_05 <- unlist(full_100Kg_05, recursive = F)
save(full_100Kg_05, file="full_100Kg_05.Rdata")

full_100Kg_075 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.0075)
}, mc.cores = num_cores)
full_100Kg_075 <- unlist(full_100Kg_075, recursive = F)
save(full_100Kg_075, file="full_100Kg_075.Rdata")

full_100Kg_1 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.01)
}, mc.cores = num_cores)
full_100Kg_1 <- unlist(full_100Kg_1, recursive = F)
save(full_100Kg_1, file="full_100Kg_1.Rdata")

full_100Kg_5 <- mclapply(1:32, function (x) {
  run_sim(mill_x_g, mill_y_g100K, linear.g.prior.loglik.irlssgd, 100000, model_partitions[x,], 0.05)
}, mc.cores = num_cores)
full_100Kg_5 <- unlist(full_100Kg_5, recursive = F)
save(full_100Kg_5, file="full_100Kg_5.Rdata")