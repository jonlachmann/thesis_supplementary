# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load packages
source("packages.R")
# Simulate data
source("sim_data1.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

num_cores <- detectCores()

options(warn=1)

model_partitions <- matrix(1:full_model_count, 32, byrow=T)

full_10Kg <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic, 10000, model_partitions[x,], 1)
}, mc.cores = num_cores)
full_10Kg <- unlist(full_10Kg, recursive = F)
save(full_10Kg, file="full_10K_g.Rdata")

full_10Kg_025 <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, model_partitions[x,], 0.0025)
}, mc.cores = num_cores)
full_10Kg_025 <- unlist(full_10Kg_025, recursive = F)
save(full_10Kg_025, file= "full_10Kg_025.Rdata")

full_10Kg_05 <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, model_partitions[x,], 0.005)
}, mc.cores = num_cores)
full_10Kg_05 <- unlist(full_10Kg_05, recursive = F)
save(full_10Kg_05, file="full_10Kg_05.Rdata")

full_10Kg_075 <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, model_partitions[x,], 0.0075)
}, mc.cores = num_cores)
full_10Kg_075 <- unlist(full_10Kg_075, recursive = F)
save(full_10Kg_075, file="full_10Kg_075.Rdata")

full_10Kg_1 <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, model_partitions[x,], 0.01)
}, mc.cores = num_cores)
full_10Kg_1 <- unlist(full_10Kg_1, recursive = F)
save(full_10Kg_1, file="full_10Kg_1.Rdata")

full_10Kg_5 <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, model_partitions[x,], 0.05)
}, mc.cores = num_cores)
full_10Kg_5 <- unlist(full_10Kg_5, recursive = F)
save(full_10Kg_5, file="full_10Kg_5.Rdata")

full_10Kg_10 <- mclapply(1:32, function (x) {
  run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, model_partitions[x,], 0.1)
}, mc.cores = num_cores)
full_10Kg_10 <- unlist(full_10Kg_10, recursive = F)
save(full_10Kg_10, file="full_10Kg_10.Rdata")