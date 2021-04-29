# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load packages
source("../../packages.R")
# Simulate data
source("../../sim_data1.R")
# Load likelihood functions
source("../../likelihoods1.R")
# Load common functions
source("../../functions.R")

num_cores <- detectCores()

model_partitions <- matrix(1:full_model_count, 128, 256, byrow=T)

full_1M_res <- mclapply(1:128, function (x) {
  run_sim(logistic.loglik.aic, 1000000, model_partitions[x,], 1)
}, mc.cores = num_cores)

save(full_1M_res, file="full_1M_res.Rdata")