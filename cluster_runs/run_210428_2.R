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

model_partitions <- matrix(1:full_model_count, 64, 512)

full_100K_res <- mclapply(1:64, function (x) {
  run_sim(logistic.loglik.aic, 100000, model_partitions[x,], 1)
}, mc.cores = num_cores)

save(full_100K_res, file="full_100K_res.Rdata")