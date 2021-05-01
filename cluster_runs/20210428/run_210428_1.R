# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load packages
source("../../packages.R")
# Simulate data
source("../../logis_sim_data.R")
# Load likelihood functions
source("../../likelihoods1.R")
# Load common functions
source("../../functions.R")

num_cores <- detectCores()

data_10K <- cbind(million_y_l, million_x)[1:10000,]

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_10K)

options(warn=1)

mjmcmc_10K_res <- mclapply(1:7, function (x) {
  mjmcmc(data_10K, logistic.loglik.aic, 6554, sim_probs, sim_pars)
}, mc.cores = num_cores)

save(mjmcmc_10K_res, file= "../../mjmcmc_10K_res.Rdata")