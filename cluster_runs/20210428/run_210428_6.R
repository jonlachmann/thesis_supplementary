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
source("../../sim_data1.R")
# Load likelihood functions
source("../../likelihoods1.R")
# Load common functions
source("../../functions.R")

num_cores <- detectCores()

data_100K <- cbind(million_y_l, million_x)[1:100000,]
data_1M <- cbind(million_y_l, million_x)[1:1000000,]

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_10K)

options(warn=1)

mjmcmc_100K <- mclapply(1:20, function (x) {
  mjmcmc(data_100K, logistic.loglik.aic, 6554, sim_probs, sim_pars)
}, mc.cores = num_cores)

save(mjmcmc_100K, file="mjmcmc_100K.Rdata")

mjmcmc_1M <- mclapply(1:20, function (x) {
  mjmcmc(data_1M, logistic.loglik.aic, 6554, sim_probs, sim_pars)
}, mc.cores = num_cores)

save(mjmcmc_1M, file="mjmcmc_1M.Rdata")