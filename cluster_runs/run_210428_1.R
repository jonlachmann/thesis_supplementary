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

mjmcmc_10K_res <- mclapply(1:20, function (x) {
  mjmcmc(cbind(million_y_l, million_x)[1:10000,], logistic.loglik.aic, 655, sim_probs, sim_pars)
}, mc.cores = num_cores)

save(mjmcmc_10K_res, file="mjmcmc_10K_res.Rdata")
ftpUpload("mjmcmc_10K_res.Rdata", paste0("ftp://jon:",args[1],"@jonlachmann.se/mjmcmc_10K_res.Rdata"))