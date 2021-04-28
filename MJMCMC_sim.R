# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-27

# Load packages
source("packages.R")
# Simulate data
source("sim_data1.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

# Load the full simulation
load(file="data/full_10K.Rdata")

# Calculate the full renormalized baseline
full_10K_renorm <- GMJMCMC:::marginal.probs.renorm(full_10K)

data_10K <- cbind(million_y_l, million_x)[1:10000,]

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_10K)

#mjmcmc_10K <- mjmcmc(cbind(million_y_l, million_x)[1:10000,], logistic.loglik.aic, 5000, sim_probs, sim_pars)
#save(mjmcmc_10K, file="data/mjmcmc_sim/mjmcmc_10K")
load(file="data/mjmcmc_sim/mjmcmc_10K")

mjmcmc_10K_conv <- rmse_conv(full_10K_renorm, mjmcmc_10K, 50)
multiplot(rowMeans(mjmcmc_10K_conv))

library(RCurl)

ftpUpload("data/full_10K.Rdata", "ftp://jon:mar71him@jonlachmann.se/full_10K.Rdata")
