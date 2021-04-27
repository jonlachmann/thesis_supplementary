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

data_10K <- cbind(million_y_l, million_x)[1:10000,]

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_10K)

mjmcmc_10K <- mjmcmc(cbind(million_y_l, million_x)[1:10000,], logistic.loglik.aic, 5000, sim_probs, sim_pars)