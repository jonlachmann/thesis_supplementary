# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

# Load packages, data, likelihoods and general functions
source("packages.R")
source("gauss_sim_data.R")
source("likelihoods1.R")
source("functions.R")

subs_list <- c(0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

data_10Kg <- cbind(mill_y_g10K, mill_x_g)[1:10000,]
data_10Kl <- cbind(mill_y_l10K, mill_x_g)[1:10000,]

data_100Kg <- cbind(mill_y_g100K, mill_x_g)[1:100000,]
data_100Kl <- cbind(mill_y_l100K, mill_x_g)[1:100000,]

data_1Mg <- cbind(mill_y_g1M, mill_x_g)
data_1Ml <- cbind(mill_y_l1M, mill_x_g)

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_10Kg)

sim_probs$large <- 0
sim_pars$loglik$subs <- 0.0001

system.time(mjmcmc_100 <- mjmcmc(data_100Kl, logistic.loglik.bic.irlssgd, 50, sim_probs, sim_pars, T))
system.time(mjmcmc_100 <- mjmcmc(data_100Kl, logistic.loglik.bic, 50, sim_probs, sim_pars))
