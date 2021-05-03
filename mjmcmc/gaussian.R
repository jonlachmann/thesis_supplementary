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


load("data/full_enumeration/gaussian/10K/full_10Kg.Rdata")
load("data/mjmcmc/gaussian/10K/run530_mjmcmc_10Kg5_1.Rdata")

matt <- matrix(NA, 15,3)
matt[,1] <- marginal.probs.renorm(full_10Kg)
matt[,2] <- marginal.probs.renorm(run530_mjmcmc_10Kg5_1$models)
matt[,3] <- GMJMCMC:::marginal.probs(run530_mjmcmc_10Kg5_1$models)
multiplot(matt, legend=T)


model.size <- length(models[[1]]$model)
models.matrix <- matrix(unlist(models), ncol=model.size+3, byrow=T)
models.matrix <- rbind(models.matrix, models.matrix)
models.matrix[21:40,17] <- models.matrix[21:40,17]-10

models.matrix.nondup <- models.matrix[(!duplicated(models.matrix[,2:(model.size+1)], dim=1, fromLast=T)),]


marginal.probs.renorm <- function (models) {
  model.size <- length(models[[1]]$model)
  models.matrix <- matrix(unlist(models), ncol=model.size+3, byrow=T)
  models.matrix <- models.matrix[(!duplicated(models.matrix[,2:(model.size+1)], dim=1, fromLast=T)),]
  max_mlik <- max(models.matrix[,(model.size+2)])
  crit.sum <- sum(exp(models.matrix[,(model.size+2)]-max_mlik))
  probs <- matrix(NA,1,model.size)
  for (i in 2:(model.size+1)) probs[i-1] <- sum(exp(models.matrix[as.logical(models.matrix[,i]),(model.size+2)]-max_mlik))/crit.sum
  return(probs)
}