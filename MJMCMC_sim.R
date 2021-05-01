# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-27

# Load packages
source("packages.R")
# Simulate data
source("logis_sim_data.R")
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
#save(mjmcmc_10K, file="data/mjmcmc/mjmcmc_10K")
load(file="data/mjmcmc_sim/mjmcmc_10K.Rdata")

mjmcmc_10K_conv <- vector("list", 20)
for (i in 1:20) {
  if (!is.null(mjmcmc_10K[[i]])) {
    mjmcmc_10K_conv[[i]] <- rmse_conv(full_10K_renorm, mjmcmc_10K[[i]], 66)
  }
}

quantmeans <- matrix_quantmean(mjmcmc_10K_conv)
par(mfrow=c(3,5))
for (i in 1:15) {
  plot(-10, xlim=c(0,66), ylim=c(0,max(quantmeans$high[,i])), ylab="RMSE", xlab="Iterations")
  polygon(c(1:66, 66:1), c(quantmeans$low[,i], rev(quantmeans$high[,i])),
        col="lightgrey", border=NA)
  lines(quantmeans$mean[,i])
}


plot(quantmeans$high[,2])

multiplot(rowMeans(quantmeans$mean))
