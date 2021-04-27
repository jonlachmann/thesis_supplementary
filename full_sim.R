# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

# Load packages
source("packages.R")
# Simulate data
source("sim_data1.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

### Logistic regression simulation

# Function for running simulations on many models with varying subsample size
run_sim <- function (loglik_fun, nobs, models, subs) {
  res <- vector("list", length(models))
  progress <- 0
  for (i in models) {
    modelvector <- as.logical(c(T,intToBits(i)[1:15]))
    loglik <- loglik_fun(million_y_l[1:nobs], million_x[1:nobs,], modelvector, NULL, list(subs = subs))
    res[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
    if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
  }
  return(res)
}

# Calculate the full model set for the logistic case using regular glm (SLOW!)
#full_10K <- run_sim(logistic.loglik.aic, 10000, 1:full_model_count, 1)
#save(full_10K, file="data/full_10K.Rdata")
load(file="data/full_10K.Rdata")

# Calculate the full model set using 0.1, 0.25, 0.5, 1, 5, and 10% at each iteration
#full_10K_sub_01 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.001)
#full_10K_sub_025 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.0025)
#full_10K_sub_05 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.05)
#full_10K_sub_1 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.01)
#full_10K_sub_5 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.05)
#full_10K_sub_10 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.1)

# Save the simulations
#save(full_10K_sub_01, file="data/full_10K_sub_01.Rdata")
#save(full_10K_sub_025, file="data/full_10K_sub_025.Rdata")
#save(full_10K_sub_05, file="data/full_10K_sub_05.Rdata")
#save(full_10K_sub_1, file="data/full_10K_sub_1.Rdata")
#save(full_10K_sub_5, file="data/full_10K_sub_5.Rdata")
#save(full_10K_sub_10, file="data/full_10K_sub_10.Rdata")

load(file="data/full_10K_sub_01.Rdata")
load(file="data/full_10K_sub_025.Rdata")
load(file="data/full_10K_sub_05.Rdata")
load(file="data/full_10K_sub_1.Rdata")
load(file="data/full_10K_sub_5.Rdata")
load(file="data/full_10K_sub_10.Rdata")

full_10K_mat <- matrix(unlist(full_10K), ncol=18, byrow=T)
full_10K_sub_01_mat <- matrix(unlist(full_10K_sub_01), ncol=18, byrow=T)
full_10K_sub_025_mat <- matrix(unlist(full_10K_sub_025), ncol=18, byrow=T)
full_10K_sub_05_mat <- matrix(unlist(full_10K_sub_05), ncol=18, byrow=T)
full_10K_sub_1_mat <- matrix(unlist(full_10K_sub_1), ncol=18, byrow=T)
full_10K_sub_5_mat <- matrix(unlist(full_10K_sub_5), ncol=18, byrow=T)
full_10K_sub_10_mat <- matrix(unlist(full_10K_sub_10), ncol=18, byrow=T)

# Create a matrix to compare the log likelihoods
full_10K_compare <- matrix(NA, full_model_count, 7)
full_10K_compare[,1] <- full_10K_mat[,17]
full_10K_compare[,2] <- full_10K_sub_01_mat[,17]
full_10K_compare[,3] <- full_10K_sub_025_mat[,17]
full_10K_compare[,4] <- full_10K_sub_05_mat[,17]
full_10K_compare[,5] <- full_10K_sub_1_mat[,17]
full_10K_compare[,6] <- full_10K_sub_5_mat[,17]
full_10K_compare[,7] <- full_10K_sub_10_mat[,17]
par(mfrow=c(1,1), oma=c(0,0,0,0))
multiplot(full_10K_compare[1:100,], ylim=c(-8000,-2200))
cor(full_10K_compare[1:7000,])

# Calculate the renormalized marginal probabilities
full_10K_renorm <- matrix(NA, nvars, 7)
full_10K_renorm[,1] <- GMJMCMC:::marginal.probs.renorm(full_10K)
full_10K_renorm[,2] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_10)
full_10K_renorm[,3] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_5)
full_10K_renorm[,4] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_1)
full_10K_renorm[,5] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_05)
full_10K_renorm[,6] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_025)
full_10K_renorm[,7] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_01)

barplot(t(full_10K_renorm), beside=T)
plot(cor(full_10K_renorm)[1,], type="l")




# Calculate the full model set for the gaussian case using regular glm (SLOW!)
full_10K_g <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- gaussian.loglik.aic(million_y_g[1:10000], million_x[1:10000,], modelvector, NULL, NULL)
  full_10K_g[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}
#save(full_10K_g, file="data/full_10K_g.Rdata")