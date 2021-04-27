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

# Calculate the full model set for the logistic case using regular glm (SLOW!)
full_10K <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, NULL)
  full_10K[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}
#save(full_10K, file="data/full_10K.Rdata")
load(file="data/full_10K.Rdata")
full_10K_mat <- matrix(unlist(full_10K), ncol=18, byrow=T)

# Calculate the full model set using 0.5% at each iteration
full_10K_sub_01 <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, list(subs = 0.001))
  full_10K_sub_01[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}

#save(full_10K_sub_01, file="data/full_10K_sub_01.Rdata")
load(file="data/full_10K_sub_01.Rdata")
full_10K_sub_01_mat <- matrix(unlist(full_10K_sub_01), ncol=18, byrow=T)

# Calculate the full model set using 0.5% at each iteration
full_10K_sub_05 <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, list(subs = 0.005))
  full_10K_sub_05[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}

#save(full_10K_sub_05, file="data/full_10K_sub_05.Rdata")
load(file="data/full_10K_sub_05.Rdata")
full_10K_sub_05_mat <- matrix(unlist(full_10K_sub_05), ncol=18, byrow=T)

# Calculate the full model set using 1% at each iteration
full_10K_sub_1 <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, list(subs = 0.01))
  full_10K_sub_1[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}
#save(full_10K_sub_1, file="data/full_10K_sub_1.Rdata")
load(file="data/full_10K_sub_1.Rdata")
full_10K_sub_1_mat <- matrix(unlist(full_10K_sub_1), ncol=18, byrow=T)

# Calculate the full model set using 1% at each iteration
full_10K_sub_5 <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, list(subs = 0.05))
  full_10K_sub_5[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}
#save(full_10K_sub_5, file="data/full_10K_sub_5.Rdata")
load(file="data/full_10K_sub_5.Rdata")
full_10K_sub_5_mat <- matrix(unlist(full_10K_sub_5), ncol=18, byrow=T)

# Calculate the full model set using 1% at each iteration
full_10K_sub_10 <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, list(subs = 0.1))
  full_10K_sub_10[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}
#save(full_10K_sub_10, file="data/full_10K_sub_10.Rdata")
load(file="data/full_10K_sub_10.Rdata")
full_10K_sub_10_mat <- matrix(unlist(full_10K_sub_10), ncol=18, byrow=T)

# Create a matrix to compare the log likelihoods
full_10K_compare <- matrix(NA, full_model_count, 5)
full_10K_compare[,1] <- full_10K_mat[,17]
full_10K_compare[,2] <- full_10K_sub_01_mat[,17]
full_10K_compare[,3] <- full_10K_sub_05_mat[,17]
full_10K_compare[,4] <- full_10K_sub_1_mat[,17]
full_10K_compare[,5] <- full_10K_sub_5_mat[,17]
par(mfrow=c(1,1), oma=c(0,0,0,0))
multiplot(full_10K_compare[1:300,], ylim=c(-6000,-2000))
cor(full_10K_compare[1:7000,])

# Calculate the renormalized marginal probabilities
full_10K_renorm <- matrix(NA, nvars, 6)
full_10K_renorm[,1] <- GMJMCMC:::marginal.probs.renorm(full_10K)
full_10K_renorm[,2] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_10)
full_10K_renorm[,3] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_5)
full_10K_renorm[,4] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_1)
full_10K_renorm[,5] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_05)
full_10K_renorm[,6] <- GMJMCMC:::marginal.probs.renorm(full_10K_sub_01)

barplot(t(full_10K_renorm), beside=T)
cor(full_10K_renorm)


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