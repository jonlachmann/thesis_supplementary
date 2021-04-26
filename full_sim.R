# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

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
full_10K_mat <- matrix(unlist(full_10K), ncol=18, byrow=T)

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
full_10K_sub_1_mat <- matrix(unlist(full_10K_sub_1), ncol=18, byrow=T)

# Create a matrix to compare the log likelihoods
full_10K_compare <- matrix(NA, full_model_count, 2)
full_10K_compare[,1] <- full_10K_mat[,17]
full_10K_compare[,2] <- full_10K_sub_1_mat[,17]
par(mfrow=c(1,1), oma=c(0,0,0,0))
multiplot(full_10K_compare[1:300,])
cor(full_10K_compare[1:7000,])

# Calculate the renormalized marginal probabilities
GMJMCMC:::marginal.probs.renorm(full_10K)


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