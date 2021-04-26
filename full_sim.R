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

# Calculate the full model set using regular glm (SLOW!)
full_10K <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, NULL)
  full_10K[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/100) == 0) progress <- print.progressbar(progress, 100)
}
