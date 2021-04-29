# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-29

library(mvtnorm)

nvars <- 15
nobs <- 10^6
full_model_count <- 2^nvars

set.seed(1911)
#save(covmat, file="data/covmat.Rdata")
#covmat <- cov(read.table(text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/examples/Simulated%20Data%20%28Example%201%29/simcen-x.txt")))
load(file="data/covmat.Rdata")

mill_x_g <- matrix(rmvnorm(nobs, rnorm(nvars), covmat), nobs)

betas_g <- c(0.48, 8.72, 1.76, 1.87, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0)
mill_x_g <- cbind(1,mill_x_g)

ic <- 2
# Increase sd with sqrt of sample size increase factor
mill_y_g1M <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(10000))) + ic), 1)
mill_y_g100K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(1000))) + ic), 1)
mill_y_g10K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(100))) + ic), 1)
mill_y_g2K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(20))) + ic), 1)