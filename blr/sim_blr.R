# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-10

set.seed(1911)

n_obs <- 10000

# Decrease effects with sqrt of sample size increase factor
blr_betas <- c(7, 7, 9, 7, 3.5, 6.6, 1.5, 1.5) / sqrt(n_obs/1000)
#simulate the data for a given run
blr_x <- as.data.frame(array(data = rbinom(n = 50*n_obs, size = 1,prob = runif(n = 50*n_obs,0,1)),
                             dim = c(n_obs, 50)))
blr_y <- rnorm(n = n_obs, mean = 1 +
  blr_betas[1]*(blr_x$V4*blr_x$V17*blr_x$V30*blr_x$V10) +
  blr_betas[2]*(((blr_x$V50*blr_x$V19*blr_x$V13*blr_x$V11)>0)) +
  blr_betas[3]*(blr_x$V37*blr_x$V20*blr_x$V12) +
  blr_betas[4]*(blr_x$V1*blr_x$V27*blr_x$V3) +
  blr_betas[5]*(blr_x$V9*blr_x$V2) +
  blr_betas[6]*(blr_x$V21*blr_x$V18) +
  blr_betas[7]*blr_x$V7 +
  blr_betas[8]*blr_x$V8, sd = 1)