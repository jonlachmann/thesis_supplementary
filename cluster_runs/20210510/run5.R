# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-10

.libPaths("/cluster/home/jola4668/R")

source("packages.R")
source("blr/sim_blr.R")
source("functions.R")
library(GMJMCMC)

dirname <- create_randdir()

set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_gmjmcmc_blr")
run_count <- 40
subs <- 1

transforms <- c("sigmoid","sini","tanh","atan","troot")

blrdata <- cbind(blr_y, blr_x)

blr_pars <- gen.params.list(blrdata, T)
blr_probs <- gen.probs.list(transforms)

blr_probs$filter <- 0.8
blr_pars$loglik$r <- 1/nrow(blrdata)
blr_pars$feat$pop.max <- 32
blr_pars$feat$prel.filter <- 0.2
blr_pars$feat$keep.org <- T
blr_pars$feat$keep.min <- 0.9
blr_pars$feat$D <- 5
blr_probs$gen <- c(10, 5, 1, 1)
blr_pars$rescale.large <- T

num_cores <- detectCores()

print(paste0("Running ",basename,"run!"))
run_gmjmcmc(run_count, basename, subs,
            blrdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 60, 250, 2000, blr_probs, blr_pars, F)
