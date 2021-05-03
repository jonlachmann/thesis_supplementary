# Set path for packages
.libPaths("/cluster/home/jola4668/R")

args <- commandArgs(trailingOnly=TRUE)
print(args[1])

# Load packages, data, likelihood functions and general functions
source("packages.R")
source("gauss_sim_data.R")
source("likelihoods1.R")
source("functions.R")

options(warn=1)

set.seed(Sys.time())

n_obs <- 10000
dirname <- create_randdir()
set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_mjmcmc_10Kg")
sub_size <- as.numeric(args[1]) # 0.05 #,0.01,0.0075,0.005,0.0025,0.001,0.0005)
run_count <- 20
iter <- round(32768*2)

data_use <- cbind(mill_y_g10K, mill_x_g)[1:n_obs,]

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_use)
sim_pars$loglik$g <- 100/sqrt(n_obs/100)
sim_probs$large <- 0.05

run_mjmcmc(linear.g.prior.loglik.irlssgd, data_use, sim_probs, sim_pars, sub_size, iter, run_count, basename)
#system.time(run_mjmcmc(linear.g.prior.loglik, data_use, sim_probs, sim_pars, 1, iter, run_count, basename))