# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load command line arguments
args <- commandArgs(trailingOnly=TRUE)

if (length(args)!=1) {
#  stop("One argument must be supplied (subsample size).n", call.=FALSE)
}

# Load packages, data, likelihood functions and general functions
source("packages.R")
source("logis_sim_data.R")
source("likelihoods1.R")
source("functions.R")

options(warn=1)

set.seed(Sys.time())

n_obs <- 10000
dirname <- create_randdir()
set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_mjmcmc_10Kl")
run_count <- 2
iter <- 32768

data_use <- cbind(mill_y_l10K, mill_x_g)[1:n_obs,]

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_use)
sim_pars$loglik$g <- 100/sqrt(n_obs/100)
sim_probs$large <- 0.05

print(paste0("Running full run!"))
run_mjmcmc(logistic.loglik.bic, data_use, sim_probs, sim_pars, 1, iter, run_count, basename)