# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load command line arguments
args <- commandArgs(trailingOnly=TRUE)

if (length(args)!=1) {
#  stop("One argument must be supplied (subsample size).n", call.=FALSE)
}

# Load packages, data, likelihood functions and general functions
source("packages.R")
source("likelihoods1.R")
source("functions.R")
load("data/mjmcmc/logistic_ex2.Rdata")

options(warn=1)

set.seed(Sys.time())

dirname <- create_randdir()
set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_mjmcmc_ex2")
run_count <- 20
iter <- 32768

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(logistic_ex2)
sim_probs$large <- 0.02


print(paste0("Running MJMCMC run!"))
run_mjmcmc(logistic.loglik.aic, logistic_ex2, sim_probs, sim_pars, 1, iter, run_count, basename)
