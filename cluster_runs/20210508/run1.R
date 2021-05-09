# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load command line arguments
args <- commandArgs(trailingOnly=TRUE)

if (length(args)!=1) {
  #stop("One argument must be supplied (subsample size).n", call.=FALSE)
}

# Load packages, data, likelihood functions and general functions
source("packages.R")
source("likelihoods1.R")
source("functions.R")
library(GMJMCMC)

options(warn=1)

set.seed(Sys.time())

dirname <- create_randdir()

# Load the data
kmdata <- as.matrix(read.csv("data/gmjmcmc/exa1.csv"))
kmdata <- cbind(kmdata[,5], kmdata[,-5])

transforms <- c("sigmoid","sini","tanh","atan","troot")
#transforms <- c("sigmoid","sini","expi","logi","troot","to23","to72")

set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_gmjmcmc_kepler")
run_count <- 50
subs <- 1


km_pars <- gen.params.list(kmdata, T)
km_probs <- gen.probs.list(transforms)

km_probs$filter <- 0.7
km_pars$loglik$r <- 1/223
km_pars$feat$pop.max <- 25
km_pars$feat$keep.org <- T
km_pars$feat$keep.min <- 0.7
km_pars$feat$D <- 8
km_probs$gen <- c(0.45, 0.45, 0.05, 0.05)

num_cores <- detectCores()

print(paste0("Running ",basename,"run!"))
run_gmjmcmc(run_count, basename, subs,
           kmdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 40, 250, 2000, km_probs, km_pars, F)
