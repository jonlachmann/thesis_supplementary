# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-12

source("likelihoods1.R")

library(GMJMCMC)

### Download simulated logistic data as per example 2
logistic_x <- read.csv(header=F, text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/supplementaries/Mode%20Jumping%20MCMC/supplementary/examples/Simulated%20Logistic%20Data%20With%20Multiple%20Modes%20(Example%203)/sim3-X.txt"))
logistic_y <- read.csv(header=F, text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/supplementaries/Mode%20Jumping%20MCMC/supplementary/examples/Simulated%20Logistic%20Data%20With%20Multiple%20Modes%20(Example%203)/sim3-Y.txt"))
# Modify data
logistic_x$V2<-(logistic_x$V10+logistic_x$V14)*logistic_x$V9
logistic_x$V5<-(logistic_x$V11+logistic_x$V15)*logistic_x$V12

# Add intercept
logistic_x <- as.matrix(cbind(1, logistic_x))
logistic_y <- as.matrix(logistic_y)

# Compile dataset
logistic_ex2 <- as.matrix(cbind(logistic_y, logistic_x))

full_model_count <- 2^(20)
model_partitions <- matrix(1:full_model_count, 8, byrow=T)

dirname <- create_randdir()

# Run the full enumeration
run_multisim(logistic_x, logistic_y, logistic.aic.loglik,
             model_partitions, 100, 1, "logistic_hubin", dirname, list(g=100))

load("data/mjmcmc/logistic_hubin/logistic_full.Rdata")

renorm_full <- GMJMCMC:::marginal.probs.renorm(logistic_full2)
names(renorm_full) <- paste0("y",1:20)

sort(renorm_full)

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(logistic_ex2)
sim_probs$large <- 0.05

mjmcmcres <- mjmcmc(logistic_data, logistic.loglik.aic, 500, sim_probs, sim_pars)
renorm_mjmcmc <- GMJMCMC:::marginal.probs.renorm(mjmcmcres$models)

save(logistic_ex2, file="data/mjmcmc/logistic_ex2.Rdata")
