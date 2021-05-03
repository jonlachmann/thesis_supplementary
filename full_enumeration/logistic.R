# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-29

source("packages.R")
source("functions.R")
source("logis_sim_data.R")

run <- "10K"
run <- "100K"

## Load files
logistic_full_files <- list.files(path=paste0("data/full_enumeration/logistic/",run,"/"))
for (file in logistic_full_files) load(file=paste0("data/full_enumeration/logistic/",run,"/",file))

# Set up parameters for analysis
runs <- sub("...", "",unique(regmatches(ls(), regexpr("run[0-9]*", ls()))))
runs <- runs[-(runs == "")]

subs_list <- c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)
basename <- paste0("full_",run,"l")

base_renorm <- GMJMCMC:::marginal.probs.renorm(eval(parse(text=basename)))

renorm_100Kl <- renorm_compare(basename, runs, base_renorm)
set.seed(1911)
best_renorms5 <- renorm_best(basename, runs, 5)
best_renorms10 <- renorm_best(basename, runs, 10)
best_renorms20 <- renorm_best(basename, runs, 20)
best_renorms_diff5 <- matrix(unlist(lapply(best_renorms5, function(x) {abs(x - base_renorm)})), nvars, length(subs_list))
best_renorms_diff10 <- matrix(unlist(lapply(best_renorms10, function(x) {abs(x - base_renorm)})), nvars, length(subs_list))
best_renorms_diff20 <- matrix(unlist(lapply(best_renorms20, function(x) {abs(x - base_renorm)})), nvars, length(subs_list))



save(renorm_100Kl, file="data/full_enumeration/logistic/100K/renorm.Rdata")

renorm_meanquant <- matrix_quantmean(renorm_100Kl)

par(mfrow=c(3,5), mar=c(1,1,1,1))
for (i in 1:15) {
  ci_plot(renorm_meanquant, i, ylab="Absolute difference", xlab="Subsample size", xlab=c("Full", subs_list))
  lines(c(0,best_renorms_diff5[i,]), col="red", lty="dashed")
  lines(c(0,best_renorms_diff10[i,]), col="blue", lty="dashed")
  lines(c(0,best_renorms_diff20[i,]), col="darkgreen", lty="dashed")
}