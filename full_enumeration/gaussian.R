# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-29

source("packages.R")
source("functions.R")

# Load all the data
gauss_100K_files <- list.files(path="data/full_enumeration/gaussian/100K/", pattern=".Rdata")
for (file in gauss_100K_files) load(file=paste0("data/full_enumeration/gaussian/100K/",file))

# Set up parameters for analysis
runs <- sub("...", "",unique(regmatches(ls(), regexpr("run[0-9]*", ls()))))
for (i in 1:length(runs)) {
  if (runs[i] == "") runs <- runs[-i]
}
subs_list <- c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

source("gauss_sim_data.R")

base_renorm <- GMJMCMC:::marginal.probs.renorm(full_100Kg)

# Get all the renormalized estimates
renormalized_estimates <- vector("list", length(runs))
for (i in 1:length(runs)) {
  renorm <- matrix(NA, nvars, length(subs_list)+1)
  renorm[,1] <- 0
  for (j in 1:length(subs_list)) {
    run_name <- paste0("run",runs[i], "_full_100Kg",subs_list[j]*100)
    run_name <- gsub("\\.", "", run_name)
    renorm[,j+1] <- abs(GMJMCMC:::marginal.probs.renorm(eval(parse(text=run_name))) - base_renorm)
    print(run_name)
  }
  renormalized_estimates[[i]] <- renorm
}

save(renormalized_estimates, file="data/full_enumeration/gaussian/100K/renorm.Rdata")

meanquant <- matrix_quantmean(renormalized_estimates)

par(mfrow=c(3,5))
for (i in 1:15) {
  ci_plot(meanquant, i, ylab="Absolute difference", xlab="Subsample size")
}


GMJMCMC:::marginal.probs.renorm(run1172_full_100Kg20)
GMJMCMC:::marginal.probs.renorm(full_100Kg)

model.size <- length(run11_full_100Kg1[[1]]$model)
models.matrix <- matrix(unlist(run1218_full_100Kg1), ncol = model.size + 3, byrow = T)
models.matrix <- models.matrix[(!duplicated(models.matrix[, 2 : (model.size + 1)], dim = 1)), ]
max_mlik <- max(models.matrix[, (model.size + 2)])

function(models) {




  crit.sum <- sum(exp(models.matrix[, (model.size + 2)] - max_mlik))
  probs <- matrix(NA, 1, model.size)
  for (i in 2 : (model.size + 1)) probs[i - 1] <- sum(exp(models.matrix[as.logical(models.matrix[, i]), (model.size + 2)] - max_mlik)) / crit.sum
  return(probs)
}

compare_100kg <- matrix(NA, full_model_count, 2)
compare_100kg[,1] <- (matrix(unlist(full_100Kg), ncol=18, byrow=T)[,17])
compare_100kg[,2] <- (matrix(unlist(run1218_full_100Kg1), ncol=18, byrow=T)[,17])

basename <- paste0("run",runname, "_full_100Kg")

load(file="data/full_enumeration/gaussian/run1218_full_100Kg1.Rdata")

for (i in 32784:32769) {
  run1218_full_100Kg1[[i]] <- NULL
}
run1218_full_100Kg1
