# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-29

source("packages.R")
source("functions.R")
source("gauss_sim_data.R")

run <- "10K"
run <- "100K"
basename <- paste0("full_",run,"g")

## Load files
gaussian_full_files <- list.files(path=paste0("data/full_enumeration/gaussian/",run,"/"))
for (file in gaussian_full_files) load(file=paste0("data/full_enumeration/gaussian/",run,"/",file))

# Set up parameters for analysis
runs <- sub("...", "",unique(regmatches(ls(), regexpr("run[0-9]*", ls()))))
runs <- runs[-(runs == "")]
subs_list <- c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

## If analysis is already done, skip to the plotting below

## Inspect results
# Check if there are any runs with the wrong number of models
for (i in ls()) {
  if (length(eval(parse(text=i))) != full_model_count) print(paste0(i,": ",length(eval(parse(text=i)))))
}
# Make histograms of all the mliks to see that we do not appear to have any oddities
all_mliks <- mlik_collect(basename, runs)
par(mfrow=c(2,5), mar=rep(2,4))
for (i in 1:20) for (j in 1:10) hist(all_mliks[[i]][,j], breaks=100)

# Calculate correlation for all of the mliks, again to make a sanity check
all_mliks_cor <- matrix(NA, 20,10)
for (i in 1:20) all_mliks_cor[i,] <- cor(all_mliks[[i]])[1,]
par(mfrow=c(1,1))
multiplot(all_mliks_cor)

base_renorm <- GMJMCMC:::marginal.probs.renorm(eval(parse(text=basename)))

# Get all the renormalized estimates for 100K
renorm_all <- renorm_compare(basename, runs, base_renorm)
set.seed(1911)
best_renorms5 <- renorm_best(basename, runs, 5)
best_renorms10 <- renorm_best(basename, runs, 10)
best_renorms20 <- renorm_best(basename, runs, 20)

# If all processing is already done, start here!
base_renorm <- GMJMCMC:::marginal.probs.renorm(eval(parse(text=basename)))
best_renorms_diff5 <- matrix(unlist(lapply(best_renorms5, function(x) {abs(x - base_renorm)})), nvars, length(subs_list))
best_renorms_diff10 <- matrix(unlist(lapply(best_renorms10, function(x) {abs(x - base_renorm)})), nvars, length(subs_list))
best_renorms_diff20 <- matrix(unlist(lapply(best_renorms20, function(x) {abs(x - base_renorm)})), nvars, length(subs_list))

# Save renormalized estimates
#save(renorm_all, file=paste0("data/full_enumeration/gaussian/",run,"/renorm.Rdata"))
#save(best_renorms5, file=paste0("data/full_enumeration/gaussian/",run,"/best_renorms5.Rdata"))
#save(best_renorms10, file=paste0("data/full_enumeration/gaussian/",run,"/best_renorms10.Rdata"))
#save(best_renorms20, file=paste0("data/full_enumeration/gaussian/",run,"/best_renorms20.Rdata"))



renorm_meanquant <- matrix_quantmean(renorm_all)

# 100K plot
full_enum_plot(renorm_meanquant, best_renorms_diff5,
               best_renorms_diff10, best_renorms_diff20,
               "Absolute difference to full IRLS for simulated Gaussian data using 100,000 observations.",
               subs_list)
# Saved as 1100*1600 @ 110 DPI

# 10K plot
full_enum_plot(renorm_meanquant, best_renorms_diff5,
               best_renorms_diff10, best_renorms_diff20,
               "Absolute difference to full IRLS for simulated Gaussian data using 10,000 observations.",
               subs_list)
# Saved as 1100*1600 @ 110 DPI






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
