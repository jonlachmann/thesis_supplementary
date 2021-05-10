# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

# Load packages, data, likelihoods and general functions
source("packages.R")
source("gauss_sim_data.R")
source("likelihoods1.R")
source("functions.R")
#    c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)
subs_list <- c("",0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

run <- "10K"
run <- "100K"
basename <- paste0("mjmcmc_",run,"g")


## Load full enumeration with full IRLS for comparison
load(file="data/full_enumeration/gaussian/10K/full_10Kg.Rdata")
load(file="data/full_enumeration/gaussian/100K/full_100Kg.Rdata")
full_10Kg_renorm <- GMJMCMC:::marginal.probs.renorm(full_10Kg)
full_100Kg_renorm <- GMJMCMC:::marginal.probs.renorm(full_100Kg)

## Load files and extract the results
mcmc_res <- vector("list", length(subs_list))
renorm_res <- vector("list", length(subs_list))
for (i in 1:length(subs_list)) {
  mcmc_res[[i]] <- matrix(NA, 66, 20)
  renorm_res[[i]] <- matrix(NA, 66, 20)
}
lastresult <- rep(0, length(subs_list))
gaussian_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/gaussian/",run,"/"))
for (file in gaussian_mjmcmc_files[81:120]) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/gaussian/",run,"/",file))
  sub_size <- substring(regmatches(file, regexpr(paste0(run,"g[0-9]*"), file)), 6)
  sub_size <- sub("(.)", "\\1\\.", sub_size)
  sub_id <- which(as.numeric(sub_size)/100 == subs_list)
  if (length(sub_id) == 0) sub_id <- 1
  print(paste(sub_size, ":", sub_id))
  lastresult[sub_id] <- lastresult[sub_id] + 1

  cat("Calculating MCMC estimates...\n")
  mcmc_res[[sub_id]][,lastresult[sub_id]] <- rmse_conv(full_100Kg_renorm, rundata, 66, F)
  cat("Calculating renormalized estimates...\n")
  renorm_res[[sub_id]][,lastresult[sub_id]] <- rmse_conv(full_100Kg_renorm, rundata, 66, T,T)
}
#save(mcmc_res, file="data/mjmcmc/gaussian/100K/mcmc_res.Rdata")
#save(renorm_res, file="data/mjmcmc/gaussian/100K/renorm_res.Rdata")
load(file="data/mjmcmc/gaussian/10K/mcmc_res.Rdata")
load(file="data/mjmcmc/gaussian/10K/renorm_res.Rdata")
load(file="data/full_enumeration/gaussian/10K/renorm.Rdata")


# Calculate the mean renorm for the full enumeration runs
full_rmse <- lapply(renorm_all, function (x) {
  square <- x^2
  means <- colMeans(square)
  roots <- sqrt(means)
  return(as.matrix(roots))
})
full_rmse_qm <- matrix_quantmean(full_rmse)

# Get the mean and 0.05, 0.95 quantiles for the runs
mcmc_qm <- lapply(mcmc_res, row_quantmean)
renorm_qm <- lapply(renorm_res, row_quantmean)
mcmc_qm33 <- vector("list")
mcmc_qm33[[1]] <- lapply(mcmc_qm[[1]], function(x) x[seq(1,66,2)])
for (i in 2:8) mcmc_qm33[[i]] <- lapply(mcmc_qm[[i]], function(x) x[seq(1:33)])

renorm_qm33 <- vector("list")
renorm_qm33[[1]] <- lapply(renorm_qm[[1]], function(x) x[seq(1,66,2)])
for (i in 2:8) renorm_qm33[[i]] <- lapply(renorm_qm[[i]], function(x) x[seq(1:33)])

#save(mcmc_qm33, file="data/mjmcmc/gaussian/10K/mcmc_qm33.Rdata")
#save(renorm_qm33, file="data/mjmcmc/gaussian/10K/renorm_qm33.Rdata")

mjmcmc_plot(mcmc_qm33,
            renorm_qm33,
            full_rmse_qm,
            ymax=0.43,
            main="RMSE of marginal posterior using MJMCMC with S-IRLS-SGD,\nGaussian data with 10,000 observations")
# Saved as 1000x800 @ 110DPI

