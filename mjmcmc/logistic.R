# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

# Load packages, data, likelihoods and general functions
source("packages.R")
source("logis_sim_data.R")
source("likelihoods1.R")
source("functions.R")

subs_list <- c("",0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

run <- "10K"
run <- "100K"
basename <- paste0("mjmcmc_",run,"l")

## Load full enumeration with full IRLS for comparison
load(file="data/full_enumeration/logistic/10K/full_10Kl.Rdata")
full_10Kl_renorm <- GMJMCMC:::marginal.probs.renorm(full_10Kl)
load(file="data/full_enumeration/logistic/100K/full_100Kl.Rdata")
full_100Kl_renorm <- GMJMCMC:::marginal.probs.renorm(full_100Kl)

## Load files and extract the results
mcmc_res <- vector("list", length(subs_list))
renorm_res <- vector("list", length(subs_list))
for (i in 1:length(subs_list)) {
  mcmc_res[[i]] <- matrix(NA, 66, 20)
  renorm_res[[i]] <- matrix(NA, 66, 20)
}
lastresult <- rep(0, length(subs_list))
logistic_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/logistic/",run,"/"))
for (file in logistic_mjmcmc_files[121:156]) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/logistic/",run,"/",file))
  sub_size <- substring(regmatches(file, regexpr(paste0(run,"l[0-9]*"), file)), 6)
  sub_size <- sub("(.)", "\\1\\.", sub_size)
  sub_id <- which(as.numeric(sub_size)/100 == subs_list)
  if (length(sub_id) == 0) sub_id <- 1
  print(paste(sub_size, ":", sub_id))
  lastresult[sub_id] <- lastresult[sub_id] + 1

  cat("Calculating MCMC estimates...\n")
  mcmc_res[[sub_id]][,lastresult[sub_id]] <- rmse_conv(full_100Kl_renorm, rundata, 66, F)
  cat("Calculating renormalized estimates...\n")
  renorm_res[[sub_id]][,lastresult[sub_id]] <- rmse_conv(full_100Kl_renorm, rundata, 66, T,T)
}
save(mcmc_res, file="data/mjmcmc/logistic/100K/mcmc_res.Rdata")
save(renorm_res, file="data/mjmcmc/logistic/100K/renorm_res.Rdata")
load(file="data/mjmcmc/logistic/10K/mcmc_res.Rdata")
load(file="data/mjmcmc/logistic/10K/renorm_res.Rdata")
load(file="data/full_enumeration/logistic/100K/renorm.Rdata")

# Add one matrix to lists
renorm_res <- c(1, renorm_res)
mcmc_res <- c(1, mcmc_res)
mcmc_res[[1]] <- matrix(NA, 66, 20)
renorm_res[[1]] <- matrix(NA, 66, 20)

mcmc_res[[2]]<- mcmc_res[[2]][,1:16]
renorm_res[[2]]<- renorm_res[[2]][,1:16]

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

save(mcmc_qm33, file="data/mjmcmc/logistic/100K/mcmc_qm33.Rdata")
save(renorm_qm33, file="data/mjmcmc/logistic/100K/renorm_qm33.Rdata")

mjmcmc_plot(mcmc_qm33,
            renorm_qm33,
            full_rmse_qm,
            ymax=0.43,
            main="RMSE of marginal posterior using MJMCMC with S-IRLS-SGD,\nlogistic data with 100,000 observations")
# Saved as 1000x800 @ 110DPI

multiplot(t(rbind(full_10Kl_renorm, full_10Kl_renorm2)))
multiplot(cbind(mcmc_res[[1]], renorm_res[[1]]))
lapply(1:7, function(x) multiplot(cbind(mcmc_res[[x]], renorm_res[[x]])))

mcmc_test <- GMJMCMC:::marginal.probs(rundata$models)

models <- c(rundata$models, rundata$lo.models)
    model.size <- length(models[[1]]$model)
    models.matrix <- matrix(unlist(models), ncol=model.size+3, byrow=T)
models.use <- models
models.use <- models.use[order(models.matrix[,(model.size+2)])]


mcmc_test <- GMJMCMC:::marginal.probs(rundata$models[-(1:5000)])
renorm_test <- GMJMCMC:::marginal.probs.renorm(models.use)

cbind(t(full_10Kl_renorm), mcmc_test, t(renorm_test))




file <- gaussian_mjmcmc_files[102]

which(as.numeric(sub_size)/100 == subs_list)

test <- loadRdata(file=paste0("data/mjmcmc/gaussian/",run,"/",gaussian_mjmcmc_files[1]))
load(file=paste0("data/mjmcmc/gaussian/",run,"/",gaussian_mjmcmc_files[21]))
load(file=paste0("data/mjmcmc/gaussian/",run,"/",gaussian_mjmcmc_files[61]))
load(file=paste0("data/mjmcmc/gaussian/",run,"/",gaussian_mjmcmc_files[81]))
load(file=paste0("data/mjmcmc/gaussian/",run,"/",gaussian_mjmcmc_files[101]))
load(file=paste0("data/mjmcmc/gaussian/",run,"/",gaussian_mjmcmc_files[121]))

## Load full enumeration for comparison
load(file="data/full_enumeration/gaussian/10K/full_10Kg.Rdata")
full_10Kg_renorm <- GMJMCMC:::marginal.probs.renorm(full_10Kg)

##
load(file="data/full_enumeration/gaussian/10K/renorm.Rdata")

renorm10Kg05 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg05_1, 66, T, T)
renorm10Kg075 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg075_1, 66, T, T)

mcmc10Kg5 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg5_1, 66, F)
renorm10Kg5 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg5_1, 66, T, T)
mcmc10Kg1 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg1_1, 66, F)
renorm10Kg1 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg1_1, 66, T, T)
mcmc10Kg05 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg05_1, 66, F)
renorm10Kg05 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg05_1, 66, T, T)
mcmc10Kg075 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg075_1, 66, F)
renorm10Kg075 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg075_1, 66, T, T)
mcmc10Kg005 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg005_1, 66, F)
renorm10Kg005 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg005_1, 66, T, T)
mcmc10Kg01 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg01_1, 66, F)
renorm10Kg01 <- rmse_conv(full_10Kg_renorm, run786_mjmcmc_10Kg01_1, 66, T, T)
renorms <- matrix(NA, 12, 66)
renorms[1,] <- sqrt(rowMeans((mcmc10Kg5/100)^2))
renorms[2,] <- sqrt(rowMeans((renorm10Kg5/100)^2))
renorms[3,] <- sqrt(rowMeans((mcmc10Kg1/100)^2))
renorms[4,] <- sqrt(rowMeans((renorm10Kg1/100)^2))
renorms[5,] <- sqrt(rowMeans((mcmc10Kg05/100)^2))
renorms[6,] <- sqrt(rowMeans((renorm10Kg05/100)^2))
renorms[7,] <- sqrt(rowMeans((mcmc10Kg01/100)^2))
renorms[8,] <- sqrt(rowMeans((renorm10Kg01/100)^2))
names(renorms) <- c("05M", "05R", "075M", "075R")

subs_list <- c(1.0,0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

ylims <- c(min(renorms), max(renorms[-c(7,8,12),]))
par(mfrow=c(1,2))
multiplot(t(renorms)[,c(1,3,5,9:11)], legend=T, names=c("5M", "1M", "05M", "01M"), ylim=ylims)
multiplot(t(renorms)[,c(2,4,6,9:11)], legend=T, names=c("5R", "1R", "05R", "01R"), ylim=ylims)



multiplot(t(renorms)[1:60,c(1,3,5,9:11)], legend=T, names=c("5M", "1M", "05M", "Full"), ylim=ylims)
multiplot(t(renorms)[1:60,c(2,4,6,9:11)], legend=T, names=c("5R", "1R", "05R", "Full"), ylim=ylims)

plot(rowMeans(mjmcmc_10K_conv), type="l", names=c("05M", "05R", "075M", "075R"))

renorms[9,] <- rep(sqrt(mean((renorm_all[[1]][,4])^2)),66)
renorms[10,] <- rep(sqrt(mean((renorm_all[[1]][,5])^2)),66)
renorms[11,] <- rep(sqrt(mean((renorm_all[[1]][,7])^2)),66)
renorms[12,] <- rep(sqrt(mean((renorm_all[[1]][,9])^2)),66)






mjmcmc_10K_conv <- vector("list", 20)
for (i in 1:20) {
  if (!is.null(mjmcmc_10K[[i]])) {
    mjmcmc_10K_conv[[i]] <- rmse_conv(full_10K_renorm, mjmcmc_10K[[i]], 66)
  }
}
