# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-12

source("functions.R")
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

full_renorm <- GMJMCMC:::marginal.probs.renorm(logistic_full2)
names(full_renorm) <- paste0("y",1:20)

sort(full_renorm)

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(logistic_ex2)
sim_probs$large <- 0.05

mjmcmcres <- mjmcmc(logistic_data, logistic.loglik.aic, 500, sim_probs, sim_pars)
renorm_mjmcmc <- GMJMCMC:::marginal.probs.renorm(mjmcmcres$models)

save(logistic_ex2, file="data/mjmcmc/logistic_ex2.Rdata")

marginal.probs.renorm <- function (models) {
  model.size <- length(models[[1]]$model)
  models.matrix <- matrix(unlist(models), ncol=model.size+3, byrow=T)
  models.matrix <- models.matrix[(!duplicated(models.matrix[,2:(model.size+1)], dim=1, fromLast=T)),]
  unique <- (nrow(models.matrix))
  max_mlik <- max(models.matrix[,(model.size+2)])
  crit.sum <- sum(exp(models.matrix[,(model.size+2)]-max_mlik))
  probs <- matrix(NA,1,model.size)
  for (i in 2:(model.size+1)) probs[i-1] <- sum(exp(models.matrix[as.logical(models.matrix[,i]),(model.size+2)]-max_mlik))/crit.sum
  return(list(probs=probs, unique=unique))
}

total <- sum(exp(matrix(unlist(logistic_full2), ncol=23, byrow=T)[,22]))

mcmc_res1 <- matrix(NA, 20, 20)
renorm_res1 <- matrix(NA, 20, 20)
totals <- matrix(NA, 1, 20)
unique <- matrix(NA, 1, 20)
counts <- matrix(NA, 1, 20)
lo.counts <- matrix(NA, 1, 20)
lastresult <- 0
logistic_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/logistic_hubin/"))[-(1:2)]
for (file in logistic_mjmcmc_files) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/logistic_hubin/",file))
  lastresult <- lastresult + 1

  cat("\nCalculating MCMC and renormalized estimates...\n")
  mcmcren <- renorm_mcmc(rundata, T, 13250, T)
  mcmc_res1[,lastresult] <- as.numeric(mcmcren$mc)
  renorm_res1[,lastresult] <- mcmcren$rm
  totals[lastresult] <- mcmcren$total
  unique[lastresult] <- mcmcren$unique
  counts[lastresult] <- mcmcren$count
  lo.counts[lastresult] <- mcmcren$lo.use
}

mcmc_res2 <- matrix(NA, 20, 20)
renorm_res2 <- matrix(NA, 20, 20)
totals2 <- matrix(NA, 1, 20)
unique2 <- matrix(NA, 1, 20)
counts2 <- matrix(NA, 1, 20)
lo.counts2 <- matrix(NA, 1, 20)
lastresult <- 0
logistic_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/logistic_hubin/"))[-(1:2)]
for (file in logistic_mjmcmc_files) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/logistic_hubin/",file))
  lastresult <- lastresult + 1

  cat("\nCalculating MCMC and renormalized estimates...\n")
  mcmcren <- renorm_mcmc(rundata, T, 27200, T)
  mcmc_res2[,lastresult] <- as.numeric(mcmcren$mc)
  renorm_res2[,lastresult] <- mcmcren$rm
  totals2[lastresult] <- mcmcren$total
  unique2[lastresult] <- mcmcren$unique
  counts2[lastresult] <- mcmcren$count
  lo.counts2[lastresult] <- mcmcren$lo.use
}



mcmc_rmse1 <- rep(0,20)
renorm_rmse1 <- rep(0,20)
for(i in 1:20) {
  mcmc_rmse1[i] <- sqrt(mean((mcmc_res1[i,] - full_renorm[i])^2))
  renorm_rmse1[i] <- sqrt(mean((renorm_res1[i,] - full_renorm[i])^2))
}
mcmc_rmse2 <- rep(0,20)
renorm_rmse2 <- rep(0,20)
for(i in 1:20) {
  mcmc_rmse2[i] <- sqrt(mean((mcmc_res2[i,] - full_renorm[i])^2))
  renorm_rmse2[i] <- sqrt(mean((renorm_res2[i,] - full_renorm[i])^2))
}

mean(unique)
mean(unique2)

round(mean(totals)/total, 2)
round(mean(totals2)/total, 2)

round(mean(renorm_rmse1)*100,2)
round(mean(mcmc_rmse1)*100,2)
round(mean(renorm_rmse2)*100,2)
round(mean(mcmc_rmse2)*100,2)

round(renorm_rmse1*100,2)[order(full_renorm)]
round(mcmc_rmse1*100,2)[order(full_renorm)]
round(renorm_rmse2*100,2)[order(full_renorm)]
round(mcmc_rmse2*100,2)[order(full_renorm)]

round(sort(full_renorm),2)

mcmc_res <- matrix(NA, 100, 20)
renorm_res <- matrix(NA, 100, 20)
lastresult <- 0
logistic_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/logistic_hubin/"))[-(1:2)]
for (file in logistic_mjmcmc_files) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/logistic_hubin/",file))
  rundata$models <- rundata$models[1:20000]
  lastresult <- lastresult + 1

  cat("\nCalculating MCMC estimates...\n")
  mcmc_res[,lastresult] <- rmse_conv(full_renorm, rundata, 100, F)
  cat("Calculating renormalized estimates...\n")
  renorm_res[,lastresult] <- rmse_conv(full_renorm, rundata, 100, T,T)
}

#save(mcmc_res, file="data/mjmcmc/logistic_hubin/mcmc_res.Rdata")
#save(renorm_res, file="data/mjmcmc/logistic_hubin/renorm_res.Rdata")

# Get the mean and 0.05, 0.95 quantiles for the runs
mcmc_qm <- row_quantmean(mcmc_res[,])
renorm_qm <- row_quantmean(renorm_res[,])

ci_plot(mcmc_qm, density=30, angle=45, border="lightgray", lty="dotted", main="RMSE of marginal posterior using MJMCMC,\nSimulated logistic data", ylab="RMSE", xlab="MJMCMC Iterations x200")
ci_plot(renorm_qm, density=15, append=T, angle=-45, border="gray")

legend("topright", legend=c("Renormalized (RM)", "MCMC (MC)"),
           lty=c("solid", "dotted"),
           density=c(15,30,0),
           angle=c(0,-45,45),
           fill=c("lightgray", "lightgray"),
           col=c("black", "black"), bty="n")

# Saved as 1000x800 @ 140DPI