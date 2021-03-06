# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-11

source("packages.R")
source("functions.R")

library(GMJMCMC)

# Download data
crime_x <- read.table(text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/supplementaries/Mode%20Jumping%20MCMC/supplementary/examples/US%20Data/simcen-x1.txt"),sep = ",")
crime_y <- read.table(text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/supplementaries/Mode%20Jumping%20MCMC/supplementary/examples/US%20Data/simcen-y1.txt"))
# Add intercept
crime_x <- cbind(1, crime_x)

crime_x <- as.matrix(crime_x)
crime_y <- as.matrix(crime_y)

full_model_count <- 32768
model_partitions <- matrix(1:full_model_count, 8, byrow=T)

dirname <- create_randdir()

# Run the full enumeration
run_multisim(crime_x, crime_y, linear.g.prior.loglik, model_partitions, 47, 1, "crime", dirname)

# The the full renormalized estimate
full_renorm <- GMJMCMC:::marginal.probs.renorm(crime)
mjmcmc2_renorm <- GMJMCMC:::marginal.probs(rundata$models)

multiplot(t(rbind(full_renorm, mjmcmc1_renorm,mjmcmc2_renorm)))
plot(full_renorm[1,], type="l")
lines(mjmcmc1_renorm, col="red")
lines(mjmcmc2_renorm, col="blue")

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(crime_data)
sim_pars$loglik$g <- 47
sim_probs$large <- 0.0
iter <- 10000
runs <- 10

# Run MJMCMC
run_mjmcmc(linear.g.prior.loglik, crime_data, sim_probs, sim_pars, 1, iter, runs, "crime_mjmcmc")

mcmc_res <- matrix(NA, 100, 20)
renorm_res <- matrix(NA, 100, 20)
lastresult <- 0
crime_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/crime/"))
for (file in crime_mjmcmc_files[1:20]) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/crime/",file))
  lastresult <- lastresult + 1

  cat("\nCalculating MCMC estimates...\n")
  mcmc_res[,lastresult] <- rmse_conv(full_renorm, rundata, 100, F)
  cat("Calculating renormalized estimates...\n")
  renorm_res[,lastresult] <- rmse_conv(full_renorm, rundata, 100, T,T)
}

mcmc_res1 <- matrix(NA, 15, 20)
renorm_res1 <- matrix(NA, 15, 20)
totals <- matrix(NA, 1, 20)
unique <- matrix(NA, 1, 20)
counts <- matrix(NA, 1, 20)
lo.counts <- matrix(NA, 1, 20)
lastresult <- 0
crime_mjmcmc_files <- list.files(path=paste0("data/mjmcmc/crime/"))
for (file in crime_mjmcmc_files) {
  # Load the file
  cat("Loading ",file,"...\n")
  rundata <- loadRdata(paste0("data/mjmcmc/crime/",file))
  lastresult <- lastresult + 1

  cat("\nCalculating MCMC and renormalized estimates...\n")
  mcmcren <- renorm_mcmc(rundata, T, 11200, T)
  mcmc_res1[,lastresult] <- as.numeric(mcmcren$mc)
  renorm_res1[,lastresult] <- mcmcren$rm
  totals[lastresult] <- mcmcren$total
  unique[lastresult] <- mcmcren$unique
  counts[lastresult] <- mcmcren$count
  lo.counts[lastresult] <- mcmcren$lo.use
}

total <- length(exp(matrix(unlist(crime), ncol=18, byrow=T)[,17]))

sum(c(counts, lo.counts))/20

mean(totals/total)
mean(unique)

mcmc_rmse1 <- rep(0,15)
renorm_rmse1 <- rep(0,15)
for(i in 1:15) {
  mcmc_rmse1[i] <- sqrt(mean((mcmc_res1[i,] - full_renorm[i])^2))
  renorm_rmse1[i] <- sqrt(mean((renorm_res1[i,] - full_renorm[i])^2))
}
names(mcmc_rmse2) <- paste0("Y",1:15)
names(renorm_rmse2) <- paste0("Y",1:15)

mean(renorm_rmse2)

mean(mcmc_rmse2)

round(renorm_rmse1,4)[order(full_renorm)]
round(mcmc_rmse1,4)[order(full_renorm)]
round(renorm_rmse2,2)[order(full_renorm)]
round(mcmc_rmse2,2)[order(full_renorm)]


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

#save(mcmc_res, file="data/mjmcmc/crime/mcmc_res.Rdata")
#save(renorm_res, file="data/mjmcmc/crime/renorm_res.Rdata")

# Get the mean and 0.05, 0.95 quantiles for the runs
mcmc_qm <- row_quantmean(mcmc_res[,])
renorm_qm <- row_quantmean(renorm_res[,])

mcmc_qm2 <- row_quantmean(mcmc_res2[,])
renorm_qm2 <- row_quantmean(renorm_res2[,])

ci_plot(mcmc_qm, density=30, angle=45, border="lightgray", lty="dotted", main="RMSE of marginal posterior using MJMCMC,\nUS Crime data", ylab="RMSE", xlab="MJMCMC Iterations x100")
ci_plot(renorm_qm, density=15, append=T, angle=-45, border="gray")

legend("topright", legend=c("Renormalized (RM)", "MCMC (MC)"),
           lty=c("solid", "dotted"),
           density=c(15,30,0),
           angle=c(0,-45,45),
           fill=c("lightgray", "lightgray"),
           col=c("black", "black"), bty="n")
abline(h=0.05)





# Calculate the full model set
crime_full <- vector("list", full_model_count)
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- linear.g.prior.loglik(as.matrix(crime_y), as.matrix(crime_x), modelvector, NULL, list(g=47))
  crime_full[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
}

load("379/crime.Rdata")

# Calculate the renormalized values for the full model space (ground truth)
full_renorm <- GMJMCMC:::marginal.probs.renorm(crime)
names(full_renorm) <- paste0("y",1:15)
sort(full_renorm)

# Set up data and parameters for MJMCMC
colnames(crime_y) <- "Y"
crime_data <- cbind(crime_y, crime_x)
crime_probs <- gen.probs.list(NA)
crime_params <- gen.params.list(crime_data)
crime_params$loglik$g <- 47

# Generate models using MJMCMC
crime_result <- gmjmcmc(crime_data, linear.g.prior.loglik, NA, transforms, 1, 6000, 6000, crime_probs, crime_params)
crime_result2 <- mjmcmc(crime_data, linear.g.prior.loglik, 6000, crime_probs, crime_params)

# Calculate the renormalized values for the MJMCMC result
mjmcmc_renorm <- marginal.probs.renorm(crime_result$models)
mjmcmc_renorm2 <- marginal.probs.renorm(crime_result2$models)

rmse <- function (full, sim, iters) {
  sim_renorm <- marginal.probs.renorm(sim$models[[1]][1:iters])
  names(sim_renorm) <- paste0("y",1:length(sim_renorm))
  sim_renorm <- sim_renorm[order(full)]
  full <- sort(full)
  rmse <- sqrt((sim_renorm - full)^2)
  return(rmse*100)
}

rmse2 <- function (full, sim, iters) {
  sim_renorm <- marginal.probs.renorm(sim$models[1:iters])
  names(sim_renorm) <- paste0("y",1:length(sim_renorm))
  sim_renorm <- sim_renorm[order(full)]
  full <- sort(full)
  rmse <- sqrt((sim_renorm - full)^2)
  return(rmse*100)
}

# Calculate the rmse for 1000-60000 iterations as the MJMCMC baseline
rmse_converge <- matrix(NA, 60, 15)
rmse_converge2 <- matrix(NA, 60, 15)
for(i in 1:60) {
  rmse_converge[i,] <- rmse(full_renorm, crime_result, i*100)
  rmse_converge2[i,] <- rmse2(full_renorm, crime_result2, i*100)
}

# Plot the convergence
plot(rowMeans(rmse_converge), type="l")
plot(rowMeans(rmse_converge2), type="l")



logistic.loglik.aic <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- fastglm(as.matrix(x[,model]), y, family=binomial())})
  ret <- -(mod$deviance/2) - mod$rank
  return(ret)
}

full_model_count <- 2^20

# Calculate the full model set
logistic_full2 <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:20]))
  loglik <- logistic.loglik.aic(as.matrix(logistic_y), as.matrix(logistic_x), modelvector, NULL, list(g=47))
  logistic_full2[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/40) == 0) progress <- print.progressbar(progress, 40)
}

save(logistic_full2, file="logistic_full2")

# Calculate the renormalized values for the full model space (ground truth)
logistic_full.renorm <- marginal.probs.renorm(logistic_full2)
names(logistic_full.renorm) <- paste0("y",1:20)

# Set up data and parameters for MJMCMC
colnames(logistic_y) <- "Y"
logistic_data <- cbind(logistic_y, logistic_x)
logistic_probs <- gen.probs.list(NA)
logistic_params <- gen.params.list(logistic_data)

# Generate models using MJMCMC
logistic_result <- gmjmcmc(logistic_data, logistic.loglik.aic, NA, transforms, 1, 60000, 60000, logistic_probs, logistic_params)

logistic_rmse_converge <- matrix(NA, 60, 20)
for(i in 1:60) {
  logistic_rmse_converge[i,] <- rmse(logistic_full.renorm, logistic_result, i*1000)
}

# Plot the convergence
plot(rowMeans(logistic_rmse_converge), type="l")

# NAIVE SUBSAMPLING EXPERIMENTS BELOW
naive.linear.g.prior.loglik <- function (y, x, model, complex, params) {
  subsample <- sample.int(nrow(x), 20)
  out <- lm.fit(as.matrix(x[subsample,model]),y[subsample])
  rsquared <- 1-sum(var(out$residuals))/sum(var(y[subsample]))
  p <- out$rank
  n <- 20
  logmarglik <- 0.5*(log(1+params$g)*(n-p) - log(1+params$g*(1-rsquared))*(n-1))*(p!=1)
  return(logmarglik)
}

crime_result_naive <- gmjmcmc(crime_data, naive.linear.g.prior.loglik, NA, transforms, 1, 60000, 60000, crime_probs, crime_params)

rmse_converge_naive <- matrix(NA, 60, 15)
for(i in 1:60) {
  rmse_converge_naive[i,] <- rmse(full_renorm, crime_result_naive, i*1000)
}

plot(rowMeans(rmse_converge_naive), type="l")

test_renorm <- marginal.probs.renorm(crime_result_naive$models[[1]])
names(test_renorm) <- paste0("y",1:15)
barplot(rbind(test_renorm, full_renorm), beside=T)

logistic.loglik.aic.naive <- function (y, x, model, complex, params) {
  subsample <- sample.int(nrow(x), 200)
  suppressWarnings({mod <- fastglm(as.matrix(x[subsample,model]), y[subsample], family=binomial())})
  ret <- -(mod$deviance/2) - mod$rank
  return(ret)
}

logistic.loglik.aic.twostep <- function (y, x, model, complex, params) {
  ret <- tryCatch({
    mod <- twostep(as.matrix(x[,model]), y, 300, 600, "mvc")
    mlik <- loglik.logi(as.matrix(x[,model]), y, mod$par)
    rank <- sum(mod$par != 0)
    return(mlik-rank)},
    error=function(e){
      return(-10000)
    })
  return(ret)
}

logistic_result.twostep <- gmjmcmc(logistic_data, logistic.loglik.aic.twostep, NA, transforms, 1, 10000, 10000, logistic_probs, logistic_params)

logistic_twostep.renorm <- marginal.probs.renorm(logistic_result.twostep$models[[1]])
names(logistic_twostep.renorm) <- paste0("y",1:20)

logistic_twostep_rmse_converge <- matrix(NA, 10, 20)
for(i in 1:10) {
  logistic_twostep_rmse_converge[i,] <- rmse(logistic_full.renorm, logistic_result.twostep, i*1000)
}

plot(rowMeans(logistic_twostep_rmse_converge), type="l")

sort(logistic_twostep.renorm)

model <- rep(T,20)
mod_truth <- fastglm(as.matrix(logistic_x[,model]), logistic_y[,], family=binomial())
mliks <- matrix(NA,200, 1)
for (i in 1:200) {
  subsamp <- sample.int(nrow(logistic_x), 500)
  logistic_data_sub <- logistic_data[subsamp,]
  mod <- fastglm(as.matrix(logistic_data_sub[,c(F,model)]), logistic_data_sub[,1], family=binomial())
  mliks[i] <- mod$deviance/2
}

subsamp <- sample.int(nrow(logistic_x), 200)
mod <- fastglm(as.matrix(logistic_x[subsamp,c(F,model)]), logistic_y[subsamp,1], family=binomial())
mod2 <- fastglm(as.matrix(logistic_x[subsamp,c(F,F,model[-1])]), logistic_y[subsamp,1], family=binomial())

mod_truth <- fastglm(as.matrix(logistic_x[,model]), logistic_y[,], family=binomial())
mod_truth2 <- fastglm(as.matrix(logistic_x[,c(F,model[-1])]), logistic_y[,], family=binomial())

mod$deviance/mod2$deviance
mod_truth$deviance/mod_truth2$deviance

log(mod_truth$deviance)

mlikss <- matrix(NA, 250, 2)
for (i in 1:250) {
  model <- as.logical(c(T,intToBits(i)[1:15]))
  mlikss[i,1] <- logLik(lm(Y ~ ., crime_data[,model]))
  samples <- matrix(NA,20,1)
  for (j in 1:20) {
    subsample <- sample.int(nrow(crime_x), 25)
    samples[j] <- logLik(lm(Y ~ ., crime_data[subsample,model]))
  }
  mlikss[i,2] <- mean(samples)
}
plot(mlikss[,1], type="l", ylim=c(45,75))
lines(mlikss[,2]/25*47, type="l", col="red")

out <- lm(Y ~ ., crime_data)
out2 <- lm(Y ~ ., crime_data[subsample,])

logLik(out)
logLik(out2)/25*47

crime_data[,c(T)]

library(OSMAC)

mlikss2 <- matrix(NA, 500, 3)
betas <- matrix(NA, 500, 21)
for (i in 1:500) {
  model <- as.logical(c(T,intToBits(i)[1:20]))
  glmod_full <- glm(Y ~ . - 1, logistic_data[,model], family = binomial())
  mlikss2[i,1] <- logLik(glmod_full)
  betas[i,model] <- glmod_full$coefficients
  subsample <- sample.int(nrow(logistic_data), 500)
  glmod <- twostep(as.matrix(logistic_data[,c(F,model)]), as.matrix(logistic_data[,1]), 300, 600, "mvc") #glm(Y ~ . - 1, logistic_data[subsample,model], family = binomial())
  mlikss2[i,3] <- loglik.logi(as.matrix(logistic_data[,c(F,model)]), as.matrix(logistic_data[,1]), glmod$par)
}

plot(mlikss2[1:50,3], type="l", ylim=c(-1370,-1270))
lines(mlikss2[1:50,1], col="red")

logistic_xx <- t(t(logistic_x) / apply(logistic_x, 2, sd))

twostep(as.matrix(logistic_xx[,2:6]), as.matrix(logistic_y), 300,800, "mvc")$par
glm.fit(as.matrix(logistic_x[,2:6]), as.matrix(logistic_y), family=binomial())$coefficients

plot(betas[,1], type="l")
plot(betas[!is.na(betas[,2]),2], type="l")
plot(betas[!is.na(betas[,3]),3], type="l")
plot(betas[!is.na(betas[,4]),4], type="l")

plot(mlikss2[,1], type="l")
lines(mlikss2[,3], type="l", col="red")
biases <- matrix(NA, 100,1)
vars <- matrix(NA, 100,1)
for (i in 1:100) biases[i] <- mean(mlikss2[1:i*5,1]-mlikss2[1:i*5,2])
for (i in 1:100) vars[i] <- var(mlikss2[1:i*5,1]-mlikss2[1:i*5,2])

plot(biases, type="l")
plot(vars, type="l")

model <- as.logical(c(T,intToBits(1911)[1:20]))
subsample <- sample.int(nrow(logistic_data), 100)
subsample2 <- sample.int(nrow(logistic_data), 100)
glmmodell <- glm(Y ~ . -1, logistic_data[subsample,c(T,model)], family = binomial())
logLik(glmmodell)

prob <- predict(glmmodell, newdata=logistic_data[subsample2,model], type="response")

sigm <- function (x) (1/(1-exp(-x)))

loglik.logi(as.matrix(logistic_data[subsample2,c(F,model)]), as.matrix(logistic_data[subsample2,1]), glmmodell$coefficients)

loglik.logi <- function (x,y,theta) {
  theta_x <- x%*%theta
  loglis <- -log(1+exp(theta_x))+y*theta_x
  sum(loglis)
}

betas_2 <- matrix(NA, 1023, 11)
mliks_2 <- matrix(NA, 1023, 1)

for (i in 1:1023) {
  model <- as.logical(c(T,intToBits(i)[1:20]))
  glmod_full <- glm(Y ~ . - 1, logistic_data[,model], family = binomial())
  mliks_2[i,1] <- logLik(glmod_full)
  betas_2[i,model[1:11]] <- glmod_full$coefficients
}

glmod.full <- fastglmPure(as.matrix(logistic_data[,2:12]), as.matrix(logistic_data[,1]), start=rep(0,11), family = binomial())

plot(betas_2[!is.na(betas_2[,4]),4], type="l")

betas_3 <- matrix(NA, 100, 11)
mliks_3 <- matrix(NA, 100, 1)
for(i in 1:10) {
  for (j in 1:10) {
    model <- c(T,rep(F,20))
    model[i+1] <- T
    model[j+1] <- T
    glmod_full <- glm(Y ~ . - 1, logistic_data[,model], family = binomial())
    mliks_3[(i-1)*10+j,1] <- logLik(glmod_full)
    betas_3[(i-1)*10+j,model[1:11]] <- glmod_full$coefficients
  }
}

plot(betas_3[1:10,2])

beta_eff <- matrix(NA,10,12)
for (i in 1:10) {
  # Intercepts given single var
  single <- ((i-1)*11+1)
  beta_eff[i,1] <- betas_3[single,1]
  beta_eff[i,2] <- betas_3[single,(i+1)]
  for (j in 1:10) {
    first <- ((i-1)*10)
    beta_eff[i,(j+2)] <-  betas_3[(first+j),(i+1)] - beta_eff[i,2]
  }
}

betas_3[2:10,2] - betas_3[1,2]



covrat <- cor(logistic_data)




