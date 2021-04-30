# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Load packages
source("packages.R")
# Simulate data
source("sim_data1.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

# Calculate the full model set using 0.1, 0.25, 0.5, 1, 5, and 10% at each iteration
#full_10K_sub_01 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.001)
#full_10K_sub_025 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.0025)
#full_10K_sub_05 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.05)
system.time(full_10Kg_sub_1 <- run_sim(million_x, as.vector(million_y_g), gaussian.loglik.aic.irlssgd, 10000, 1:1000, 0.01))
system.time(full_10Kg <- run_sim(million_x, million_y_g, gaussian.loglik.aic, 10000, 1:1000, 0.01))
#full_10K_sub_5 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.05)
#full_10K_sub_10 <- run_sim(logistic.loglik.aic.irlssgd, 10000, 1:full_model_count, 0.1)

load("data/full_10K_g.Rdata")

full_10Kg_sub_1_mat <- matrix(unlist(full_10Kg_sub_1), ncol=18, byrow=T)
full_10K_g_mat <- matrix(unlist(full_10Kg), ncol=18, byrow=T)
testmat <- matrix(unlist(full_100Kg_005), ncol=18, byrow=T)

compare_g <- matrix(NA,1000,2)
compare_g[,1] <- full_10K_g_mat[1:1000,17]
compare_g[,2] <- full_10Kg_sub_1_mat[1:1000,17]

par(mfrow=c(1,1))
multiplot(compare_g)

cor(compare_g)

load("data/full_enumeration/logistic/full_1M_005.Rdata")
full_1M_005 <- unlist(full_1M_005, recursive = F)
save(full_1M_005, file="data/full_enumeration/logistic/full_1M_005.Rdata")

full_sim_g_files <- list.files(path="data/full_enumeration/gaussian/")
for (file in full_sim_g_files) load(file=paste0("data/full_enumeration/gaussian/",file))

# Calculate the renormalized marginal probabilities
full_10Kg_renorm <- matrix(NA, nvars, 8)
full_10Kg_renorm[,1] <- GMJMCMC:::marginal.probs.renorm(full_10Kg)
full_10Kg_renorm[,2] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_10)
full_10Kg_renorm[,3] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_5)
full_10Kg_renorm[,4] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_1)
full_10Kg_renorm[,5] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_075)
full_10Kg_renorm[,6] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_05)
full_10Kg_renorm[,7] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_025)
full_10Kg_renorm[,8] <- GMJMCMC:::marginal.probs.renorm(full_10Kg_01)

full_100Kg_renorm <- matrix(NA, nvars, 8)
full_100Kg_renorm[,1] <- GMJMCMC:::marginal.probs.renorm(full_100Kg)
full_100Kg_renorm[,2] <- GMJMCMC:::marginal.probs.renorm(full_100Kg5)
full_100Kg_renorm[,3] <- GMJMCMC:::marginal.probs.renorm(full_100Kg1)
full_100Kg_renorm[,4] <- GMJMCMC:::marginal.probs.renorm(full_100Kg075)
full_100Kg_renorm[,5] <- GMJMCMC:::marginal.probs.renorm(full_100Kg05)
full_100Kg_renorm[,6] <- GMJMCMC:::marginal.probs.renorm(full_100Kg025)
full_100Kg_renorm[,7] <- GMJMCMC:::marginal.probs.renorm(full_100Kg01)
full_100Kg_renorm[,8] <- GMJMCMC:::marginal.probs.renorm(full_100Kg005)

barplot(t(full_100Kg_renorm), beside=T)
multiplot(full_100Kg_renorm, legend=T, names=c("Full", "5%", "1%", "0.75%", "0.5%", "0.25%", "0.1%", "0.05%"))
barplot(t(GMJMCMC:::marginal.probs.renorm(full_100Kg)), beside=T)
cor(full_100Kg_renorm)

par(mfrow=c(3,5))
for (i in 1:15) {
  multiplot(abs(full_100Kg_renorm[i,]-full_100Kg_renorm[i,1]))
}

compare_100kg <- matrix(NA, full_model_count, 8)
compare_100kg[,1] <- (matrix(unlist(full_100Kg), ncol=18, byrow=T)[,17])
compare_100kg[,2] <- (matrix(unlist(full_100Kg_5), ncol=18, byrow=T)[,17])
compare_100kg[,3] <- (matrix(unlist(full_100Kg_1), ncol=18, byrow=T)[,17])
compare_100kg[,4] <- (matrix(unlist(full_100Kg_075), ncol=18, byrow=T)[,17])
compare_100kg[,5] <- (matrix(unlist(full_100Kg_05), ncol=18, byrow=T)[,17])
compare_100kg[,6] <- (matrix(unlist(full_100Kg_025), ncol=18, byrow=T)[,17])
compare_100kg[,7] <- (matrix(unlist(full_100Kg_01), ncol=18, byrow=T)[,17])
compare_100kg[,8] <- (matrix(unlist(full_100Kg_005), ncol=18, byrow=T)[,17])

best50_100K <- compare_100kg[,1] > 588.919
multiplot(compare_100kg[best50_100K,1:8], legend=T)

compare_100kg[best50_100K,3]

cor(compare_100kg[best50_100K,1:8])

hist(full_100Kg_mat[,17], breaks=50)

par(mfrow=c(1,1))
barplot(t(full_10Kg_renorm), beside=T)
plot(cor(full_100Kg_renorm)[1,], type="l")

load(file="data/full_enumeration/gaussian/old_full_10Kg_025.Rdata")

full_10Kg_10_mat <- matrix(unlist(full_10Kg_10), ncol=18, byrow=T)
full_10Kg_5_mat <- matrix(unlist(full_10Kg_5), ncol=18, byrow=T)
full_10Kg_1_mat <- matrix(unlist(full_10Kg_1), ncol=18, byrow=T)
full_10Kg_075_mat <- matrix(unlist(full_10Kg_075), ncol=18, byrow=T)
full_10Kg_05_mat <- matrix(unlist(full_10Kg_05), ncol=18, byrow=T)
full_10Kg_025_mat <- matrix(unlist(full_10Kg_025), ncol=18, byrow=T)
full_10Kg_mat <- matrix(unlist(full_10Kg), ncol=18, byrow=T)

compare_g <- matrix(NA,1000,7)
compare_g[,1] <- full_10Kg_mat[1:1000,17]
compare_g[,2] <- full_10Kg_10_mat[1:1000,17]
compare_g[,3] <- full_10Kg_5_mat[1:1000,17]
compare_g[,4] <- full_10Kg_1_mat[1:1000,17]
compare_g[,5] <- full_10Kg_075_mat[1:1000,17]
compare_g[,6] <- full_10Kg_05_mat[1:1000,17]
compare_g[,7] <- full_10Kg_025_mat[1:1000,17]

par(mfrow=c(1,1))
multiplot(compare_g[,c(1,7)])

for (i in 1:5000) {
  set.seed(i)
  full_10Kg_test <- run_sim(million_x, million_y_g, gaussian.loglik.aic.irlssgd, 10000, 12, 0.0075)
  print(full_10Kg_test[[1]]$crit)
  if (is.na(full_10Kg_test[[1]]$crit)) stop(paste0("NA occurred at seed ",i))
  if (is.nan(full_10Kg_test[[1]]$crit)) stop(paste0("NaN occurred at seed ",i))
  if (is.infinite(full_10Kg_test[[1]]$crit)) stop(paste0("Inf occurred at seed ",i))
  if (i %% 100 == 0) print(i)
}

mill <- as.data.frame(cbind(million_x, million_y_g))


simx <- as.matrix(read.table(text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/examples/Simulated%20Data%20%28Example%201%29/simcen-x.txt")))
simy <- as.matrix(read.table(text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/examples/Simulated%20Data%20%28Example%201%29/simcen-y.txt")))
simx <- cbind(1, simx)

full_100g <- mclapply(1:32, function (x) {
  run_sim(xvars, mill_y_g100, linear.g.prior.loglik, 100, model_partitions[x,], 1)
}, mc.cores = num_cores, mc.preschedule = F)
full_100g <- unlist(full_100g, recursive = F)

full_10Kg <- mclapply(1:32, function (x) {
  run_sim(xvars, mill_y_g10K, gaussian.loglik.aic, 10000, model_partitions[x,], 1)
}, mc.cores = num_cores, mc.preschedule = F)
full_10Kg <- unlist(full_10Kg, recursive = F)

full_1Kg <- mclapply(1:32, function (x) {
  run_sim(xvars, mill_y_g1K, linear.g.prior.loglik, 1000, model_partitions[x,], 1)
}, mc.cores = num_cores, mc.preschedule = F)
full_1Kg <- unlist(full_1Kg, recursive = F)

full_2Kg <- mclapply(1:32, function (x) {
  run_sim(xvars, mill_y_g2K, linear.g.prior.loglik, 2000, model_partitions[x,], 1)
}, mc.cores = num_cores, mc.preschedule = F)
full_2Kg <- unlist(full_2Kg, recursive = F)

full_10Kg <- mclapply(1:32, function (x) {
  run_sim(xvars, mill_y_g10K, linear.g.prior.loglik, 10000, model_partitions[x,], 1)
}, mc.cores = num_cores, mc.preschedule = F)
full_10Kg <- unlist(full_10Kg, recursive = F)
mods1 <- run_sim(xvars, mill_y_g2K, linear.g.prior.loglik.irlssgd, 2000, model_partitions[1,], 1)
mods2 <- run_sim(xvars, mill_y_g2K, linear.g.prior.loglik, 2000, model_partitions[1,], 1)

mods1_mat <- matrix(unlist(mods1), ncol=18, byrow=T)
mods2_mat <- matrix(unlist(mods2), ncol=18, byrow=T)

compa <- matrix(NA,1024,2)
compa[,1] <- mods1_mat[,17]
compa[,2] <- mods2_mat[,17]
multiplot(compa)

full_10Kg_mat <- matrix(unlist(full_10Kg), ncol=18, byrow=T)

GMJMCMC:::marginal.probs.renorm(full_10Kg)
mattt <- matrix(NA,15,4)
mattt[,1] <- GMJMCMC:::marginal.probs.renorm(full_100g)
mattt[,2] <- GMJMCMC:::marginal.probs.renorm(full_1Kg)
mattt[,3] <- GMJMCMC:::marginal.probs.renorm(full_2Kg)
mattt[,4] <- GMJMCMC:::marginal.probs.renorm(full_10Kg)
GMJMCMC:::marginal.probs.renorm(full_1Kg)

multiplot(mattt[,4])
hist(full_10Kg_mat[(full_10Kg_mat[,17] > -10000),17], breaks=100)

covmat <- cov(simx)
