# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

source("packages.R")
source("functions.R")
library(GMJMCMC)

kmdata <- as.matrix(read.csv("data/gmjmcmc/exa1.csv"))
kmdata <- cbind(kmdata[,5], kmdata[,-5])

transforms <- c("sigmoid","sini","tanh","atan","troot")
set.transforms(transforms)

# Load results from disk
kepler_dir <- "data/gmjmcmc/kepler/"
kepler_runs <- vector("list")
kepler_files <- list.files(path=paste0(kepler_dir))
for (i in 1:length(kepler_files)) kepler_runs[[i]] <- loadRdata(file=paste0(kepler_dir,kepler_files[i]))



kepler200b <- merge.results(kepler_runs, "best", 2)

plot(kepler200b, 20)


resmat <- matrix(NA,100,1)
for(i in 1:length(kepler_runs)) resmat[i,] <- kepler_runs[[i]]$best

sapply(kepler_runs[[80]]$populations[[40]], print)

mods <- matrix(unlist(kepler_runs[[80]]$models[[i]]), ncol=length(kepler_runs[[80]]$models[[i]][[1]]$model)+3, byrow=T)

for(i in 1:40) {
  print(max(matrix(unlist(kepler_runs[[80]]$models[[i]]), ncol=length(kepler_runs[[80]]$models[[i]][[1]]$model)+3, byrow=T)[,length(kepler_runs[[80]]$models[[i]][[1]]$model)+2]))
  print(i)
}
cbind(sapply(kepler_runs[[80]]$populations[[28]], print), t(unlist(GMJMCMC:::marginal.probs.renorm(kepler_runs[[80]]$models[[28]]))))

lab <- sapply(kepler_runs[[80]]$populations[[26]], print)
marg <- unlist(GMJMCMC:::marginal.probs.renorm(kepler_runs[[80]]$models[[26]]))

cbind(lab, t(marg))

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

pargmj <- mclapply(1:8, function (x) {
  gmjmcmc(kmdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 200, 250, 500, km_probs, km_pars, F)
}, mc.cores = num_cores, mc.preschedule = F)

set.transforms(transforms)

mergeres <- merge.results(pargmj, 2)
resplot(mergeres, 10)

resplot <- function (result, count) {
  tot <- length(result$importance)
  y <- barplot(result$importance[(tot-count):tot], horiz=T)
  text((max(result$importance[(tot-count):tot])/2), y, sapply(result$feats[(tot-count):tot], print))
}





GMJMCMC:::print.dist(mergeres$importance, sapply(mergeres$feats, print), -1)