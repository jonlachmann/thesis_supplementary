# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

source("packages.R")
library(GMJMCMC)

kmdata <- as.matrix(read.csv("data/gmjmcmc/exa1.csv"))
kmdata <- cbind(kmdata[,5], kmdata[,-5])

transforms <- c("cosi","sigmoid","tanh","atan","sini","troot")

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

mergeres <- merge.results(pargmj)
resplot(mergeres, 10)

resplot <- function (result, count) {
  tot <- length(result$importance)
  y <- barplot(result$importance[(tot-count):tot], horiz=T)
  text((max(result$importance[(tot-count):tot])/2), y, sapply(result$feats[(tot-count):tot], print))
}





GMJMCMC:::print.dist(mergeres$importance, sapply(mergeres$feats, print), -1)