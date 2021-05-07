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
km_probs$gen <- c(0.9, 0.5, 0.05, 0.05)

num_cores <- detectCores()

pargmj <- mclapply(1:8, function (x) {
  gmjmcmc(kmdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 40, 500, 1000, km_probs, km_pars, F)
}, mc.cores = num_cores, mc.preschedule = F)

set.transforms(transforms)
summary(pargmj[[1]])

res1 <- pargmj[[1]]
attr(res1, "class") <- "gmjmcmc.res"
attributes(res1)

mergeres <- merge.results(pargmj)
print(mergeres$feats[[98]])

res.summary <- summary.gmjresult(gmjres)
importance <- data.frame(res.summary$importance)
names(importance) <- res.summary$features
par(mar=c(14,3,3,3))
barplot(importance, las=2)