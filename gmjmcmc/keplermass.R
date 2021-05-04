# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

library(GMJMCMC)

kmdata <- as.matrix(read.csv("data/gmjmcmc/exa1.csv"))
kmdata <- cbind(kmdata[,5], kmdata[,-5])

transforms <- c("cosi","sigmoid","tanh","atan","sini","troot")
transforms <- c("troot")

km_pars <- gen.params.list(kmdata, T)
km_probs <- gen.probs.list(transforms)

km_probs$filter <- 0.4
km_pars$loglik$r <- 0.1 #2.23
km_pars$feat$pop.max <- 14
km_probs$gen <- c(0.5, 0.3, 0.1, 0.1)


gmjres <- gmjmcmc(kmdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 40, 250, 2000, km_probs, km_pars, F)

