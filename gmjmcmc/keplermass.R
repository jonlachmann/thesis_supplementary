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
km_pars$loglik$r <- 1/223
km_pars$feat$pop.max <- 23
km_pars$feat$keep.org <- T
km_probs$gen <- c(0.7, 0.2, 0.05, 0.05)

gmjres <- gmjmcmc(kmdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 40, 50, 2000, km_probs, km_pars, F)


kmdata2 <- cbind(1,kmdata[,-1])
kmdata2[,2] <- troot(kmdata2[,5]*kmdata2[,5]*kmdata2[,8])
kmdata2[,2] <- kmdata2[,8] #*kmdata2[,5]*kmdata2[,8]


gaussian.loglik2(kmdata[,1], kmdata2, c(T,T,F,F,F,F,F,F,F,F,F), complex=list(width=rep(0,2)), list(r=1/223))
gaussian.loglik3(kmdata[,1], kmdata2, c(T,T,F,F,F,F,F,F,F,F,F), complex=list(width=rep(0,2)), list(r=1/223))



gaussian.loglik2 <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- fastglm(as.matrix(x[,model]), y, family=gaussian())})
  ret <- (-(mod$deviance -2*log(params$r)*sum(complex$width)))/2
  return(ret)
}

gaussian.loglik3 <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- glm.fit(as.matrix(x[,model]), y)})
  ret <- (-(mod$deviance -2*log(params$r)*sum(complex$width)))/2
  return(ret)
}