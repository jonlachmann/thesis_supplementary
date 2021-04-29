# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

logistic.loglik.aic <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- glm.fit(as.matrix(x[,model]), y, family=binomial())})
  ret <- -(mod$deviance/2) - mod$rank
  return(ret)
}

gaussian.loglik.aic <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- glm.fit(as.matrix(x[,model]), y, family=gaussian())})
  ret <- -(mod$deviance/2) - mod$rank
  return(ret)
}

logistic.loglik.aic.irlssgd <- function (y, x, model, complex, params) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(subs=params$subs, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=params$subs, maxit=500, alpha=0.05, decay=0.99, histfreq=10))
  return(-(mod$deviance/2) - mod$rank)
}

gaussian.loglik.aic.irlssgd <- function (y, x, model, complex, params) {
  mod <- irls.sgd(as.matrix(x[,model]), y, gaussian(),
            irls.control=list(subs=params$subs, maxit=15, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5)),
            sgd.control=list(subs=params$subs, maxit=200, alpha=0.05, decay=0.99, histfreq=10))
  return(-(mod$deviance/2) - mod$rank)
}