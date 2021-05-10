# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-10

transforms <- c("sigmoid","sini","tanh","atan","troot")

blrdata <- cbind(blr_y, blr_x)

blr_pars <- gen.params.list(blrdata, T)
blr_probs <- gen.probs.list(transforms)

blr_probs$filter <- 0.8
blr_pars$loglik$r <- 1/nrow(blrdata)
blr_pars$feat$pop.max <- 50
blr_pars$feat$keep.org <- F
blr_pars$feat$keep.min <- 0.6
blr_pars$feat$D <- 5
blr_probs$gen <- c(10, 5, 1, 1)
blr_pars$rescale.large <- T

blrres <- gmjmcmc(blrdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 40, 250, 2000, blr_probs, blr_pars, F)