# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

# Set up simulations study parameters
nvars <- 15
nobs <- 10^6
full_model_count <- 2^nvars

# Generate the data to use
{
  set.seed(1911)
  covmat <- matrix(rnorm(nvars^2, runif(nvars^2, -5,5), runif(nvars^2, 0, 5)), nvars)
  covmat <- covmat %*% t(covmat)

  million_x <- cbind(1, matrix(rmvnorm(nobs, runif(nvars, -5,5), covmat), nobs))
  covars <- sample.int(nvars, 5)
  betas <- runif(6, -10, 10)
  million_y_g <- million_x[,c(1,covars)] %*% betas + rnorm(nobs, 0, 3)
  million_y_l <- rbinom(nobs, 1, (1/(1+exp(-million_y_g))))
}

# Load likelihood functions
source("likelihoods1.R")
source("functions.R")

# Create a plot demonstrating convergence in logistic case
irlssgd_res_l <- irls.sgd(million_x, million_y_l, binomial(),
            irls.control=list(subs=0.0005, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.0005, maxit=500, alpha=0.05, decay=0.99))

true_glm_res <- glm.fit(million_x, million_y_l, family=binomial())

par(mfrow=c(4,4))
for (i in 1:16) {
  multiplot(cbind(c(irlssgd_res_l$irls_hist[,i], irlssgd_res_l$sgd_hist[,i]), true_glm_res$coefficients[i]),
            frame.plot=F,
            ylab=bquote(beta[.(i-1)]), xlab="Iteration")
  abline(v=length(irlssgd_res_l$irls_hist[,i]))
}

# Calculate the full model set using regular glm (SLOW!)
full_10K <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, NULL)
  full_10K[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/40) == 0) progress <- print.progressbar(progress, 40)
}

loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], rep(T,16), NULL, list(subs = 0.03))