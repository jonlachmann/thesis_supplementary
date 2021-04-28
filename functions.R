# Title     : General functions
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

# Plot many columns in a matrix, log scale can be enabled too
multiplot <- function (mat, logscale=F, ylim=c(min(mat), max(mat)), ...) {
  if (logscale) {
    mat[mat > 0] <- log(mat[mat > 0])
    mat[mat < 0] <- -log(-mat[mat < 0])
  }
  mat <- as.matrix(mat)
  rbcol <- rainbow(ncol(mat))
  plot(mat[,1], type="l", ylim=ylim, col=rbcol[1], ...)
  if (ncol(mat) > 1) for (i in 2:ncol(mat)) lines(mat[,i], col=rbcol[i])
}

# Print a progress bar while iterating over a population
print.progressbar <- function (progress, size=40) {
  cat("\r", "|")
  for (p in 1:size-1) {
    if (progress >= p) cat("=")
    else cat(" ")
  }
  cat("|")
  return(progress+1)
}

# Calculate rmse for the first iters iterations when also having the full renormalized probabilities
rmse <- function (full, sim, iters) {
  sim_renorm <- GMJMCMC:::marginal.probs.renorm(sim$models[1:iters])
  names(sim_renorm) <- paste0("y",1:length(sim_renorm))
  sim_renorm <- sim_renorm[order(full)]
  full <- sort(full)
  rmse <- sqrt((sim_renorm - full)^2)
  return(rmse*100)
}

rmse_conv <- function (full, sim, steps) {
  rmse_converge <- matrix(NA, steps, length(full))
  step_size <- length(sim$models) / steps
  for(i in 1:steps) {
    rmse_converge[i,] <- rmse(full, sim, i*step_size)
  }
  return(rmse_converge)
}

# Function for running simulations on many models with varying subsample size
run_sim <- function (loglik_fun, nobs, models, subs) {
  res <- vector("list", length(models))
  progress <- 0
  for (i in models) {
    modelvector <- as.logical(c(T,intToBits(i)[1:15]))
    loglik <- loglik_fun(million_y_l[1:nobs], million_x[1:nobs,], modelvector, NULL, list(subs = subs))
    res[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
    if (i %% floor(length(models)/100) < 0.5) progress <- print.progressbar(progress, 100)
  }
  return(res)
}