# Title     : General functions
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

# Plot many columns in a matrix, log scale can be enabled too
multiplot <- function (mat, logscale=F, ylim=c(min(mat), max(mat)), legend=F, names=names(mat), ...) {
  if (logscale) {
    mat[mat > 0] <- log(mat[mat > 0])
    mat[mat < 0] <- -log(-mat[mat < 0])
  }
  mat <- as.matrix(mat)
  rbcol <- rainbow(ncol(mat))
  plot(mat[,1], type="l", ylim=ylim, col=rbcol[1], ...)
  if (ncol(mat) > 1) for (i in 2:ncol(mat)) lines(mat[,i], col=rbcol[i])
  if (legend) {
    if (is.null(names)) names <- 1:ncol(mat)
    legend("bottomright", col=rbcol, legend=names, lty=1)
  }
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
run_sim <- function (x, y, loglik_fun, nobs, models, subs) {
  models <- models[!is.na(models)]
  res <- vector("list", length(models))
  progress <- 0
  index <- 1
  for (i in models) {
    modelvector <- as.logical(c(T,intToBits(i)[1:15]))
    loglik <- loglik_fun(y[1:nobs], x[1:nobs,], modelvector, NULL, list(subs = subs, g=100/sqrt(nobs/100)))
    res[[index]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
    if (index %% max(floor(length(models)/100),1) == 0) progress <- print.progressbar(progress, 100)
    index <- index + 1
  }
  return(res)
}

# Function for running a multicore simulation and save the results properly
run_multisim <- function (x, y, loglik, model_parts, n_obs, subs, name, directory) {
  simres <- mclapply(1:nrow(model_parts), function (mods) {
    run_sim(x, y, loglik, n_obs, model_parts[mods,], subs)
  }, mc.cores = nrow(model_parts), mc.preschedule = F)
  simres <- unlist(simres, recursive = F)
  assign(name, simres)
  filename <- paste0(directory,"/",name,".Rdata")
  eval(parse(text=paste0("save(",name,", file=\"",filename,"\")")))
}

create_randdir <- function () {
  dirname <- as.character(sample.int(1911, 1))
  while (dir.exists(dirname)) dirname <- as.character(sample.int(1911, 1))
  dir.create(dirname)
  return(dirname)
}

# Function for calculating the quantiles and mean of many matrices
matrix_quantmean <- function (matlist, quantiles=c(0.05, 0.95)) {
  mat_all <- matrix(NA, nrow(matlist[[1]])*ncol(matlist[[1]]), length(matlist))
  count <- 0
  for (i in 1:length(matlist)) {
    if (!is.null(matlist[[i]])) {
      count <- count + 1
      mat_all[,count] <- as.vector(matlist[[i]])
    }
  }
  mat_all <- mat_all[,1:count]
  mat_mean <- matrix(rowMeans(mat_all), nrow(matlist[[1]]), ncol(matlist[[1]]))
  mat_low <- matrix(apply(mat_all, 1, quantile, probs = quantiles[1]), nrow(matlist[[1]]), ncol(matlist[[1]]))
  mat_high <- matrix(apply(mat_all, 1, quantile, probs = quantiles[2]), nrow(matlist[[1]]), ncol(matlist[[1]]))
  return(list(mean=mat_mean, low=mat_low, high=mat_high))
}