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
  worker_id <- (models[1]-1) / length(models)
  models <- models[!is.na(models)]
  cat(paste0("\n","Worker: ",worker_id," simulating ", length(models)," models.\n"))
  res <- vector("list", length(models))
  progress <- 0
  index <- 1
  for (i in models) {
    modelvector <- as.logical(c(T,intToBits(i)[1:15]))
    loglik <- loglik_fun(y[1:nobs], x[1:nobs,], modelvector, NULL, list(subs = subs, g=100/sqrt(nobs/100)))
    res[[index]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
    if (index %% max(floor(length(models)/100),1) == 0) {
      progress <- print.progressbar(progress, 100)
      cat(worker_id)
      gc()
    }
    index <- index + 1
  }
  cat(paste0("\n","Worker: ",worker_id," done simulating ", length(models)," models.\n"))
  return(res)
}

# Function for running a multicore simulation and save the results properly
run_multisim <- function (x, y, loglik, model_parts, n_obs, subs, name, directory) {
  cat(paste0("\n", "Running ", name, " simulation.\n"))
  simres <- mclapply(1:nrow(model_parts), function (mods) {
    run_sim(x, y, loglik, n_obs, model_parts[mods,], subs)
  }, mc.cores = nrow(model_parts), mc.preschedule = F)
  simres <- unlist(simres, recursive = F)
  assign(name, simres)
  filename <- paste0(directory,"/",name,".Rdata")
  eval(parse(text=paste0("save(",name,", file=\"",filename,"\")")))
  cat(paste0("\n", name, " simulation done.\n"))
}

# Function for running a cluster simulation and save the results properly
run_clustersim <- function (x, y, loglik, model_parts, n_obs, subs, name, directory) {
  cat(paste0("\n", "Running ", name, " simulation.\n"))
  logfile <- paste0(directory,"/run",directory,".log")
  clust <- makeCluster(nrow(model_parts), outfile=logfile)
  clusterExport(clust, varlist=c("x", "y", "model_parts", "n_obs", "subs"), envir=environment())
  simres <- parLapply(clust, 1:nrow(model_parts), function (mods) {
    .libPaths("/cluster/home/jola4668/R")
    source("packages.R")
    source("likelihoods1.R")
    source("functions.R")
    run_sim(x, y, loglik, n_obs, model_parts[mods,], subs)
  })
  stopCluster(clust)
  simres <- unlist(simres, recursive = F)
  assign(name, simres)
  filename <- paste0(directory,"/",name,".Rdata")
  eval(parse(text=paste0("save(",name,", file=\"",filename,"\")")))
  cat(paste0("\n", name, " simulation done.\n"))
}

# Function to align model matrix with the number of cores available
align_models <- function (models) {
  num_cores <- detectCores()
  if ((full_model_count %% num_cores) != 0) {
    models <- c(models, rep(NA, (num_cores-(full_model_count %% num_cores))))
  }
  model_partitions <- matrix(models, num_cores, byrow=T)
  return(model_partitions)
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

# Function for creating a plot with confidence intervals
ci_plot <- function(data, row, ...) {
  x_size <- ncol(data$mean)
  plot(-10, xlim=c(1,x_size), ylim=c(0,max(data$high[row,])), ...)
  polygon(c(1:x_size, x_size:1), c(data$low[row,], rev(data$high[row,])),
        col="lightgrey", border=NA)
  lines(data$mean[row,])
}

# Function for getting the renormalized comparison to the real values
renorm_compare <- function (basename, runs, true_renorm) {
  renormalized_estimates <- vector("list", length(runs))
  for (i in 1:length(runs)) {
    renorm <- matrix(NA, nvars, length(subs_list)+1)
    renorm[,1] <- 0
    for (j in 1:length(subs_list)) {
      run_name <- paste0("run",runs[i], "_",basename,subs_list[j]*100)
      run_name <- gsub("\\.", "", run_name)
      renorm[,j+1] <- abs(GMJMCMC:::marginal.probs.renorm(eval(parse(text=run_name))) - true_renorm)
      print(run_name)
    }
    renormalized_estimates[[i]] <- renorm
  }
  return(renormalized_estimates)
}

# Function for getting the renormalized comparison to the real values
renorm_best <- function (basename, runs, count) {
  best_renorm <- vector("list", length(subs_list))
  for (i in 1:length(subs_list)) {
    best_mods <- vector("list", full_model_count)
    sub_mlik <- matrix(NA, full_model_count, count)
    runs_use <- sample.int(length(runs), count, replace = F)
    for (j in 1:length(runs_use)) {
      run_name <- paste0("run",runs[runs_use[j]], "_",basename,subs_list[i]*100)
      run_name <- gsub("\\.", "", run_name)
      sub_mlik[,j] <- matrix(unlist(eval(parse(text=run_name))), full_model_count, 18, byrow=T)[,17]
      print(run_name)
    }
    max_mliks <- apply(sub_mlik, 1, max)
    for (mod in 1:full_model_count) {
      modvec <- as.logical(intToBits(mod)[1:15])
      best_mods[[mod]] <- list(prob=NA, model=modvec, crit=max_mliks[mod], alpha=NA)
    }
    best_renorm[[i]] <- GMJMCMC:::marginal.probs.renorm(best_mods)
  }
  return(best_renorm)
}

# Function for collecting all marginal likelihoods
mlik_collect <- function (basename, runs) {
  all_mliks <- vector("list", length(runs))
  for (i in 1:length(runs)) {
    mliks <- matrix(NA, full_model_count, length(subs_list)+1)
    mliks[,1] <- matrix(unlist(eval(parse(text=basename))), full_model_count, byrow=T)[,17]
    for (j in 1:length(subs_list)) {
      run_name <- paste0("run",runs[i], "_",basename,subs_list[j]*100)
      run_name <- gsub("\\.", "", run_name)
      mliks[,j+1] <- matrix(unlist(eval(parse(text=run_name))), full_model_count, byrow=T)[,17]
      print(run_name)
    }
    all_mliks[[i]] <- mliks
  }
  return(all_mliks)
}