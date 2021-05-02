# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load packages
source("packages.R")
# Simulate data
source("logis_sim_data.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

# Align runs with number of cores
model_partitions <- align_models(1:full_model_count)

options(warn=1)

set.seed(Sys.time())

n_obs <- 100000
dirname <- create_randdir()
set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_full_100Kl")
subs_list <- c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

# Run using full data
#run_multisim(mill_x_g, mill_y_l100K, logistic.loglik.bic, model_partitions, n_obs, 1, basename, dirname)

# Run using various subsample sizes
for (subs in subs_list) {
  name <- paste0(basename, subs*100)
  name <- gsub("\\.", "", name)
  run_clustersim(mill_x_g, mill_y_l100K, logistic.loglik.bic.irlssgd, model_partitions, n_obs, subs, name, dirname)
}