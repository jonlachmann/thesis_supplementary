# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load packages
source("packages.R")
# Simulate data
source("gauss_sim_data.R")
# Load likelihood functions
source("likelihoods1.R")
# Load common functions
source("functions.R")

# Set up simulation parameters
num_cores <- detectCores()
model_partitions <- matrix(c(1:full_model_count, rep(NA,16)), 48, byrow=T)
options(warn=1)

n_obs <- 10000
dirname <- create_randdir()
set.seed(dirname)
basename <- paste0("run",dirname, "_full_10Kg")
subs_list <- c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001)

# Run using full data
cat(paste0("\n", "Running ", basename, " simulation.\n"))
run_multisim(mill_x_g, mill_y_g10K, linear.g.prior.loglik, model_partitions, n_obs, 1, basename, dirname)
cat(paste0("\n", basename, " simulation done.\n"))

# Run using various subsample sizes
for (subs in subs_list) {
  name <- paste0(basename, subs*100)
  name <- gsub("\\.", "", name)
  cat(paste0("\n", "Running ", name, " simulation.\n"))
  run_multisim(mill_x_g, mill_y_g10K, linear.g.prior.loglik.irlssgd, model_partitions, n_obs, subs, name, dirname)
  cat(paste0("\n", name, " simulation done.\n"))
}