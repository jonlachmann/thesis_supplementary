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

# Set up simulation parameters
num_cores <- detectCores()
model_partitions <- matrix(1:full_model_count, 32, byrow=T)
options(warn=1)

set.seed(Sys.time())

n_obs <- 100000
dirname <- create_randdir()
set.seed(as.numeric(dirname)+Sys.time())
basename <- paste0("run",dirname, "_full_100Kl")
subs_list <- c(0.2,0.1,0.05,0.01,0.0075,0.005,0.0025,0.001,0.0005)

# Run using full data
cat(paste0("\n", "Running ", basename, " simulation.\n"))
run_multisim(million_x, million_y_l, logistic.loglik.aic.irlssgd, model_partitions, n_obs, 1, basename, dirname)
cat(paste0("\n", basename, " simulation done.\n"))

# Run using various subsample sizes
for (subs in subs_list) {
  name <- paste0(basename, subs*100)
  name <- gsub("\\.", "", name)
  cat(paste0("\n", "Running ", name, " simulation.\n"))
  run_multisim(million_x, million_y_l, logistic.loglik.aic.irlssgd, model_partitions, n_obs, subs, name, dirname)
  cat(paste0("\n", name, " simulation done.\n"))
}