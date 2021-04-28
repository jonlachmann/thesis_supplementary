# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-28

# Set path for packages
.libPaths("/cluster/home/jola4668/R")

# Load devtools
install.packages("usethis", repos='http://cran.us.r-project.org')
install.packages("devtools", repos='http://cran.us.r-project.org')
library(devtools)

# Load RCurl
install.packages("RCurl", repos='http://cran.us.r-project.org')
library(RCurl)

# Install our packages
install_github("jonlachmann/GMJMCMC", build_vignettes=F)
install_github("jonlachmann/irls.sgd", build_vignettes=F)

install.packages("parallel", repos='http://cran.us.r-project.org')
library(parallel)

system.time(mjmcmc_10K_test <- mclapply(1:4, function (x) {
  mjmcmc(cbind(million_y_l, million_x)[1:10000,], logistic.loglik.aic, 500, sim_probs, sim_pars)
}, mc.cores = 4))