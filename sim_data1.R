# Title     : Simulation data 1
# Objective : Generate data for the first simulation study
# Created by: jonlachmann
# Created on: 2021-04-26

# Set up simulations study parameters
nvars <- 15
nobs <- 10^6
full_model_count <- 2^nvars

# Generate the data to use
{
  #set.seed(1911)
  #covmat <- matrix(rnorm(nvars^2, runif(nvars^2, -5,5), runif(nvars^2, 0, 5)), nvars)
  #covmat <- covmat %*% t(covmat)

  #million_x <- cbind(1, matrix(rmvnorm(nobs, runif(nvars, -5,5), covmat), nobs))
  #covars <- sample.int(nvars, 4)
  #betas <- runif(length(covars)+1, -10, 10)
  #million_y_g <- (million_x[,c(1,covars)] * rnorm(nobs*(length(covars)+1), 1, 0.5)) %*% betas
  #million_y_eta <- (1/(1+exp(-million_y_g)))
  #million_y_l <- rbinom(nobs, 1, million_y_eta)
}
million_x_g <- cbind(1, rnorm(nobs, million_x[,7]*million_x[,15], 100),
                     million_x[,3:12], rnorm(nobs, million_x[,7],10),
                     rnorm(nobs, million_x[,8],10), rnorm(nobs), million_x[,16])
million_y_g <- rnorm(nobs, ((million_x[,c(1,covars)]) %*% betas), 50)

cormat <- cor(cbind(million_y_g, million_x_g))


#save(covmat, file="data/sim_data1/covmat.Rdata")
#save(covars, file="data/sim_data1/covars.Rdata")
#save(betas, file="data/sim_data1/betas.Rdata")
#million_x_1 <- million_x[1:500000,]
#million_x_2 <- million_x[500001:1000000,]
#save(million_x_1, file="data/sim_data1/million_x_1.Rdata")
#save(million_x_2, file="data/sim_data1/million_x_2.Rdata")
#save(million_y_g, file="data/sim_data1/million_y_g.Rdata")
#save(million_y_l, file="data/sim_data1/million_y_l.Rdata")
sim_data1_files <- list.files(path="data/sim_data1/")
for (file in sim_data1_files) load(file=paste0("data/sim_data1/",file))
million_x <- rbind(million_x_1, million_x_2)
remove(million_x_1, million_x_2, sim_data1_files)