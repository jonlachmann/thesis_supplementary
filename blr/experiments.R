# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-10

source("packages.R")
source("blr/sim_blr.R")
source("functions.R")

transforms <- c("sigmoid","sini","tanh","atan","troot")
set.transforms(transforms)

blrdata <- cbind(blr_y, blr_x)

blr_pars <- gen.params.list(blrdata, T)
blr_probs <- gen.probs.list(transforms)

blr_probs$filter <- 0.8
blr_pars$loglik$r <- 1/nrow(blrdata)
blr_pars$feat$pop.max <- 70
blr_pars$feat$keep.org <- T
blr_pars$feat$keep.min <- 0.9
blr_pars$feat$D <- 5
blr_probs$gen <- c(10, 0, 0, 0)
blr_pars$rescale.large <- T

blrres2 <- gmjmcmc(blrdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 40, 500, 2000, blr_probs, blr_pars, F)

plot(blrres2, 20)

multiplot(unlist(blrres2$best.margs))

mergeres <- merge.results(list(blrres), "all")

plot(mergeres, 10)
blr_dir <- "299/"
blr_dir <- "data/blr/results3/"
blr_runs <- vector("list")
blr_files <- list.files(path=paste0(blr_dir))
for (i in 1:10) blr_runs[[i]] <- loadRdata(file=paste0(blr_dir,blr_files[i]))

mergeres_blr <- merge.results(blr_runs, "best", 2, 0.0001)
par(mfrow=c(1,1))
plot(mergeres_blr, 35)

load("191/run191_gmjmcmc_blr10_1.Rdata")

plot(run191_gmjmcmc_blr10_1, 20)