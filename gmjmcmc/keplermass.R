# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-03

source("packages.R")
source("functions.R")
library(GMJMCMC)

kmdata <- as.matrix(read.csv("data/gmjmcmc/exa1.csv"))
kmdata <- cbind(kmdata[,5], kmdata[,-5])

transforms <- c("sigmoid","sini","tanh","atan","troot")
set.transforms(transforms)

truth <- matrix(NA,223,3)
truth[,1] <- troot(kmdata[,5]*kmdata[,5]*kmdata[,7])
truth[,2] <- troot(kmdata[,5]*kmdata[,5]*kmdata[,8])
truth[,3] <- troot(kmdata[,5]*kmdata[,5]*kmdata[,10])
cor(truth)

# Load results from disk
kepler_dir <- "data/gmjmcmc/kepler/"
kepler_runs <- vector("list")
kepler_files <- list.files(path=paste0(kepler_dir))
# for (i in 1:64) kepler_runs[[i]] <- loadRdata(file=paste0(kepler_dir,kepler_files[i]))

sizes <- c(1,8,16,24,32,40,48,56,64)
rateslist <- vector("list")
for (j in 1:length(sizes)) {
  size <- sizes[j]
  rates <- matrix(0, 3, 100)
  for (i in 1:100) {
    print(i)
    kepler_runs <- vector("list")
    # Load data
    for (j in 1:size) {
      # print(kepler_files[size*(i-1)+j])
      kepler_runs[[j]] <- loadRdata(file=paste0(kepler_dir,kepler_files[size*(i-1)+j]))
    }
    keplermerge <- merge.results(kepler_runs, "best", 2, 0.00001)
    detected <- lapply(keplermerge, function(x) x[keplermerge$marg.probs > 0.25])
    attr(detected, "class") <- "gmjmcmcresult"
    #plot(detected)
    test <- GMJMCMC:::precalc.features(cbind(kmdata[,1],1,kmdata[,-1]), detected$features)[,-(1:2), drop=F]
    positive <- 0
    negative <- 0
    for (k in 1:ncol(test)) {
      detected.pos <- sum(cor(cbind(test[,k], truth))[1,-1] > (1-0.000001))
      if (detected.pos > 0) positive <- positive + 1
      else negative <- negative + 1
      print(detected.pos)
    }
    if (positive > 0) {
      print("positive found")
      rates[1,i] <- rates[1,i] + 1
    }
    if (positive > 0) {
      print("positive found")
      rates[2,i] <- rates[2,i] + positive
    }
    if (negative > 0) {
      rates[3,i] <- rates[3,i] + negative
      print("negative found")
    }
  }
  rateslist[[j]] <- rates
}

rowSums(rates)
rateslist <- rateslist[-which(sapply(rateslist, is.null))]
save(rateslist, file="data/gmjmcmc/kepler/rateslist.Rdata")
rateslist2 <- lapply(rateslist, rowMeans)
ratesmat <- matrix(unlist(rateslist2), 3)

total <- ratesmat[2,]+ratesmat[3,]
fdr <- ratesmat[3,] / total

# 1   thread: 204 neg, 6  pos
# 8  threads: 142 neg, 36 pos
# 16 threads: 127 neg, 56 pos
# 24 threads:  78 neg, 75 pos
# 32 threads:  67 neg, 80 pos
# 40 threads:  56 neg, 85 pos
# 48 threads:  42 neg, 86 pos
# 56 threads:  40 neg, 88 pos
# 64 threads:  20 neg, 95 pos



plot(detected)
plot(keplermerge)

bestmargs <- matrix(NA, 40, 64)
for(i in 1:64) bestmargs[,i] <- unlist(kepler_runs[[i]]$best.margs)

multiplot(rowMeans(bestmargs))
multiplot((bestmargs))


resmat <- matrix(NA,100,1)
for(i in 1:length(kepler_runs)) resmat[i,] <- kepler_runs[[i]]$best

sapply(kepler_runs[[80]]$populations[[40]], print)

mods <- matrix(unlist(kepler_runs[[80]]$models[[i]]), ncol=length(kepler_runs[[80]]$models[[i]][[1]]$model)+3, byrow=T)

for(i in 1:40) {
  print(max(matrix(unlist(kepler_runs[[80]]$models[[i]]), ncol=length(kepler_runs[[80]]$models[[i]][[1]]$model)+3, byrow=T)[,length(kepler_runs[[80]]$models[[i]][[1]]$model)+2]))
  print(i)
}
cbind(sapply(kepler_runs[[80]]$populations[[28]], print), t(unlist(GMJMCMC:::marginal.probs.renorm(kepler_runs[[80]]$models[[28]]))))

lab <- sapply(kepler_runs[[80]]$populations[[26]], print)
marg <- unlist(GMJMCMC:::marginal.probs.renorm(kepler_runs[[80]]$models[[26]]))

cbind(lab, t(marg))

km_pars <- gen.params.list(kmdata, T)
km_probs <- gen.probs.list(transforms)

km_probs$filter <- 0.7
km_pars$loglik$r <- 1/223
km_pars$feat$pop.max <- 25
km_pars$feat$keep.org <- T
km_pars$feat$keep.min <- 0.7
km_pars$feat$D <- 8
km_probs$gen <- c(0.45, 0.45, 0.05, 0.05)

num_cores <- detectCores()

pargmj <- mclapply(1:8, function (x) {
  gmjmcmc(kmdata, gaussian.loglik, gaussian.loglik.alpha, transforms, 200, 250, 500, km_probs, km_pars, F)
}, mc.cores = num_cores, mc.preschedule = F)

set.transforms(transforms)

mergeres <- merge.results(pargmj, 2)
resplot(mergeres, 10)

resplot <- function (result, count) {
  tot <- length(result$importance)
  y <- barplot(result$importance[(tot-count):tot], horiz=T)
  text((max(result$importance[(tot-count):tot])/2), y, sapply(result$feats[(tot-count):tot], print))
}





GMJMCMC:::print.dist(mergeres$importance, sapply(mergeres$feats, print), -1)