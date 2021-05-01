# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-29

logistic_full_files <- list.files(path="data/full_enumeration/logistic/")
for (file in logistic_full_files) load(file=paste0("data/full_enumeration/logistic/",file))

# Calculate the renormalized marginal probabilities
full_1M_renorm <- matrix(NA, nvars, 6)
full_1M_renorm[,1] <- GMJMCMC:::marginal.probs.renorm(full_1M)
full_1M_renorm[,2] <- GMJMCMC:::marginal.probs.renorm(full_1M_1)
full_1M_renorm[,3] <- GMJMCMC:::marginal.probs.renorm(full_1M_05)
full_1M_renorm[,4] <- GMJMCMC:::marginal.probs.renorm(full_1M_01)
full_1M_renorm[,5] <- GMJMCMC:::marginal.probs.renorm(full_1M_005) # This one is wrong!!
full_1M_renorm[,6] <- GMJMCMC:::marginal.probs.renorm(full_1M_001)

barplot(t(full_1M_renorm), beside=T)
cor(full_1M_renorm)

mattt <- matrix(unlist(run1135_full_100Kl), ncol=18, byrow=T)
