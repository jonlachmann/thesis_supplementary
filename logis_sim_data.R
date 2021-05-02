# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-05-02

source("gauss_sim_data.R")

# Generate if that flag is set, otherwise all data is loaded when sourcing the gauss_sim_data.R file
if (exists("generate")) {
  print("Generating data...")
  set.seed(1911)
  mill_y_eta_l1M <- (1/(1+exp(-mill_y_g1M+mean(mill_y_g1M))))
  mill_y_l1M <- rbinom(nobs, 1, mill_y_eta_l1M)

  mill_y_eta_l100K <- (1/(1+exp(-mill_y_g100K+mean(mill_y_g100K))))
  mill_y_l100K <- rbinom(nobs, 1, mill_y_eta_l100K)

  mill_y_eta_l10K <- (1/(1+exp(-mill_y_g10K+mean(mill_y_g10K))))
  mill_y_l10K <- rbinom(nobs, 1, mill_y_eta_l10K)

  mill_y_eta_l4K <- (1/(1+exp(-mill_y_g4K+mean(mill_y_g4K))))
  mill_y_l4K <- rbinom(nobs, 1, mill_y_eta_l4K)

  mill_y_eta_l2K <- (1/(1+exp(-mill_y_g2K+mean(mill_y_g2K))))
  mill_y_l2K <- rbinom(nobs, 1, mill_y_eta_l2K)
  if (exists("save_data")) {
    print("Saving data...")
    save(mill_y_l1M, file="data/ex1_sim/mill_y_l1M.Rdata")
    save(mill_y_l100K, file="data/ex1_sim/mill_y_l100K.Rdata")
    save(mill_y_l10K, file="data/ex1_sim/mill_y_l10K.Rdata")
  }
}
