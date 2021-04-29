#!/bin/bash

# Job name:
#SBATCH --job-name=MJMCMC_RUN
#
# Project:
#SBATCH --account=NN9862K
#
# Wall time limit:
#SBATCH --time=00-02:00:00
#SBATCH --mem-per-cpu=3G
#SBATCH --ntasks=32
#

## Set up job environment:
set -o errexit  # Exit the script on any error
set -o nounset  # Treat any unset variables as an error

module --quiet purge  # Reset the modules to the system default
module load R/4.0.3-foss-2020b
module list

## Do some work:
Rscript cluster_runs/20210429/run_210429_1.R
tar -cf results.tar *.Rdata