#!/bin/bash

# Job name:
#SBATCH --job-name=MJMCMC RUN
#
# Project:
#SBATCH --account=NN9862K
#
# Wall time limit:
#SBATCH --time=01-12:00:00
#

## Set up job environment:
set -o errexit  # Exit the script on any error
set -o nounset  # Treat any unset variables as an error

module --quiet purge  # Reset the modules to the system default
module load R/4.0.3-foss-2020b
module list

## Do some work:
git clone https://github.com/jonlachmann/thesis_supplementary.git
R CMD BATCH thesis_supplementary/cluster_setup.R
R CMD BATCH thesis_supplementary/cluster_runs/run_210428.R