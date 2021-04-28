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
if cd thesis_supplementary; then git pull; else git clone https://server/thesis_supplementary thesis_supplementary; fi
Rscript cluster_setup.R
Rscript cluster_runs/run_210428.R password