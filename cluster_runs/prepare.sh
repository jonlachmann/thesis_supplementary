#!/bin/bash

# Prepare everything for a run
if cd thesis_supplementary; then git pull; else git clone https://github.com/jonlachmann/thesis_supplementary.git thesis_supplementary; cd thesis_supplementary; fi
Rscript cluster_setup.R
