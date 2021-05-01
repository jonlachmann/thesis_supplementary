#!/bin/bash

## Setup job
JOB_FILE="job_$RANDOM.out"
while test -f "JOB_FILE"; do
    JOB_FILE="job_$RANDOM.out"
done

## Do some work:
Rscript cluster_runs/$1 > $JOB_FILE 2>&1 &