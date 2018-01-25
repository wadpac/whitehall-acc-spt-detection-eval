# Code related to the evaluation of Sleep Period Time window detection algorithm in the Whitehall study II
See for more information [insert reference to paper: van Hees bioRxiv et al. 2018].

## call_GGIRshell.R
Central code to process binary accelerometer data to derive night specific estimates of sleep parameters based on: HDCZA and L5+/-6hr algorithms. To run this script on the cluster we used run-mainscript.R and submit.sh. The file config.txt is a configuration file used by these scripts and indicates the location of the data, the output directory, the location R itself, and the logcation of the sleeplog (diary) data.

## compare_psg_accmodels.R
Script that loads the output from the previous step, cleans it, and then compares the two algorithms.
