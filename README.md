# Code related to the analyses of sleep diary and accelerometer data in the Whitehall study II
See for more information [insert reference to paper].

## call_GGIRshell.R

Binary accelerometer data are processed to derive night specific estimates of sleep parameters based on:
- Heuristic algorithm
- L5+/-6hr approach

## evaluate_model2.R
Output from previous step is cleaned and evaluated.

## run-mainscript.R, submit.sh
Bash scripts to enable running call_GGIRshell.R on the cluster.