#! /bin/bash
#$ cwd -V
#$ -l h_vmem=12G
/cm/shared/apps/R/3.3.1/bin/R --vanilla --args f0=$1 f1=$2 < /users/vv233/whitehall-acc/call_GGIRshell.R
