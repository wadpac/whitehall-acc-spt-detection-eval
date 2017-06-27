#! /bin/bash
#$ cwd -V
#$ -l h_vmem=12G
/usr/lib64/R --vanilla --args f0=$1 f1=$2 < /home/eem/vv233/whitehall-acc/call_GGIRshell.R
