#! /bin/bash
echo "inside run-test.sh"
. /etc/profile.d/modules.sh
PATH=$PATH:/cm/shared/apps/R/R-3.1.1/bin/
module load R/3.1.1 
module load gcc
module load sge
R --vanilla --args f0=$1 f1=$2 < /users/vv233/whitehall-acc/testbash/test.R
echo "R command submitted"
