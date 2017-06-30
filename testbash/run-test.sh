#! /bin/bash
echo "inside run-test.sh"
. /etc/profile.d/modules.sh
module load R/3.3.1 
echo "module R loaded"
R --args f0=$1 f1=$2 < /users/vv233/whitehall-acc/testbash/test.R
echo "R command submitted"
