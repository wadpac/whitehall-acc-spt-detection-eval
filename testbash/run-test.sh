#! /bin/bash -i
echo "inside run-test.sh"
./etc/profile.d/modules.sh
module load R/3.3.1
R CMD --vanilla --args f0=$1 f1=$2 < /users/vv233/whitehall-acc/test.R
