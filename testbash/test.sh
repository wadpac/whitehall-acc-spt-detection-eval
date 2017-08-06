#! /bin/bash
echo "inside test.sh"
for i in {1..2}; do
    n=1
    s=$(($(($n * $[$i-1]))+1))
    e=$(($i * $n))
    /cm/shared/apps/sge/2011.11/bin/linux-x64/qsub -w e -N testjob -l h_vmem=6G /users/vv233/whitehall-acc/testbash/run-test.sh $s $e
done
