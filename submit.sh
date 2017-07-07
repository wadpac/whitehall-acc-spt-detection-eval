#! /bin/bash
for i in {101..500}; do
    n=1
    s=$(($(($n * $[$i-1]))+1))
    e=$(($i * $n))
    /cm/shared/apps/sge/2011.11/bin/linux-x64/qsub -w e -N run1 -l h_vmem=8G /users/vv233/whitehall-acc/run-mainscript.sh $s $e
done
