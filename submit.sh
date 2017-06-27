#! /bin/bash
for i in {1..707}; do
    n=1
    s=$(($(($n * $[$i-1]))+1))
    e=$(($i * $n))
    qsub /home/eem/vv233/whitehall-acc/run-mainscript.sh $s $e
done
