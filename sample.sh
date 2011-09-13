#!/bin/sh
for program in bubble insertion merge quick;do
    for language in c cpp hs ml;do
        exe="src/$program-$language"
        echo $exe
        for i in $(seq 1000);do
            for i in $(seq 10);do
                ./test.sh $exe p$i;
            done
        done
        opannotate -a $exe > $exe.samp
    done
done
