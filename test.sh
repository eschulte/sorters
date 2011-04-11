#!/bin/bash


# check that $1 can successfully sort the output of in input files in
# test/$i.in by comparing the salient output against test/$i.out
#
# mark each passed test with a line in $2
#
rm -rf $2
for i in `seq 10`; do
    cat test/$i.in |xargs ./limit $1 >& $i.my
    diff -wBq test/$i.out $i.my && (echo "passed $i" >> $2)
    rm -rf $i.my
done
exit 0
