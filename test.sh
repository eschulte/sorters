#!/bin/sh
#
# check that $1 can successfully sort the output of in input files in
# test/$i.in by comparing the salient output against test/$i.out

# point output stream to $2 if $2 is given
if [ $2 ]; then exec > $2; fi

# mark each passed test with a line
for i in `seq 10`; do
    cat test/$i.in |xargs ./limit $1 > $i.my 2> $i.my
    diff -wBq test/$i.out $i.my > /dev/null 2> /dev/null && echo "passed $i"
    rm -rf $i.my
done
exit 0
