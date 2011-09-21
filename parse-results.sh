#!/bin/sh
# package parse and show the results
cd ../
for program in bubble insertion merge quick;do
    for language in c cpp hs ml;do
        base="$program-$language"
        cd $base
        ../package.sh $base 2>/dev/null
        cd $base
        ../../remove-dups.sh repair.debug.*>results
        echo -n "| $base "
        ../../show-results.sh results 10|tail -1
        cd ../../
    done
done
