#!/bin/sh
for i in `ls src/*.c`;do
    binary=src/`basename $i .c`
    for i in `seq 10000`;do
        ./test.sh $binary > /dev/null
    done
    opannotate --assembly $binary > $binary.path
done
