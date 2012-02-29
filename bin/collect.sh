#!/bin/sh
for i in `seq 100`;do
    cp elf-$i/repair.debug.* debugs/$i.debug
done
