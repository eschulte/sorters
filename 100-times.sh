#!/bin/sh

for i in `seq 100`;do
  nice repair merge.elf.conf
  ./package.sh elf-$i
done
