#!/bin/sh

for i in `seq 100`;do
  echo $i
  nice repair merge.elf.conf 1>/dev/null 2>/dev/null
  ./package.sh elf-$i
  ./clean.sh
done
