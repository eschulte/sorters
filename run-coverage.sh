#!/bin/sh
#
# run coverage.[pos|neg] scripts
# $1=num-runs
# $2=num-pos-tests
# $3=num-neg-tests
for i in `seq $1`;do
  for p in `seq $2`;do
    nice ./test.sh ./coverage.pos p$p
  done
  for n in `seq $3`;do
    nice ./test.sh ./coverage.neg n$n
  done
done
