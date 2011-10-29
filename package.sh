#!/bin/sh
# name=$1
name=$1
mkdir $name
mv 0* $name/
mv repair.* $name/
mv *cache $name/
tar cjf $name.tar.bz2 $name
