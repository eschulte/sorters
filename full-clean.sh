#!/bin/sh
# remove *everything*, namely the directories above this one
base=$(basename $(pwd))
echo $base
for config in *.conf;do
    name=$(basename $config .conf)
    echo $name
    cd ../
    rm -rf $name
    cd $base
done
