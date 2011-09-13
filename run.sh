#!/bin/sh
# run one instance for each configuration file
base=$(basename $(pwd))
echo $base
for config in *.conf;do
    name=$(basename $config .conf)
    echo $name
    cd ../
    mkdir $name
    for sub in $config src test test.sh limit;do cp -R $base/$sub $name/; done
    cd $name
    echo -n "in $(pwd) "
    nice repair $config &
    disown %1
    cd ../$base
done
