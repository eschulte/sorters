#!/bin/sh
debug=$1
tar=$2
pops=$(cat $debug|egrep "pop\[")
sizes=$(cat $debug|egrep "^sizes"|sed 's/sizes://')
orig_size=$(cat $debug|egrep -m 1 "stmt_count|asm: lines"|awk '{print $4}')

robustness()
{
IFS="
"
for try in $(echo "$1"|tr '[' ' '|tr ']' ' '|awk '{print $2}');do
    echo "scale=8; 100.0 * (100.0/$try.0)"|bc
done |awk '{print NR " " $1}'
}

length()
{
IFS="
"
for pop in $(echo "$1");do
    echo "$pop"|tr ' ' '\n'|awk '{sum=sum+$1}END{print sum/NR}'
done |awk '{print NR " " $1}'
}

# under construction -- not used
unique()
{
    hash_file=$(mktemp /tmp/hashes.XXXXXX)
    map_file=$(mktemp /tmp/hists.XXXXXX)
    for i in $(tar tf $tar|egrep -v "\.s");do
        hash=$(tar xfO insertion-c.tar $i|sha1sum|awk '{print $1}')
        echo "$i $hash"
    done > $hash_file
    cat $debug|egrep "^[[:space:]]+10"|awk '{print $3 " " $2}'|sort|join $hash_file - >$map_file
    IFS="
"
    for pop in $(echo "$1"|sed 's/  /\t/g;s/ //g;s/\t/ /g;s/^.\+://');do
        IFS=" "
        for ind in $pop;do
            grep -m 1 $ind $map_file|awk '{print $2}'
        done |tr ' ' '\n'|sort|uniq|wc -l
    done |awk '{print NR " " $1}'    
}

rb=$(mktemp /tmp/robustness.XXXXXX)
robustness "$pops" > $rb

ln=$(mktemp /tmp/lengths.XXXXXX)
length "$sizes" > $ln

join $rb $ln
