#!/bin/sh
# convert samples (see sample.sh) into lines of code in the related
# assembly files
for program in bubble insertion merge quick;do
    for language in c cpp hs ml;do
        exe="src/$program-$language"
        asm="src/$program-$language.s"
        smp="src/$program-$language.samp"
        adr="src/$program-$language.addrs"
        map="src/$program-$language.map"
        pth="src/$program-$language.path"
        ./mem-mapping $asm $exe|tr '[A-F]' '[a-f]' > $map
        cat $smp|cut -c1-24|sed 's/://g' \
            |egrep "^ +[0-9]+ +[0-9\.]+ +[0-9a-f]+$" \
            |awk '{print $3 " " $1}'|sort> $adr
        join $map $adr|awk '{print $2}'|sort -n|uniq > $pth
    done
done
