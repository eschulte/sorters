#!/bin/sh
num=$(echo $2|cut -c2-)

cat test/$num.in |xargs ./limit $1 > $num.my 2> $num.my
diff -wBq test/$num.out $num.my > /dev/null 2> /dev/null && exit 0
exit 1
