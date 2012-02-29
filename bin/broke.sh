#!/bin/sh

case $2 in
    p1) num=1;;
    p2) num=2;;
    p3) num=4;;
    p4) num=6;;
    p5) num=9;;
    p6) num=10;;
    n1) num=3;;
    n2) num=5;;
    n3) num=7;;
    n4) num=8;;
esac

cat test/$num.in |xargs ./limit $1 > $num.my 2> $num.my
diff -wBq test/$num.out $num.my > /dev/null 2> /dev/null && exit 0
exit 1
