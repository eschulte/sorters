#!/bin/bash
#
# Multithreaded caps in the shell
#
./lorem 24|tee                               \
    >(head -6|sed 's/ [a-z]/\U&/g')          \
    >(head -12|tail -6|sed 's/ [a-z]/\U&/g') \
    >(head -18|tail -6|sed 's/ [a-z]/\U&/g') \
    >(tail -6|sed 's/ [a-z]/\U&/g')
