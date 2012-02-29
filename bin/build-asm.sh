#!/bin/bash
BASE=`basename $3 .s`
as -o $BASE.o $3 && \
    ld $BASE.o -o $2 && \
    rm -f $BASE.o
