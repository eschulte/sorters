#!/bin/bash
#
# Usage: test.sh EXECUTABLE [OPTIONS...]
#  test and maybe profile a sorting executable
#
# OPTIONS:
#   -t,--test --- the specific test to run (otherwise print # passed)
#   -p,--perf --- return profiling information
#   -e,--events - list of perf events
#
BASE="$(dirname $0)"
TEST=""
PERF=""
EVENTS="cycles,instructions,cache-references,page-faults,branches,branch-misses,task-clock"
PERF_FILE=$(mktemp)
if [ -z "$LIMIT" ];then LIMIT="${BASE}/limit"; fi
declare -a INPUTS
declare -a OUTPUTS
HELP_TEXT=$(cat "$0" \
        |sed '/^[^#]/q' \
        |head -n -1 \
        |tail -n +3 \
        |sed -e :a -e '/^\n*$/{$d;N;ba' -e '}' \
        |cut -c3-)
if [ $(grep "\-h" <(echo "$1")) ];then echo "$HELP_TEXT"; exit 0; fi

eval set -- $(getopt -o ht:p -l help,test:,perf -- "$@" \
    || echo "$HELP_TEXT" && exit 1;)

while [ $# -gt 0 ];do
    case $1 in
        -h|--help) echo "$HELP_TEXT" && exit 0;;
        -t|--test) TEST="$2"; shift;;
        -p|--perf) PERF="yes";;
        (--) shift; break;;
        (-*) error "unrecognized option $1";;
        (*)  break;;
    esac
    shift
done
PROG="$(dirname $1)/$(basename $1)"; shift;

run_prog(){
    if [ -z "$PERF" ];then
        $LIMIT $PROG $1
    else
        $LIMIT perf stat -x, -e "$EVENTS" --append -o $PERF_FILE $PROG $1
    fi; }

num_diff(){ # difference between two lists
    diff -wB <(run_prog "$1"|tr ' ' '\n') <(echo "$2"|tr ' ' '\n')|wc -l; }

run(){
    if [ $1 -gt 9 ];then
        num_diff "$(cat "$BASE/$1.in")" "$(cat "$BASE/$1.out")"
    else
        num_diff "$(echo "${INPUTS[$1]}")" "$(echo "${OUTPUTS[$1]}")"
    fi; }

INPUTS[0]="1 4 56 2 43 8 76 12 43 7"
INPUTS[1]="87686876 9879789 4756456 4534657 45354 9878 123 2 1"
INPUTS[2]="94 38 42 30 19 90 42"
INPUTS[3]="4844 8783"
INPUTS[4]="1"
INPUTS[5]="0"
INPUTS[6]="7 6 4 9 7 9 2 9 6 3"
INPUTS[7]="804 683 473 892 689 422 365 896 871 384 80 101 817 419 460 419 837 627 681 92 566 935 69 768 727 442 252 878 948 987 158 973 977 461 747 715 108 658 185 908 2 792 376 277 383 402 356 724 287 20 112 424 624 888 791 447 831 961 94 540 298 655 908 350 75 90 864 627 82 82 554 891 674 759 609 630 965 469 707 346 727 629 829 689 70 219 788 425 318 935 113 280 700 411"
INPUTS[8]="50384 49152 94598 45069 34709 28075 41004 14862 8268 58155 72379 60325 63673 36390 11362 43942 78637 12491 40124 54376"
INPUTS[9]="0 0 0 1 1 1 0 1 1 1"

OUTPUTS[0]="1 2 4 7 8 12 43 43 56 76"
OUTPUTS[1]="1 2 123 9878 45354 4534657 4756456 9879789 87686876"
OUTPUTS[2]="19 30 38 42 42 90 94"
OUTPUTS[3]="4844 8783"
OUTPUTS[4]="1"
OUTPUTS[5]="0"
OUTPUTS[6]="2 3 4 6 6 7 7 9 9 9"
OUTPUTS[7]="2 20 69 70 75 80 82 82 90 92 94 101 108 112 113 158 185 219 252 277 280 287 298 318 346 350 356 365 376 383 384 402 411 419 419 422 424 425 442 447 460 461 469 473 540 554 566 609 624 627 627 629 630 655 658 674 681 683 689 689 700 707 715 724 727 727 747 759 768 788 791 792 804 817 829 831 837 864 871 878 888 891 892 896 908 908 935 935 948 961 965 973 977 987"
OUTPUTS[8]="8268 11362 12491 14862 28075 34709 36390 40124 41004 43942 45069 49152 50384 54376 58155 60325 63673 72379 78637 94598"
OUTPUTS[9]="0 0 0 0 1 1 1 1 1 1"

if [ -z "$TEST" ];then
    passed=0
    for t in {0..9};do
        ERR=$(run $t)
        if [ $ERR -eq 0 ];then
            passed=$(($passed + 1))
        fi
    done
    if [ -z "$PERF" ];then
        echo $passed
    else
        echo "$passed,passed"
        cat $PERF_FILE|tail -n +3
    fi
else
    ERR=$(run $TEST)
    if [ -z "$PERF" ];then
        echo $ERR
    else
        echo "$ERR,error"
        cat $PERF_FILE|tail -n +3
    fi
fi
rm -f $PERF_FILE
