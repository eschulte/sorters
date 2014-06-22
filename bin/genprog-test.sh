#!/bin/bash
#
# Usage: genprog-test.sh EXECUTABLE TESTNAME
#  run test.sh in the way genprog expects
#
BASE="$(dirname $0)"
TEST="$BASE/test.sh"
HELP_TEXT=$(cat "$0" \
        |sed '/^[^#]/q' \
        |head -n -1 \
        |tail -n +3 \
        |sed -e :a -e '/^\n*$/{$d;N;ba' -e '}' \
        |cut -c3-)
if [ $(grep "\-h" <(echo "$1")) ];then echo "$HELP_TEXT"; exit 0; fi

EXE=$1
NUM=$(($(echo $2|cut -c2-) - 1))

if [ $($TEST $EXE -t $NUM) -eq 0 ];then
    echo "PASS $NUM"
    exit 0
else
    echo "FAIL $NUM"
    exit 1
fi
