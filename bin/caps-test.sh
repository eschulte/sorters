#!/bin/bash
#
# Run the sorter ($1) on a single LARGE test
# report time and memory
# non-zero exit if an incorrect result it returned
#
TIME=/usr/bin/time
LARGER_LIMIT=$(dirname $0)"/large-limit"
PROG=$1;

diff <($TIME -f "%e %M" -- $LARGE_LIMIT $PROG /tmp/to-cap) \
    /tmp/capped \
    >/dev/null 2>&1

exit $?
