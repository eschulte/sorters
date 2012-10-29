#!/bin/bash
#
# Usage: cat FILE|caps
#
sed 's/ [a-z]/\U&/g'
