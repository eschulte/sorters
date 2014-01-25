#!/bin/bash
#
#
#
EXE=$1

declare -a IN
IN+=("eric")
IN+=("the gnu general public license is a free, copyleft license for software and other kinds of works.")
IN+=("the licenses for most software and other practical works are designed to take away your freedom to share and change the works")
IN+=("for example, if you distribute copies of such a program, whether gratis or for a fee, you must pass on to the recipients the same freedoms that you received")

declare -a OUT
OUT+=("Eric")
OUT+=("The Gnu General Public License Is A Free, Copyleft License For Software And Other Kinds Of Works.")
OUT+=("The Licenses For Most Software And Other Practical Works Are Designed To Take Away Your Freedom To Share And Change The Works")
OUT+=("For Example, If You Distribute Copies Of Such A Program, Whether Gratis Or For A Fee, You Must Pass On To The Recipients The Same Freedoms That You Received")

SCORE=0
for i in {0..3};do
    if $(diff -wB <($EXE <(echo "${IN[$i]}")) <(echo "${OUT[$i]}"));then
        SCORE=$(($SCORE + 1))
    fi
done 2>/dev/null
echo $SCORE
