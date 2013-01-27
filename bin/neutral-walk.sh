#!/bin/bash
#
# Usage: ./bin/neutral-walk.sh [SORTER] [OPTIONS...]
# Generate some number of neutral variants of SORTER using either
# clang-mutate or cil-mutate.
#
# Options:
#   -e --engine [clang|cil] ..... use the (clang|cil)-mutate executable
#   -n --number [#] ............. the number of variants to generate
#   -r --result-dir [dir] ....... specify where to save the results
#   -t --test [test|test-file] .. test script to use
#   -s --steps [#] .............. number of steps to take
#   
HOME=$(pwd)
SORTER="$1"; shift;
if [ ! -f $SORTER ];then
    echo "$0: $SORTER does not exist"; exit 1
fi
STEPS=32

# Parse Options
ENGINE=cil-mutate
NUMBER=10
DIR=variants
TEST_SCRIPT="test.sh"
eval set -- $(getopt -o e:n:r:t:s: -l engine:,number:,result-dir:,test:,steps: -- "$@" || exit 1)
while [ $# -gt 0 ];do
    case $1 in
        -e|--engine)
            ENGINE="$2"; shift;
            if ! which $ENGINE >/dev/null;then
                echo "$0: engine $ENGINE not installed"; exit 1
            fi;;
        -n|--number) NUMBER="$2"; shift;;
        -s|--steps) STEPS="$2"; shift;;
        -r|--result-dir) DIR="$2"; shift;;
        -t|--test)
            case $2 in
                test) TEST_SCRIPT="test.sh";;
                test-file) TEST_SCRIPT="test-file.sh";;
                (*) echo "$0: bad test file '$2'"; exit 1;;
            esac; shift;;
        (--) shift; break;;
        (-*) echo "unrecognized option $1">&2; exit 1;;
        (*)  break;;
    esac
    shift
done
if [ -d "$HOME/variants" ];then
    echo "./variants already exists"; exit 1
fi

# Random element of directory
random_from(){
    for i in $(ls $1);do echo "$RANDOM $i"; done|sort|head -1|awk '{print $2}'
}

# Engines
cil(){
    (
        case $1 in
            ids) cil-mutate -ids $2;;
            del) cil-mutate -delete -stmt1 $2 $3 > $4;;
            ins) cil-mutate -insert -stmt1 $2 -stmt2 $3 $4 > $5;;
            swp) cil-mutate -swap -stmt1 $2 -stmt2 $3 $4 > $5;;
        esac
    ) 2>/dev/null; }
clang(){ echo "$0: engine 'clang' is not supported"; }
run(){
    case $ENGINE in
        cil-mutate) cil $@;;
        clang-mutate) clang $@;;
    esac; }

# Testing neutrality
fitness(){
    local SRC="$1";
    local EXE=$(basename $1 ".$EXT");
    make $EXE >/dev/null
    FITNESS=$(./limit ./$TEST_SCRIPT $EXE)
    RES=$?
    rm $EXE
    if [ $RES -eq 0 ]; then echo $FITNESS;else echo 0;fi; }

# Make and populate the temp directory
TMPDIR=$(mktemp -d)
function exit_hook (){ rm -rf $TMPDIR; exit 0; }
trap exit_hook EXIT
cp sorters/Makefile $TMPDIR
cp sorters/sorters.h /tmp/
make bin/limit
cp bin/limit $TMPDIR
cp bin/$TEST_SCRIPT $TMPDIR
mkdir -p $TMPDIR/tmp/0/neut
cat $SORTER|sed 's/sorters.h/\/tmp\/sorters.h/' > $TMPDIR/tmp/0/neut/$(basename $SORTER)
pushd $TMPDIR>/dev/null

# Generate the neutral variants
for ((s=1;s<=$STEPS;s++));do
    echo -n "starting step $s "
    mkdir -p tmp/$s/neut
    COUNTER=0
    for ((;;));do
        # randomly pick an individual
        SORTER="tmp/$(($s - 1))/neut/$(random_from tmp/$(($s - 1))/neut)"
        EXT=$(echo "$SORTER"|sed 's/^.*\.//')
        NAME=$(basename "$SORTER" ".$EXT")
        case $ENGINE in
            # cil requires pre-processing of macros from C files
            cil-mutate) cpp $SORTER > $TMPDIR/$NAME.$EXT;;
            clang-mutate) cp $SORTER $TMPDIR;;
        esac
        SORTER=$(basename $SORTER)
        IDS=$(run ids $SORTER)
        # randomly pick an operation
        case $(($RANDOM % 3)) in
            0)  # delete
                ID=$(($RANDOM % $IDS))
                OUTPUT="${NAME}_del_${ID}.${EXT}"
                $(run del $ID $SORTER $OUTPUT);;
            1)  # insert
                ID1=$(($RANDOM % $IDS))
                ID2=$(($RANDOM % $IDS))
                OUTPUT="${NAME}_ins_${ID1}_${ID2}.${EXT}"
                $(run ins $ID1 $ID2 $SORTER $OUTPUT);;
            2)  # swap
                ID1=$(($RANDOM % $IDS))
                ID2=$(($RANDOM % $IDS))
                OUTPUT="${NAME}_swp_${ID1}_${ID2}.${EXT}"
                $(run swp $ID1 $ID2 $SORTER $OUTPUT);;
        esac
        FIT=$(fitness $OUTPUT 2>/dev/null)
        echo "$FIT $OUTPUT" >> $TMPDIR/tmp/fitness
        if [ $FIT -eq 10 ];then
            echo -n "."
            COUNTER=$(($COUNTER + 1))
            if [ $COUNTER -eq $NUMBER ];then
                echo ""
                break;
            fi
            mv $OUTPUT tmp/$s/neut
        else
            mv $OUTPUT tmp/$s
        fi
        rm $SORTER
    done
done
popd>/dev/null
mv $TMPDIR/tmp $DIR
cat <<EOF
DONE
$NUMBER variants saved to $DIR
fitness for each variant recorded in $DIR/fitness
EOF
