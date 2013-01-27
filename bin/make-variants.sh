#!/bin/bash
#
# Usage: ./bin/make-variants.sh [SORTER] [OPTIONS...]
# Generate some number of neutral variants of SORTER using either
# clang-mutate or cil-mutate.
#
# Options:
#   -e --engine [clang|cil] ..... use the (clang|cil)-mutate executable
#   -n --number [#] ............. the number of variants to generate
#   -r --result-dir [dir] ....... specify where to save the results
#   -t --test [test|test-file] .. test script to use
#   
HOME=$(pwd)
SORTER="$1"; shift;
if [ ! -f $SORTER ];then
    echo "$0: $SORTER does not exist"; exit 1
fi
EXT=$(echo "$SORTER"|sed 's/^.*\.//')
NAME=$(basename "$SORTER" ".$EXT")

# Parse Options
ENGINE=cil-mutate
NUMBER=100
DIR=variants
TEST_SCRIPT="test.sh"
eval set -- $(getopt -o e:n:r:t: -l engine:,number:,result-dir:,test: -- "$@")
while [ $# -gt 0 ];do
    case $1 in
        -e|--engine)
            ENGINE="$2"; shift;
            if ! which $ENGINE >/dev/null;then
                echo "$0: engine $ENGINE not installed"; exit 1
            fi;;
        -n|--number) NUMBER="$2"; shift;;
        -r|--result-dir) DIR="$2"; shift;;
        -t|--test)
            case $2 in
                test) TEST_SCRIPT="test.sh";;
                test-file) TEST_SCRIPT="test-file.sh";;
                (*) echo "$0: bad test file '$2'"; exit 1;;
            esac; shift;;
        (--) shift; break;;
        (-*) error "unrecognized option $1";;
        (*)  break;;
    esac
    shift
done
if [ -d "$HOME/variants" ];then
    echo "./variants already exists"; exit 1
fi

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
    if [ $? -eq 0 ]; then echo $FITNESS;else echo 0;fi; }

# Make and populate the temp directory
TMPDIR=$(mktemp -d)
function exit_hook (){ rm -rf $TMPDIR; exit 0; }
trap exit_hook EXIT
case $ENGINE in
    # cil requires pre-processing of macros from C files
    cil-mutate) cpp $SORTER > $TMPDIR/$NAME.$EXT;;
    clang-mutate) cp $SORTER $TMPDIR;;
esac
cp sorters/Makefile $TMPDIR
make bin/limit
cp bin/limit $TMPDIR
cp bin/$TEST_SCRIPT $TMPDIR
pushd $TMPDIR>/dev/null
SORTER=$(basename $SORTER)

# Generate the neutral variants
mkdir tmp
IDS=$(run ids $SORTER)
for ((i=0;i<$NUMBER;i++));do
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
    printf "%2d %s\n" $(fitness $OUTPUT 2>/dev/null) $OUTPUT \
        |tee -a tmp/fitness
    mv $OUTPUT tmp/
done
popd>/dev/null
mv $TMPDIR/tmp $DIR
cat <<EOF
DONE
$NUMBER variants saved to $DIR
fitness for each variant recorded in $DIR/fitness
EOF
