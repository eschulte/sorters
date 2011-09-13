#!/bin/sh
base=$(cat <<EOF
# lang:replace-lang prog:replace-prog
--program src/replace-prog-replace-lang.s
--asm-sample-runs 0
--pos-tests 10
--neg-tests 0
replace-cmp
replace-ops

# use statements in coverage.path for pos/neg weights
--fault-scheme line
--fix-scheme line
--fault-file src/replace-prog-replace-lang.path
--fix-file src/replace-prog-replace-lang.path

# for post-processing to remove duplicates of the original
--print-source-name
--keep-source

# for possible use in the collection of coverage information
--uniq
--coverage-info covered-statements

# mutational robustness flags
--search neutral
--neutral 10
--mutrb-runs 250
EOF
)

for program in bubble insertion merge quick;do
    by_prog=$(echo "$base"|sed "s/replace-prog/$program/g")
    for language in c cpp hs ml;do
        by_lang=$(echo "$by_prog"|sed "s/replace-lang/$language/g")
        case $language in
            c)
                cmp="" ;
                ops="--compiler-opts -O2" ;;
            cpp)
                cmp="--compiler g++" ;
                ops="--compiler-opts -O2" ;;
            hs)
                cmp="--compiler ghc" ;
                ops="--compiler-opts -O2" ;;
            ml)
                cmp="--compiler .\/ocamlc.sh" ;
                ops="" ;;
        esac
        echo "$by_lang"|sed "s/replace-cmp/$cmp/;s/replace-ops/$ops/" > $program-$language.conf
    done
done
