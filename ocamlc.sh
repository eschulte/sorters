#!/bin/sh
# build ocaml assembly files
# ./ocamlc.sh -o exe source.s
startup="src/bubble-startup.s"
exe=$2
echo "gcc -o $exe '-L/usr/lib/ocaml' $startup '/usr/lib/ocaml/std_exit.o' $3 '/usr/lib/ocaml/stdlib.a' '/usr/lib/ocaml/libasmrun.a' -lm  -ldl"
gcc -o $exe '-L/usr/lib/ocaml' $startup '/usr/lib/ocaml/std_exit.o' $3 '/usr/lib/ocaml/stdlib.a' '/usr/lib/ocaml/libasmrun.a' -lm  -ldl
