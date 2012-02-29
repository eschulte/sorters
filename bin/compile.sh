#!/bin/sh
cd src/

for i in bubble insertion merge quick; do
    gcc -o $i-c.s -S $i.c       # C
    gcc -O2 -o $i-c $i-c.s

    g++ -o $i-cpp.s -S $i.cpp   # CPP
    g++ -O2 -o $i-cpp $i-cpp.s

    ghc -o $i-hs.s -S $i.hs     # Haskell
    ghc -O2 -o $i-hs $i-hs.s

    ocamlopt -dstartup -S $i.ml # OCaml
    mv $i.s $i-ml.s
    mv a.out.startup.s $i-startup.s
    gcc -O2 -o $i-ml '-L/usr/lib/ocaml' $i-startup.s \
        '/usr/lib/ocaml/std_exit.o' $i-ml.s \
        '/usr/lib/ocaml/stdlib.a' '/usr/lib/ocaml/libasmrun.a' -lm  -ldl 
done

rm a.out *.cmx *.cmi *.o *.hi
