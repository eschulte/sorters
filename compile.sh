#!/bin/sh
cd src/

for i in bubble insertion merge quick; do
    gcc -o $i-c.s -S $i.c       # C
    g++ -o $i-cpp.s -S $i.cpp   # CPP
    ghc -o $i-hs.s -S $i.hs     # Haskell
    # OCaml
    ocamlopt -dstartup -S $i.ml
    mv $i.s $i-ml.s
    mv a.out.startup.s $i-startup.s
done

rm a.out *.cmx *.cmi *.o *.hi
