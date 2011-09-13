#!/bin/sh
rm -rf *sanity* *debug* 00* *.map *.out
rm -f *cache *.ast *.instr-sizes *.ast
rm -f *-coverage.c *-baseline.c coverage.o
rm -f coverage coverage.path.neg coverage.path.pos coverage.s
rm -rf coverage.pos.d coverage.pos coverage.neg.d coverage.neg
rm -f repair.*
rm -f coverage.
rm -f internal/*cache source/*cache
