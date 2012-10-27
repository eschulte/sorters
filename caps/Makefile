CC := gcc
HC := ghc

all: check

%: %.c
	gcc -Ofast -o $@ $<

%: %.hs
	ghc -Ofast -o $@ $<
