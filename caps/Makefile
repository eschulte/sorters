CC := gcc
HC := ghc

all: check

%: %.c
	gcc -Ofast -lpthread -o $@ $<

%: %.hs
	ghc -Ofast -o $@ $<
