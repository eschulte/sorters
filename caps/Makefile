C:= gcc
HC := ghc

all: check

%: %.c
	gcc -Ofast -lpthread -o $@ $<

%: %.hs
	ghc -Ofast -o $@ $<

clean:
	find . -type f -executable|grep -v "\.git"|xargs -I{} rm {}
