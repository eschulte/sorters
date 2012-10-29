C:= gcc
HC := ghc

all: check

%: %.c
	${C} -Ofast -lpthread -o $@ $<

%: %.hs
	${HC} -Ofast -o $@ $<

clean:
	find . -type f -executable|grep -v "\.git"|xargs -I{} rm {}
