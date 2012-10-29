CC:= gcc
HC := ghc

all: check

%: %.c
	${CC} -Ofast -o $@ $<  -lpthread

%: %.hs
	${HC} -Ofast -o $@ $<

python:
	python setup.py build_ext --inplace

clean:
	find . -type f -executable|grep -v "\.git"|xargs -I{} rm {}
