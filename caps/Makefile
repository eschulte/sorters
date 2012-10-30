CC:= gcc
HC := ghc

all: check

%: %.c
	${CC} -Ofast -o $@ $<  -lpthread

%: %.hs
	${HC} -Ofast -o $@ $<

python:
	python setup.py build_ext --inplace

/tmp/test: lorem
	./lorem 2500000 > $@

clean:
	find . -type f -executable|grep -v "\.git"|grep -v "\.sh$$"|xargs -I{} rm {}
