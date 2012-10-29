C:= gcc
HC := ghc

all: check

%: %.c
	${C} -Ofast -lpthread -o $@ $<

%: %.hs
	${HC} -Ofast -o $@ $<

python:
	python setup.py build_ext --inplace

clean:
	find . -type f -executable|grep -v "\.git"|xargs -I{} rm {}
