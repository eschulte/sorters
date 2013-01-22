CC := gcc
HC := ghc
OC := ocamlopt

%: %.c
	${CC} -Ofast -o $@ $<  -lpthread

%: %.hs
	${HC} -O4 -o $@ $<

%.s: %.c
	$(CC) -o $@ -S $<

caps-ml: caps.ml
	${OC} -linkall -nodynlink -o $@ $<

python:
	python setup.py build_ext --inplace

/tmp/test: lorem
	./lorem 2500000 > $@

clean:
	find . -type f -executable|grep -v "\.git"|grep -v "\.sh$$"|xargs -I{} rm {}
