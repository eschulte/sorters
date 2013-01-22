CC=gcc
DC=data-wrapper
SORT_SIZE=1000000
CAPS_SIZE=100000

NEUT_COLUMNS= algorithm language compiler flag neutral

all: asms opts bin/lorem bin/limit bin/large-limit

.PHONY: clean real-clean asms opts

bin/limit: bin/limit.c
	$(CC) -o $@ $<

bin/large-limit: bin/large-limit.c
	$(CC) -o $@ $<

bin/lorem: bin/lorem.c
	$(CC) -o $@ $<

/tmp/to-sort:
	for ((i=0;i<$(SORT_SIZE);i++));do echo $$RANDOM; done > $@

/tmp/sorted: /tmp/to-sort
	cat $<|tr ' ' '\n'|sort -n > $@; \

/tmp/to-cap: bin/lorem
	$< $(CAPS_SIZE) > $@

/tmp/capped: /tmp/to-cap caps/caps
	caps/caps /tmp/to-cap > $@

results/neut-viewer: results/neut
	$(DC) $< $(NEUT_COLUMNS)

results/full-viewer: results/full
	$(DC) $< $(NEUT_COLUMNS) size

sorters/%:
	$(MAKE) -C sorters/ $*;

caps/%:
	$(MAKE) -C caps/ $*;

asms:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

opts:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS); \
	$(MAKE) -C caps/ $(MAKECMDGOALS); \
	rm -f /tmp/to-sort /tmp/sorted /tmp/to-cap /tmp/capped

real-clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

real-real-clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);
