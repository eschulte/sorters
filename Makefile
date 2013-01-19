CC=gcc
DC=data-wrapper
SORT_SIZE=1000000

NEUT_COLUMNS= algorithm language compiler flag neutral

all: asms opts bin/limit bin/large-limit /tmp/sorted

.PHONY: clean real-clean asms opts

bin/limit: bin/limit.c
	$(CC) -o $@ $<

bin/large-limit: bin/large-limit.c
	$(CC) -o $@ $<

/tmp/to-sort:
	for ((i=0;i<$(SORT_SIZE);i++));do echo $$RANDOM; done > $@

/tmp/sorted: /tmp/to-sort
	cat $<|tr ' ' '\n'|sort -n > $@; \

results/neut-viewer: results/neut
	$(DC) $< $(NEUT_COLUMNS)

results/full-viewer: results/full
	$(DC) $< $(NEUT_COLUMNS) size

sorters/%:
	$(MAKE) -C sorters/ $*;

asms:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

opts:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS); \
	rm -f /tmp/to-sort /tmp/sorted

real-clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

real-real-clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);
