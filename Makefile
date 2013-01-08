CC=gcc
DC=data-wrapper

NEUT_COLUMNS= algorithm language compiler flag neutral

all: asms opts bin/limit

.PHONY: clean real-clean asms opts

bin/limit: bin/limit.c
	$(CC) -o $@ $<

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
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

real-clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

real-real-clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);
