CC=gcc
DC=./bin/data-viewer

NEUT_COLUMNS= algorithm language compiler flag result

all: asms opts bin/limit

.PHONY: clean real-clean asms opts

bin/limit: bin/limit.c
	$(CC) -o $@ $<

results/neut-viewer: results/neut $(DC)
	$(DC) $< $(NEUT_COLUMNS)

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
