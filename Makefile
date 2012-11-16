CC=gcc

all: asms opts bin/limit

.PHONY: clean asms opts

bin/limit: bin/limit.c
	$(CC) -o $@ $<

asms:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

opts:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);
