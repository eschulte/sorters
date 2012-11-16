all: asms opts

.PHONY: clean asms opts

asms:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

opts:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);

clean:
	$(MAKE) -C sorters/ $(MAKECMDGOALS);
