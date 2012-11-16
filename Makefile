CC=gcc

bin/limit: bin/limit.c
	$(CC) -o $@ $<
