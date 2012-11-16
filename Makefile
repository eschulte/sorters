CC=gcc
CXX=g++
HC=ghc
OC=ocamlopt

ALGORITHMS= 		\
	bubble		\
	insertion	\
	merge		\
	quick

C_SRC   = $(addprefix sorters/, $(ALGORITHMS:=-c.c))
CPP_SRC = $(addprefix sorters/, $(ALGORITHMS:=-cpp.cpp))
HS_SRC  = $(addprefix sorters/, $(ALGORITHMS:=-hs.hs))
ML_SRC  = $(addprefix sorters/, $(ALGORITHMS:=-ml.ml))

ASM  = $(C_SRC:.c=.s)
ASM += $(CPP_SRC:.cpp=.s)
ASM += $(HS_SRC:.hs=.s)
# ASM += $(ML_SRC:.ml=.s)

all: $(ASM) bin/limit
.PHONY: clean

%.s: %.c
	$(CC) -S $< -o $@

%.s: %.cpp
	$(CXX) -S $< -o $@

%.s: %.hs
	$(HC) -S $< -o $@

%.s: %.ml
	$(OC) -dstartup -S $<

bin/limit: bin/limit.c
	$(CC) -o $@ $<

clean:
	rm -f $(ASM) bin/limit
