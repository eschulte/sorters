CC=gcc
CXX=g++
HC=ghc
OC=ocamlopt

C_SRC = \
	sorters/bubble-c.c \
	sorters/insertion-c.c \
	sorters/merge-c.c \
	sorters/quick-c.c
CPP_SRC = \
	sorters/bubble-cpp.cpp \
	sorters/insertion-cpp.cpp \
	sorters/merge-cpp.cpp \
	sorters/quick-cpp.cpp
HS_SRC = \
	sorters/bubble-hs.hs \
	sorters/insertion-hs.hs \
	sorters/merge-hs.hs \
	sorters/quick-hs.hs
ML_SRC = \
	sorters/bubble-ml.ml \
	sorters/insertion-ml.ml \
	sorters/merge-ml.ml \
	sorters/quick-ml.ml

ASM += $(C_SRC:.c=.s)
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
	$(CC) -o $<

clean:
	$(ASM) bin/limit
