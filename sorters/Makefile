CC=gcc
CP=g++
HC=ghc
OC=ocamlopt

ALGORITHMS= \
	bubble \
	insertion \
	merge \
	quick

ASM  = $(ALGORITHMS:=_c.s)
ASM += $(ALGORITHMS:=_cpp.s)
ASM += $(ALGORITHMS:=_hs.s)
ASM += $(ALGORITHMS:=_ml.s)

GCC_OPTS   = O0 O1 O2 O3 Os Ofast
CLANG_OPTS = O0 O1 O2 Os Oz O3 O4
GHC_OPTS   = O0 O1 O2 O3 O4
ML_OPTS    = compact nodynlink

C_GCC       = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_c_gcc$(opt).s))
C_CLANG     = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_c_clang$(opt).s))
CPP_GCC     = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_cpp_g++$(opt).s))
CPP_CLANG   = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_cpp_clang++$(opt).s))
HS_GHC      = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_hs_ghc$(opt).s))
ML_OCAMLOPT = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_ml_ocamlopt$(opt).s))

OPTS  = $(C_GCC)
OPTS += $(C_CLANG)
OPTS += $(CPP_GCC)
OPTS += $(CPP_CLANG)
OPTS += $(HS_GHC)
OPTS += $(ML_OCAMLOPT)

all: $(ASM) bin/limit
opts:
	echo $(OPTS)
.PHONY: clean opts

%.s: %.c
	$(CC) -o $@ -S $<

%.s: %.cpp
	$(CP) -o $@ -S $<

%.s: %.hs
	$(HC) -o $@ -S $<

%.s: %.ml
	$(OC) -dstartup -S $<

%.s:
	./bin/omni-compiler $@

bin/limit: bin/limit.c
	$(CC) -o $@ $<

clean:
	rm -f $(ASM) bin/limit sorters/*.cm* sorters/*.o sorters/*.hi
