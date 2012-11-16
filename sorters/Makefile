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

C_GCC       = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_c_gcc_$(opt).s))
C_CLANG     = $(foreach opt,$(CLANG_OPTS), $(ALGORITHMS:=_c_clang_$(opt).s))
CPP_GCC     = $(foreach opt,$(GCC_OPTS), $(ALGORITHMS:=_cpp_g++_$(opt).s))
CPP_CLANG   = $(foreach opt,$(CLANG_OPTS), $(ALGORITHMS:=_cpp_clang++_$(opt).s))
HS_GHC      = $(foreach opt,$(GHC_OPTS), $(ALGORITHMS:=_hs_ghc_$(opt).s))
ML_OCAMLOPT = $(foreach opt,$(ML_OPTS), $(ALGORITHMS:=_ml_ocamlopt_$(opt).s))
TMP  = $(C_GCC) $(C_CLANG) $(CPP_GCC) $(CPP_CLANG) $(HS_GHC) $(ML_OCAMLOPT)
OPTS = $(addprefix _, $(TMP))

all: $(ASM) bin/limit

.PHONY: clean opts

opts: $(OPTS)

%.s: %.c
	$(CC) -o $@ -S $<

%.s: %.cpp
	$(CP) -o $@ -S $<

%.s: %.hs
	$(HC) -o $@ -S $<

%.s: %.ml
	$(OC) -dstartup -S $<

_%.s:
	../bin/omni-compile $@

bin/limit: bin/limit.c
	$(CC) -o $@ $<

clean:
	rm -f $(ASM) $(OPTS) bin/limit *.cm* *.o *.hi
