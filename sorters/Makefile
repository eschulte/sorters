CC=gcc
CP=g++
HC=ghc
OC=ocamlopt

ALGORITHMS= bubble insertion merge quick
LANGUAGES= c cpp hs ml
BASIC_EXES=$(foreach lang,$(LANGUAGES), $(ALGORITHMS:=_$(lang)))

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
EXES = $(OPTS:.s=)
TESTS = $(OPTS:=-test)

all: $(ASM)

.PHONY: clean real-clean opts exes neut _%-test test

opts: $(OPTS)
exes: $(EXES)
# TODO: test: $(TESTS) <- make this work

neuts: opts
	../bin/neutrality $^

echo:
	echo $(OPTS)

stores: $(OPTS:.s=.store)

%.s: %.c
	$(CC) -o $@ -S $<

%.s: %.cpp
	$(CP) -o $@ -S $<

%.s: %.hs
	$(HC) -o $@ -S $<

%.s: %.ml
	$(OC) -dstartup -S $<

_%.s:
	@../bin/omni-compile $@

_%: _%.s
	@../bin/omni-link $<

_%-test: _%.s
	@if [ ! -f _$* ];then ../bin/omni-link $< >/dev/null 2>/dev/null; fi;
	@echo "$$(../bin/test.sh _$*) _$*";

%.store: %.s
	../bin/neutrality $<

clean:
	rm -f $(ASM) $(OPTS) $(EXES) $(BASIC_EXES) *.cm* *.o *.hi merge_file_c

real-clean: clean
	rm -f $(OPTS:.s=.store)

real-real-clean: real-clean
	rm -f seed

#########################################################################
# Build unreliable versions of executables, the `ur' utility is
# available from https://github.com/eschulte/unreliable-asm.
UR=ur
UNRELIABILITY:=0.1
URS  = $(OPTS:.s=_ur_01.s)
URS += $(OPTS:.s=_ur_05.s)
URS += $(OPTS:.s=_ur_10.s)
URS += $(OPTS:.s=_ur_50.s)
UR_TESTS = $(URS:=-test)

%_ur.s: %.s
	$(UR) -r $(UNRELIABILITY) $< > $@

%_ur_01.s: %.s
	$(UR) -r 0.01 $< > $@

%_ur_05.s: %.s
	$(UR) -r 0.05 $< > $@

%_ur_10.s: %.s
	$(UR) -r 0.10 $< > $@

%_ur_25.s: %.s
	$(UR) -r 0.25 $< > $@

%_ur_50.s: %.s
	$(UR) -r 0.5 $< > $@

# Run the test 100 times, saving the raw results.
%_ur.test: %_ur
	@echo "$$(../bin/test.sh $*) _$*";

unreliable: $(UR_TESTS)
