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
	@rm -f $(ASM) $(OPTS) $(EXES) $(BASIC_EXES) *.cm* *.o *.hi merge_file_c *_ur*

real-clean: clean
	@rm -f $(OPTS:.s=.store)

real-real-clean: real-clean
	@rm -f seed

#########################################################################
# Build unreliable versions of executables, the `ur' utility is
# available from https://github.com/eschulte/unreliable-asm.
UR=ur
UNRELIABILITY:=0.1
UR_C_GCC = $(addprefix _, $(C_GCC))
UR_GRADATIONS=0 0.001 0.0025 0.005 0.0075 0.01 0.025 0.05 0.075 0.1 0.25 0.5 0.75 1
UR_SRCS  = $(foreach num,$(UR_GRADATIONS),$(UR_C_GCC:.s=_ur_$(num).s))
UR_EXES  = $(UR_SRCS:.s=)
UR_TESTS = $(UR_EXES:=.test)

%_ur.s: %.s
	$(UR) -r $(UNRELIABILITY) $< > $@

%_ur_0.s: %.s
	$(UR) -r 0 $< > $@
%_ur_0.001.s: %.s
	$(UR) -r 0.001 $< > $@
%_ur_0.0025.s: %.s
	$(UR) -r 0.0025 $< > $@
%_ur_0.005.s: %.s
	$(UR) -r 0.005 $< > $@
%_ur_0.0075.s: %.s
	$(UR) -r 0.0075 $< > $@
%_ur_0.01.s: %.s
	$(UR) -r 0.01 $< > $@
%_ur_0.025.s: %.s
	$(UR) -r 0.025 $< > $@
%_ur_0.05.s: %.s
	$(UR) -r 0.05 $< > $@
%_ur_0.075.s: %.s
	$(UR) -r 0.075 $< > $@
%_ur_0.1.s: %.s
	$(UR) -r 0.1 $< > $@
%_ur_0.25.s: %.s
	$(UR) -r 0.25 $< > $@
%_ur_0.5.s: %.s
	$(UR) -r 0.5 $< > $@
%_ur_0.75.s: %.s
	$(UR) -r 0.75 $< > $@
%_ur_1.s: %.s
	$(UR) -r 1 $< > $@

%_ur_01.test: %_ur
	for i in $$(seq 100);do ../bin/test.sh -i $<; done > $@;
%_ur_25.test: %_ur
	for i in $$(seq 100);do ../bin/test.sh -i $<; done > $@;
%_ur_05.test: %_ur
	for i in $$(seq 100);do ../bin/test.sh -i $<; done > $@;
%_ur_10.test: %_ur
	for i in $$(seq 100);do ../bin/test.sh -i $<; done > $@;
%_ur_50.test: %_ur
	for i in $$(seq 100);do ../bin/test.sh -i $<; done > $@;

unreliable-src: $(UR_SRCS)

unreliable-exe: $(UR_EXES)

unreliable-test: $(UR_TESTS)
