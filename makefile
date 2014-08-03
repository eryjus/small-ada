
TARGET=small-ada

FLEX=flex -d
CC=gcc
TREECC=ast-cc
YACC=bison --report=all --report-file=parser.log

FFLAGS=
YFLAGS=-t
CFLAGS=-Iinclude
TFLAGS=

LSRC=$(wildcard source/*.l)
LCSRC=$(subst .l,.cc,$(LSRC))

TSRC=$(wildcard source/*.ast)
THDR=$(subst .ast,.h,$(subst source,include,$(TSRC)))

YSRC=$(wildcard source/*.y)
YCSRC=$(subst .y,.cc,$(YSRC))
YHDR=$(subst .cc,.h,$(subst source,include,$(YCSRC)))

HSRC=$(sort $(wildcard include/*.h) $(THDR) $(YHDR))
CSRC=$(sort $(wildcard source/*.cc) $(LCSRC) $(YCSRC) $(TCSRC))
OBJ=$(subst source,obj,$(subst .cc,.o,$(CSRC)))
TGT=bin/$(TARGET)

.PHONY: Debug
Debug: all

.PHONY: all
all: preamble $(TGT)

.PHONY: preamble
preamble:
	@mkdir -p obj
	@mkdir -p bin

$(TGT): $(OBJ) makefile
	@echo "LINK : $@"
	@$(CC) $(CFLAGS) -o$@ $(OBJ) -lstdc++

$(OBJ): $(CSRC) $(HSRC) makefile
	@echo "GCC  : $(subst .o,.cc,$(subst obj/,source/,$@))"
	@$(CC) $(CFLAGS) -c -o$@ $(subst .o,.cc,$(subst obj/,source/,$@))

$(LCSRC): $(LSRC) $(YHDR) $(HSRC) makefile
	@echo "FLEX : $<"
	@$(FLEX) $(FFLAGS) -o$(LCSRC) $(LSRC)

$(YCSRC) $(YHDR): $(YSRC) $(THDR) makefile
	@echo "YACC : $<"
	@$(YACC) $(YFLAGS) --defines=$(YHDR) -o$(YCSRC) $(YSRC)

$(THDR): $(TSRC) makefile
	@echo "AST  : $<"
	@$(TREECC) $(TFLAGS) $(TSRC) > /dev/null
	@mv ast-nodes.h $(THDR)

.PHONY: clean
clean:
	rm -f $(LCSRC)
	rm -f $(YCSRC)
	rm -f $(YHDR)
	rm -f $(THDR)
	rm -Rf obj
	rm -Rf bin
	rm -f source/*.output
