SHELL := /bin/bash

GENERATED = build/generated/rflx-spdm.ads

SOURCE_SPECIFICATIONS = \
	specs/spdm_responder.rflx \
	specs/spdm.rflx

MISSING_SPECIFICATIONS = $(filter-out $(SOURCE_SPECIFICATIONS),$(wildcard specs/*.rflx))
ifneq ($(MISSING_SPECIFICATIONS),)
$(error Unhandled specifications: $(MISSING_SPECIFICATIONS))
endif

SPECIFICATIONS = $(addprefix specs/, $(foreach spec, $(SOURCE_SPECIFICATIONS), $(notdir $(spec))))

INTEGRATION_FILES = \
	specs/spdm_responder.rfi

RFLX ?= $(shell command -v rflx)

.PHONY: all

all: check test prove

.PHONY: check check_spec

check: check_spec

check_spec: $(SPECIFICATIONS) | $(INTEGRATION_FILES) $(RFLX)
	timeout -k 60 900 $(RFLX) check $^

.PHONY: test test_cross test_binary_size

test: test_cross test_binary_size lib

test_cross: build/arm/example/main build/riscv64/example/main libarm libriscv64

test_binary_size: build/arm/example/main build/riscv64/example/main
	tools/check_size.py arm build/arm/example/main .text 46000
	tools/check_size.py riscv64 build/riscv64/example/main .text 38000

build/lib/libspdm.a: $(GENERATED)
	gprbuild -j0 -P spdm

build/%/lib/libspdm.a: $(GENERATED)
	gprbuild -j0 -P spdm -XTARGET=$*

build/%/example/main: $(GENERATED)
	gprbuild -j0 -P build.gpr -XTARGET=$*
	test -f $@

build/generated:
	mkdir -p build/generated

# The verification of the SPDM specification occasionally gets stuck in the CI. The code generation
# is attempted twice to decrease the occurrence of unsuccessful pipelines.
build/generated/%: $(SPECIFICATIONS) | build/generated $(INTEGRATION_FILES) $(RFLX)
	timeout -k 60 1200 $(RFLX) generate $^ -d build/generated; \
	RC=$$?; \
	if [ $$RC -eq 124 ]; \
	then \
		timeout -k 60 1200 $(RFLX) generate $^ -d build/generated; \
	else \
		(exit $$RC); \
	fi

.PHONY: lib libarm libriscv64

lib: build/lib/libspdm.a

libarm: build/arm/lib/libspdm.a

libriscv64: build/riscv64/lib/libspdm.a

.PHONY: prove

prove: $(GENERATED)
	gnatprove -P build_lib.gpr -j0 -XTARGET=riscv64 -u responder
	gnatprove -P build.gpr -j0 -XTARGET=riscv64 -u main

.PHONY: generate

generate: $(GENERATED)

.PHONY: clean

clean:
	rm -rf build
