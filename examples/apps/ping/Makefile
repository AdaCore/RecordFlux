include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
GENERATED = generated/rflx.ads
BIN = obj/ping
CWD := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))

.PHONY: test test_python test_spark build prove generate clean

test: test_python test_spark

test_python:
	../../../devutils/linux/run $(CWD) timeout 120 tests/python_test.sh $$(dirname $$(command -v python3))

test_spark: $(BIN)
	../../../devutils/linux/run $(CWD) timeout 60 tests/spark_test.sh

build: $(BIN)

prove: $(GNATPROVE_CACHE_DIR) $(GENERATED)
	$(GNATPROVE) -Pping

generate: $(GENERATED)

clean:
	$(RM) -r generated/* obj/* python

$(GENERATED): $(SPECS) | generated
	rflx generate -d generated --ignore-unsupported-checksum $^

generated:
	mkdir -p generated

$(BIN): $(GENERATED) $(wildcard generated/*)
	gprbuild -Pping -Xgnat=$(GNAT)
