include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
GENERATED = build/generated/rflx.ads
BIN = build/obj/ping
CWD := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))

.PHONY: test test_python test_spark build prove generate clean

test: test_python test_spark

test_python:
	../../../devutils/linux/run $(CWD) timeout 60 tests/python_test.sh $$(dirname $$(command -v python3))

test_spark: $(BIN)
	../../../devutils/linux/run $(CWD) timeout 60 tests/spark_test.sh

build: $(BIN)

prove: $(GNATPROVE_CACHE_DIR) $(GENERATED)
	$(GNATPROVE) -Pping

generate: $(GENERATED)

clean:
	rm -rf build python

$(GENERATED): $(SPECS) | build/generated
	rflx generate -d build/generated --ignore-unsupported-checksum $^

build/generated:
	mkdir -p build/generated

$(BIN): $(GENERATED) $(wildcard build/generated/*)
	gprbuild -Pping -Xgnat=$(GNAT)
