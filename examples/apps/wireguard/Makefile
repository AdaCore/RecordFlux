include ../../../Makefile.common

SPECS = $(wildcard specs/*.rflx)
CWD := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
RUST_BINDING = tai64_bindings/target/debug/libtai64_bindings.so

.PHONY: test generate clean

$(RUST_BINDING):
	$(MAKE) -C tai64_bindings build

test: $(RUST_BINDING)
	$(POETRY) run ../../../devutils/linux/run $(CWD) timeout 180 tests/wireguard_test.sh $$(dirname $$(command -v python3))

generate:

clean:
	$(RM) -r build
	$(MAKE) -C tai64_bindings clean
