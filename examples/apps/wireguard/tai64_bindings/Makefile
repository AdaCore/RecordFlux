include ../../../../Makefile.common

RUST_BINDING = target/debug/libtai64_bindings.so

.PHONY: clean build

build: $(RUST_BINDING)

$(RUST_BINDING): $(wildcard src/*)
	maturin develop

clean:
	cargo clean
