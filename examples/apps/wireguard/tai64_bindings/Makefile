include ../../../../Makefile.common

RUST_BINDING = target/debug/libtai64_bindings.so

.PHONY: clean build maturin

maturin:
	if ! which maturin &> /dev/null; then\
		cargo install maturin;\
	fi

build: maturin $(RUST_BINDING)

$(RUST_BINDING): $(wildcard src/*)
	maturin develop

clean:
	cargo clean
