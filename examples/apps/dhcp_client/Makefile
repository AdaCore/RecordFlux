specs = $(wildcard specs/*.rflx)
src = generated/rflx-dhcp_client.ads
bin = obj/dhcp_client
bin-optimized = obj_optimized/dhcp_client

.PHONY: test build prove generate clean binary_size

test: $(bin)
	sudo tests/run

build: $(bin)

prove: $(src)
	gnatprove -Pdhcp_client

generate: $(src)

clean:
	rm -rf generated/* obj obj_optimized

binary_size: $(bin-optimized)
	test $(shell size -A -d obj_optimized/dhcp_client | grep .text | awk '{ print $$2 }') -le 175000

$(src): $(specs)
	rflx generate -d generated --debug built-in $^

$(bin): $(src) $(wildcard src/*) $(wildcard generated/*)
	gprbuild -Pdhcp_client -Xgnat=$(GNAT) -Xmode=asserts_enabled

$(bin-optimized): $(src) $(wildcard src/*) $(wildcard generated/*)
	gprbuild -Pdhcp_client -Xgnat=$(GNAT) -Xmode=optimized
