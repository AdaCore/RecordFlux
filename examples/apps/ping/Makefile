specs = $(wildcard specs/*.rflx)
src = generated/rflx.ads
bin = obj/ping
generate = rflx generate -d generated --ignore-unsupported-checksum $(specs)
build = gprbuild -Pping

.PHONY: test test_python test_spark build prove generate clean clean_proof

test: test_python test_spark

test_python:
	sudo -n timeout 3 `which python3` ping.py 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

test_spark: $(bin)
	sudo -n timeout 3 obj/ping 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

build: $(src)
	$(build)

prove: $(src)
	gnatprove -Pping

generate:
	$(generate)

clean:
	rm -rf generated/* obj python

clean_proof:
	rm -rf proof/*

$(src): specs
	$(generate)

$(bin): $(src)
	$(build)
