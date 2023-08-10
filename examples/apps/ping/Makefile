include ../../../Makefile.common

specs = $(wildcard specs/*.rflx)
src = generated/rflx.ads
bin = obj/ping

.PHONY: test test_python test_spark build prove generate clean

test: test_python test_spark

test_python:
	sudo -n timeout 5 `command -v python3` ping.py 127.0.0.1 | sed '/64 bytes from 127\.0\.0\.1: icmp_seq=0/q; $$q1'

test_spark: $(bin)
	sudo -n timeout 3 obj/ping 127.0.0.1 | sed '/64 bytes from 127\.0\.0\.1: icmp_seq=0/q; $$q1'

build: $(bin)

prove: $(GNATPROVE_CACHE_DIR) $(src)
	$(GNATPROVE) -Pping

generate: $(src)

clean:
	rm -rf generated/* obj python

$(src): $(specs)
	rflx generate -d generated --ignore-unsupported-checksum $^

$(bin): $(src)
	gprbuild -Pping -Xgnat=$(GNAT)
