specs = $(wildcard specs/*.rflx)
src = generated/rflx.ads
bin = obj/ping

.PHONY: test test_python test_spark build prove generate clean

test: test_python test_spark

test_python:
	sudo -n timeout 5 `command -v python3` ping.py 127.0.0.1 | tee /dev/tty | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

test_spark: $(bin)
	sudo -n timeout 3 obj/ping 127.0.0.1 | tee /dev/tty | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

build: $(bin)

prove: $(src)
	gnatprove -Pping --memcached-server=localhost:11211

generate: $(src)

clean:
	rm -rf generated/* obj python

$(src): $(specs)
	rflx generate -d generated --ignore-unsupported-checksum $^

$(bin): $(src)
	gprbuild -Pping -Xgnat=$(GNAT)
