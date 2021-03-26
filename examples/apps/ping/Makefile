test: test_python test_spark

test_python:
	sudo -n timeout 3 `which python3` ping.py 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

test_spark:
	rflx generate -d generated specs/ipv4.rflx
	gprbuild -Pping
	sudo -n timeout 3 obj/ping 127.0.0.1 | grep -q "64 bytes from 127.0.0.1: icmp_seq=0"

prove:
	rflx generate -d generated specs/ipv4.rflx
	gnatprove -Pping

clean:
	rm -rf generated/* obj python

clean_proof:
	rm -rf proof/*
