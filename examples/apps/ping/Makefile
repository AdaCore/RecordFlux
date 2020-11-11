test: test_python test_spark

test_python:
	tools/create_privileged_python
	timeout 3 ./ping.py 127.0.0.1 || test $$? -eq 124

test_spark:
	rflx generate -d generated specs/ipv4.rflx
	gprbuild -Pping
	sudo -n timeout 3 obj/ping 127.0.0.1 || test $$? -eq 124

prove:
	rflx generate -d generated specs/ipv4.rflx
	gnatprove -Pping

clean:
	rm -rf generated/* obj python
