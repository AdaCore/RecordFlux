test:
	python -m unittest
	mypy rflx *.py tests/*.py
	gprbuild -Ptest
	build/test
	gnatprove -Ptest

clean:
	gprclean -Ptest
	rmdir build
