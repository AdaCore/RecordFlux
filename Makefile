test: test_python test_spark

test_python:
	coverage run -m unittest
	mypy rflx *.py tests/*.py
	pylint -E rflx *.py tests/*.py
	flake8 rflx *.py tests/*.py

test_spark:
	gprbuild -Ptest
	build/test
	gnatprove -Ptest --checks-as-errors

clean:
	gprclean -Ptest
	rmdir build
