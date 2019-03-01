test: test_python test_spark prove_spark

test_python:
	coverage run -m unittest
	mypy bin/*.py rflx/*.py tests/*.py
	pylint bin/*.py rflx/*.py tests/*.py
	flake8 bin/*.py rflx/*.py tests/*.py
	isort -c -w 100 bin/*.py rflx/*.py tests/*.py

test_spark:
	gprbuild -Ptest
	build/test

prove_spark:
	gnatprove -Ptest --checks-as-errors

clean:
	gprclean -Ptest
	rmdir build
