test: test_python test_spark prove_spark

test_python:
	coverage run --branch --source=rflx -m unittest -b
	mypy bin/*.py rflx/*.py tests/*.py
	pylint bin/*.py rflx/*.py tests/*.py
	flake8 bin/*.py rflx/*.py tests/*.py
	isort -c -w 100 bin/*.py rflx/*.py tests/*.py

test_spark:
	gprbuild -Ptest
	build/test

prove_spark:
	gnatprove -Ptest --checks-as-errors

prove_spark_ci:
	gnatprove -Ptest --checks-as-errors -Xaunit=no

clean:
	gprclean -Ptest
	rmdir build
