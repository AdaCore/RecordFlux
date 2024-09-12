# Tests

## Test Structure

All Python test files have the suffix `_test.py` (e.g., `generator_test.py`). The top level directory `tests` does not contain any test files. Utility functions used by multiple test files are collected in `tests/utils.py`.

### Unit Tests (`tests/unit`)

A unit test verifies the functionality of a single Python module. The result of a unit test mainly depends on the functionality of the to be tested module, but may involve data structures of different modules (counterexample: validating errors which are actually detected in another module). A unit test is simple, fast and generic (counterexample: testing multiple complex examples).

Each test file corresponds to a module in `rflx` and for each module in `rflx` a unit test file must exist.
This correspondence is checked in the Makefile. 
In addition, the per-unit branch coverage can be measured using the `test_per_unit_coverage` make target.


### Integration Tests (`tests/integration`)

Integration tests verify the correct interaction between multiple modules. Complex tests comprising just a single module may also be included in this directory.

### Feature Tests (`tests/feature`)

Each subdirectory is considered as a feature test and must contain a `test.rflx` file. The following actions are performed for each feature test:

- Check parsability of specification and creation of model
- Check for changes in SPARK code generation (if `generated` directory exists)
- Check compilability of generated SPARK code
- Check executability of generated SPARK code (if `sequence` key in config is defined)
- Check provabilility of generated SPARK code (if `proof` key in config is defined)

The checks are implemented in the `feature_test.py` in `tests/integration`, `tests/compilation` and `tests/verification`.

The executability and provability tests require the definition of a state machine called `S`. The actions can be configured in an optional `config.yml` file:

- `input`: A mapping of all readable channels to a list of messages. A message is represented by a space-separated list of bytes (decimal numerals in the range 0 to 255).
- `output`: A list of all writable channels.
- `sequence`: The expected sequence of entered states and IO actions (read and written messages).
- `proof`: If the `proof` key exists, the generated SPARK code for `S` and each additionally listed unit will be proved.

State machine functions can be defined by putting a custom implementation of the `S` package inside the `src` directory.

### End-to-End Tests (`tests/end_to_end`)

End-to-end tests verify the functionality of the entire application. These tests ensure that the application behaves as intended, particularly focusing on its primary use cases.

### Property Tests (`tests/property`)

Property-based testing based on [Hypothesis](https://hypothesis.readthedocs.io/).

#### Reproducing Failures of Property Testing

Property tests don't have fixed inputs and instead generate inputs based on rules provided to the Hypothesis framework.
This poses a problem when reproducing test failures, as the failing input isn't readily available.
The framework provides the failing input in the form of an annotation.
The error message on test failure can look like this:

```
You can reproduce this example by temporarily adding @reproduce_failure('6.23.4', b'AAMBAAABAAAAAAA=') as a decorator on your test case,
```

In this case, you should add the decorator to your testcase and add `from hypothesis import reproduce_failure` to the imports of the test file.

#### Property-Based Compilation Tests

The tests in `tests/property/generator_test.py::test_code_compilation`, when they fail, generate very large output and it's not easy to extract the relevant information.
Here are some directions to deal with this situation:
- Pipe the output to `less` to be able to see the whole output.
- The end of the output should contain the model (also represented as RecordFlux specification) which triggered the test failure.
- The beginning of the output should contain e.g. the compilation error triggered by the model.
- You can also save the model into a `.rflx` file and run `rflx` by hand to reproduce the error.

### Verification Tests (`tests/verification`, `tests/property_verification`)

All Python-based tests that require GNATprove.

### Compilation Tests (`tests/compilation`)

All Python-based tests that require GNAT.

### SPARK Tests (`tests/spark`)

The SPARK tests verify the correctness of the generated SPARK code. A [test suite](https://docs.adacore.com/live/wave/aunit/html/aunit_cb/aunit_cb.html) and [formal verification](https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/en/gnatprove.html) is used to ensure functional correctness and prove absence of runtime errors. The to be proven code is contained in `tests/spark/generated`. This code is also used by integration tests for regression testing.

These tests are not based on pytest and come with their own Makefile.
Run `make test` to compile and run the tests, and `make prove` to run the proofs of the test code and generated SPARK code.

### Language Tests (`tests/language`)

Tests for the generated Langkit-based parser.

### Tool Tests (`tests/tools`)

Tool tests verify the functionality of the complementary tools in the `tools/` directory of the repository.

### IDE Tests (`tests/ide`)

IDE tests allow to verify the functionality of the IDE plugin. These tests are not executed automatically.

### Tests for Examples (`tests/examples`)

New specifications in the `examples/specs` directory are automatically detected. Each message specification is validated by valid and invalid binary samples, which are expected in `tests/examples/data/<package_name>/<message_name>/valid` and `tests/examples/data/<package_name>/<message_name>/invalid`, respectively.

## Test Data (`tests/data`)

- Binary test message (`tests/data/captured`)
- Test fixtures (`tests/data/fixtures`)
- Test specifications (`tests/data/specs`)

Test data should be kept as local as possible. If test data is required for a single, very specific test case, it should be considered to define the test data locally instead of making the data globally available in one of the directories mentioned above.

## Best Practices

Unnecessary duplication of test code and test data should be avoided for maintainablity reasons. For example, the painful act of adapting all specifications due to a change of the syntax or semantics can be mitigated by keeping the number of specifications low (e.g., `Universal::Message` can be reused in new feature tests by adding a symbolic link to `../messages/universal.rflx`). The use of pytest's [test parameterization](https://docs.pytest.org/en/stable/parametrize.html) is particularly useful in unit tests.

## Diffs

The readability of pytest's diffs of complex objects can be improved by setting `RFLX_TESTING` and installing one of the following packages:

- [pytest-clarity](https://github.com/darrenburns/pytest-clarity)
- [pytest-icdiff](https://github.com/hjwp/pytest-icdiff)

## Tutorial

The majority of testing is based on [pytest](https://pytest.org).
The Makefile provides standard targets that execute well-defined subsets of the testsuite.

### Predefined subsets (Makefile targets)

The main Makefile defines some targets which exercise well-defined subsets of the testsuite.
Most of these subsets are still quite time-consuming.
In many cases, the name of the target indicates the subset (e.g. `make test_property` runs the tests in `tests/property`).
We list some notable targets below:

- `make test_coverage`: Run unit, integration and compilation tests, require 100% code coverage.
- `make test_unit_coverage`: Run unit tests, require 95% code coverage.
- `make test_rflx`: Run a comprehensive list of tests (but not all tests), including unit tests and integration tests, as well as compilation tests, but no proofs.
- `make test_examples`: Run the example tests in `tests/examples` as well as the example apps in `examples/apps` (without proofs).
- `make test`: Combination of `test_rflx` and `test_examples`.
- `make test_compilation`: Execute all additional tests where GNAT is required (example apps, examples in documentation and SPARK unit tests).
- `make prove`: Run all tests that run GNATprove, including tests in `tests/spark` and `tests/verification` (see also the explanation for feature tests).

### How to run subsets of tests

During development, to run a specific set of tests, run e.g. `pytest tests/unit` to run all unit tests, or `pytest tests/integration` to run all integration tests.
You can add parallelism by using the `-n` switch, and you can specify (partial) test names using the `-k` switch.
A complete command to run all unit tests with "bounds" in their name:

```
pytest -n 8 -k bounds tests/unit
```

You can add the option `-vv` to get more information in case of a test failure.

## When to modify tests or add new tests

### Unit Tests

Unit testing is essential for early bug detection, facilitates code refactoring, and serves as executable documentation of intended behavior. Speed is an important aspect of unit testing. Fast-running tests allow you to quickly iterate on the code, providing rapid feedback on the correctness of your changes. All new or modified code, except for the code generator, should be covered by unit tests. For the code generator, integration and feature tests are usually preferred because they are easier to maintain.

### Integration Tests

Integration testing ensures that different components of RecordFlux work together seamlessly. An integration test is required if a functionality is added or changed that depends on the interaction of multiple components (e.g., parsing of a specification and subsequent validation of the model).

### Code Coverage

Any code changes may cause the testsuite to fail to reach the coverage targets.
Change the unit tests or add new ones to increase the coverage.

### Fixing a Bug

When a bug is fixed in the tool, a test should be added that would fail if the bug had not been fixed.
This is called a regression test and is intended to protect against the bug resurfacing later.
Depending on the nature of the bug, this can be a unit test or integration test.

### Adding a Feature

Adding a feature should be complemented by these tests (at least):

- Add unit tests (if the feature is not related to code generation)
- Add integration tests if multiple components are involved
- Add or extend a feature test which exercises the feature if the feature is related to code generation (extending tests rather than adding tests can minimize additional test execution time)
