=================
Development Guide
=================

Setup
=====

Additionally to the dependencies listed in the `User's Guide <https://docs.adacore.com/live/wave/recordflux/html/recordflux_ug/index.html>`_ the following software is required:

- `AUnit <https://github.com/AdaCore/aunit>`_ (if FSF GNAT is used)
- `Alire <https://alire.ada.dev/>`_ 2.0.1 (to install FSF GNAT and Ada dependencies including AUnit)
- `Dnsmasq <https://thekelleys.org.uk/dnsmasq/doc.html>`_ (to test an example app)
- `Node.js <https://nodejs.org/>`_ 20.5.0 or higher (to create the VS Code extension; Node.js should preferably be installed using `nvm <https://github.com/nvm-sh/nvm>`_, the Snap Node.js led to unexpected errors in the past)
- `TeX Live <https://tug.org/texlive/>`_ (to create the PDF documentation)
- `Rust <https://www.rust-lang.org/>`_ 1.77 or newer (to build the Rust source code and run tests)

If Alire has been set up, the FSF GNAT and all Ada dependencies can be installed as follows:

.. code:: console

   $ make install_gnat
   $ eval `make printenv_gnat`

Rust should be installed using `rustup <https://rustup.rs/>`_.
It's also possible to use GNAT Pro for Rust to build RecordFlux.
See the official `rustup <https://rustup.rs/>`_ website to get installation instructions.

Set up the development environment.

.. code:: console

   $ make install

All other make targets that require an existing development environment (e.g. ``make check``) will automatically download and install all required dependencies as well.
Some dependencies are managed in other git repositories and will be cloned during the initial setup.
The origin of these repositories can be changed by setting the respective variable (e.g., ``DEVUTILS_ORIGIN``) appropriately either in the environment or directly in the initial call to ``make``.
The initial setup can be repeated after resetting the repository with ``make clean_all``.

**Note:**
An editable installation is used, so all changes to the Python source code take effect immediately and no reinstallation is required.
An exception is the ``language`` module, which contains the specification of the langkit-based specification parser.
The parser must be regenerated and recompiled using ``make parser`` before changes take effect.

The repository contains configuration files for Visual Studio Code which enable most linters available from the Makefile and coverage reports integrated into the IDE.
To make use of the coverage data, the `Coverage Gutters <https://github.com/ryanluker/vscode-coverage-gutters>` extension needs to be installed.
The path to GNAT and SPARK has to be configured to allow compilation and proof tests to succeed.
After configuring the path to the desired tool versions on the command line, a file called ``.env`` should be created as follows:

.. code:: console

   $ echo "PATH=$PATH" > .env

Note, that this file has to be recreated whenever different tool versions are to be used.

Tools
=====

Make
----

Make targets for common development tasks are:

- ``all`` Execute ``check``, ``test`` and ``prove`` (default target)
- ``check`` Run general checks and static code analysis tools for Python and Rust code
- ``test`` Execute tests for Python, SPARK and Rust code
- ``prove`` Run GNATprove on SPARK tests and example apps
- ``format`` Perform automatic code formatting on Python code
- ``install`` Install project in editable mode
- ``doc`` Generate HTML documentation
- ``dist`` Create Python package
- ``clean`` Remove build directories and generated files
- ``clean_all`` Bring the repository to a completely clean state

Additional tools can be found in ``tools/``.

Poetry
------

The Python project is managed by `Poetry <https://python-poetry.org/>`_.

Poetry's virtual environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Poetry will be automatically installed into its own virtual environment (``.venv.poetry``) the first time when any of the ``make`` commands that require it are executed.
It is important that the Poetry's own virtual environment is kept separate from the development virtual environment described next.

Project's virtual environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Python packages of RecordFlux and its dependencies will be installed into a separate virtual environment.

Default (.venv)
"""""""""""""""

The default and recommended scenario is to let ``make`` and Poetry handle the creation of the project's virtual environment.
If there is no active virtual environment before executing the ``make`` commands, a dedicated virtual environment named ``.venv`` will be created if it doesn't exist yet.

.. note::
   If there is already an active virtual environment, RecordFlux will be installed into that virtual environment instead.
   Hence, if you intend to use the default scenario make sure to deactivate any other virtual environment in the current shell.
   See the section `Custom virtual environment (advanced usage)`_ for more information.

It is not necessary to explicitly activate the default virtual environment.
You can run the ``make`` commands directly and Poetry will use ``.venv`` automatically.
However, in order to have the ``rflx`` command directly available in the shell or use the Python tools from the virtual environment it is necessary to activate the project's virtual environment.
The following commands can be used to respectively activate and deactivate it, as well as add or remove Poetry to/from the ``PATH``.
Note the need to use ``source <(...)`` in the command below.

.. code:: console

   $ source <(make activate)
   $ deactivate

Alternatively, RecordFlux can be executed also without activating the default virtual environment by calling:

.. code:: console

  $ .venv.poetry/bin/poetry run rflx

Custom virtual environment (advanced usage)
"""""""""""""""""""""""""""""""""""""""""""

If another virtual environment is active before running ``make`` commands that virtual environment will be used for RecordFlux and its dependencies.
The following points need to be kept in mind in that scenario:

* If a non-default virtual environment is intended to be used, then it must always be explicitly activated before running the ``make`` commands.
  *If that is not done, then Poetry will switch to the default environment instead.*
* In order for the ``source <(make activate)`` command to work with a non-default virtual environment it is recommended to set the following environment variable:

  .. code:: console

    $ export DEVEL_VENV=/path/to/custom/venv

* The command ``make clean_all`` removes the ``DEVEL_VENV`` directory.

However, Poetry will always be installed into its own environment as described in the section `Poetry's virtual environment`_.

Python dependencies and Poetry lock
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Dependencies managed by Poetry
""""""""""""""""""""""""""""""

The Python dependencies for the RecordFlux project are specified in ``pyproject.toml``.

.. note::
   The ``pyproject.toml`` file is not supposed to be edited manually.
   The dependencies should be configured in the ``pyproject.toml.in`` file instead.
   The respective ``make`` rules refer to that and compose the ``pyproject.toml`` file automatically.

The ``pyproject.toml`` file typically specifies a range of supported versions for each dependency.
However, to ensure deterministic test results the dependencies are locked to concrete versions in the ``poetry.lock`` file.
If the dependencies in ``pyproject.toml`` are modified or a different compatible version of a dependency is to be used, then the lock file should be updated using the `poetry lock <https://python-poetry.org/docs/cli/#lock>`_ command.
In addition, if the dependencies in ``devutils`` have changed, the lock file must be updated using the following specific command:

.. code:: console

   $ pip install "./devutils[devel]"
   $ poetry lock --no-update

When building RecordFlux through ``make`` the ``poetry install`` command is executed to install any missing dependencies and set the versions of all the dependencies specified in ``pyproject.toml`` to the versions selected in ``poetry.lock``.

Additional Python packages
""""""""""""""""""""""""""

It is also possible to add further packages to the project's virtual environment.
For instance, see the `README.md <../../tests/README.md>`_ file in the ``tests`` folder for some additional packages that make the test outputs more readable.

Rust
====

Some parts of RecordFlux are implemented in Rust for performance reasons.
The main Rust code is in the ``librapidflux`` directory.
The Python binding is implemented using `PyO3 <https://pyo3.rs/>`_ in the ``rapidflux`` directory.

The type hints for the Python binding must be specified in the ``rflx/rapidflux.pyi`` stub file (`PyO3/pyo3#510 <https://github.com/PyO3/pyo3/issues/510>`_).

The test coverage of the main Rust code is checked using `cargo-llvm-cov <https://github.com/taiki-e/cargo-llvm-cov>`_.
The Python binding is tested in the Python test suite (``tests/unit``).

Classes created by PyO3 cannot be pickled by default (`PyO3/pyo3#100 <https://github.com/PyO3/pyo3/issues/100>`_).
Pickling of objects can be enabled by defining ``__setstate__``, ``__getstate__``, ``__getnewargs__`` and the module name (``#[pyclass(module = "rflx.rapidflux")]``).

It is advised to `configure <https://rust-analyzer.github.io/manual.html#installation>`_ and use ``rust-analyzer`` with ``clippy`` checks enabled within your IDE.

VS Code extension
=================

To build the VS Code extension, execute the following command at the project root:

.. code:: console

   make vscode

Finally, use the dedicated ``rflx install`` sub-command to install the extension into VS Code:

.. code:: console

   rflx install vscode

Code Design Guidelines
======================

The following guidelines should be followed for clarity and consistency throughout the project:

- The formatting of error messages should be consistent and conform to the guidelines given in the section `Error Messages`_.
- Private functions are preferred over inner functions. Long inner functions can impede the comprehension of a function.
- Internal methods are prefixed by a single underscore.
- The methods of a class are sorted to make it easer to identify the public interface of a class:
   1. Special methods (starting and ending with double underscores)
   2. Public methods
   3. Internal methods (starting with single underscore).

Pull requests
=============

We accept pull requests `via GitHub <https://github.com/AdaCore/RecordFlux/compare>`_.
To contribute to the project, fork it under your own GitHub user and perform your changes on a topic branch.
Ideally, create an issue in the upstream repository describing the problem you would like to solve and your intention to work on it.
This will help us to point you to potential prior work and discuss your idea.
Your branch should be named ``issue_<ISSUE_NUMBER>``, e.g. ``issue_694`` where #694 is the ticket you created, and the issue should be linked in the PR (by adding ``Closes #<ISSUE_NUMBER>`` in the PR description).
Ideally, the PR title is prefixed with ``Issue <ISSUE_NUMBER>:``.
For small (!) changes descriptive branch names without a ticket are acceptable.

When submitting a pull request, your topic branch should be rebased to the current upstream ``main`` branch.
Verify that all automatic checks performed by ``make check``, ``make test`` and ``make prove`` succeed before submitting the PR.
For Python code we follow and automatically enforce the coding style of `Black <https://pypi.org/project/black/>`_.
You can format your code automatically using the ``make format`` target on the command line.
For Ada code (examples as well as generated code) please follow `our Ada style guide <https://github.com/Componolit/ada-style>`_.

We enforce 100% branch coverage for Python code using `pytest <https://pytest.org>`_.
Make sure to add relevant test cases to achieve that for your code.
See the `test documentation <https://github.com/AdaCore/RecordFlux/blob/main/tests/README.md>`_ and have a look at the existing test cases in the ``tests`` directory to get an idea of the structure of our test suite.
Our Python code is also statically type-checked using `mypy <http://mypy-lang.org/>`_.
Make sure to include the required type annotations with your code.

Your code will be reviewed by at least one core developer before inclusion into the project.
Don’t be discouraged should we have many comments and ask you for a lot of changes to your pull request.
This even happens to the most experienced developers in our project and we consider these discussions an essential part of the development process and a necessity to maintain high quality.
Don’t hesitate to open an issue if you have any question or submit the pull request in draft mode first.

If the code review reveals that changes are required, the necessary changes should be added in a new commit and the corresponding review comment should be answered.
This makes it easier for a reviewer to track which issues were addressed.
All review comments must be set to resolved by a reviewer before a pull request can be merged.
Force pushing is required and accepted for rebasing to the base branch.
Commits may be squashed before the pull request is merged to prevent a high number of "Fix review comments" commits.
Squashing should be avoided before the changes have been accepted by all reviewers.

Development loop
================

The following is a suggested workflow that should fit many scenarios.
When working on a change, it is a good idea to use the following commands often:

.. code:: console

   $ make format && make check

The first command formats the code, while the second runs various checks, including type checks.
This should catch a variety of syntax and typing errors.

Test suite
----------

While developing, it can be useful to test the code on a single example (e.g. a RecordFlux spec that contains the new feature to develop, or triggers the bug to fix, etc).
Once the desired behavior is achieved, one can test the change on the larger test suite, e.g., using ``make test``.

If individual tests fail, the code needs to be fixed or the test changed.
Knowing which action should be done requires checking out the unit test code.

RecordFlux's test suite is composed of both Rust and Python tests.
``pytest`` is used to test Python code.
Rust's built-in ``test`` is used in conjunction with ``rstest``, which adds useful features such as fixtures and parameterized tests.
More information can be found in the `rstest documentation <https://docs.rs/rstest/latest/rstest/>`_.

Python tests
^^^^^^^^^^^^

Here are some tips to use ``pytest`` efficiently:
Individual failing tests can be run with a command like this:

.. code:: console

   $ pytest -k testname tests/unit

Other useful options of ``pytest`` include:

- ``--last-failed`` or ``--lf`` runs only the tests that failed during the last run of ``pytest``.
- ``--exitfirst`` or ``-x`` stops at the first error.
- ``-n <number>`` specifies the number of tests to run in parallel.
- The full path to a test can be given using the syntax ``pytest path/to/test.py::test_foo``.

Rust tests
^^^^^^^^^^

The Rust test suite can be invoked using ``make``:

.. code:: console

    $ make test_rapidflux

This will run the whole test suite, compute code coverage, and finally try to generate some mutations to check that the tests are still able to catch these mistakes.

As generating code mutations (and testing them) can take time, it is possible to only run tests and compute code coverage using the following command:

.. code:: console

    $ make test_rapidflux_coverage

It is possible to run a single test or a subset of them by specifying their module.
The example below only runs tests related to the module ``diagnostics::errors``:

.. code:: console

    $ cargo nextest run diagnostics::errors

It is also possible to run a single test like this:

.. code:: console

    $ cargo nextest run path::to::module::tests::<test_name>

Note that advanced test filtering can be achieved using cargo-nextest's `DSL <https://nexte.st/docs/filtersets/reference/>`_.
It is recommended to use ``cargo-nextest`` instead of the default ``cargo-test`` because it brings a lot of additional features.

Tests with side effects
"""""""""""""""""""""""

Be aware of potential side effects (e.g., registering source code) when authoring tests.
When side effects can't be avoided, it is possible to prevent a test from being run at the same time as other tests using the ``#[serial]`` attribute.
The ``#[parallel]`` attribute can be used for tests that may be run in parallel without clashing with tests marked as ``#[serial]``.
In general, this attribute should be only used to test functionalities that relies on global state to work (e.g source code management, logger...).
Having a look to the `serial <https://docs.rs/serial_test/latest/serial_test/>`_ documentation before using this attribute is recommended.
This attribute must always be the **last** attribute of a given test; otherwise, the serial attribute may not work and may cause potential race conditions with other tests.

Doctests
""""""""

When authoring Rust documentation, try to include examples when it makes sense to do so.
Rust examples are automatically compiled and all the assertions will be executed.
This is a good way to make sure that the documentation is always up to date with the current API.
It is also recommended to have a look at the `rustdoc <https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html>`_ book to use ``rustdoc`` efficiently.

Error messages
==============

Philosophy
----------

Error messages should be beginner-friendly while maintaining an efficient workflow for experienced users.

Phrasing
--------

This section provides guidelines for composing all types of descriptions within error messages:

1.
   Error messages should be brief and to the point, rather than full English sentences, providing concise explanations of the diagnostic.
2.
   Messages are crafted to avoid misinterpretation by users, with efforts made to clarify any potential ambiguity.
3.
   Error messages are designed with the possibility of being viewed within an IDE or any program supporting Language Server Protocol (LSP).
   They may be displayed alongside the user's code or in a designated area, such as VS Code's "problems" menu.
   This context is considered to enhance the user experience.

Since error messages are no full sentences, they do not start with a capital letter nor end with a period.

Example
^^^^^^^

This following sentence:

.. code:: console

    Type "Foo" is not declared

Should rather be:

.. code:: console

    undeclared type "Foo"

A complete example should be:

.. code:: console

   error: undeclared type "Foo"

Diagnostic structure
--------------------

Error messages represent the interface between RecordFlux and the user.
An error message itself is considered as an "entry".
Each entry has the following elements:

1.
   The actual error message.
2.
   Some relevant annotations on the user's code.

Error message
-------------

The error message should be short and descriptive.
The message should be as beginner-friendly as possible.
However, sometimes it's not always possible to write a beginner-friendly error message because the error is too complicated to be explained in a single sentence.
In those cases, try to phrase the error in a way that an intermediate or expert user could understand it and iterate quickly through the edit/check cycle.
The message appears in **bold** and is preceded by its severity.

User code
---------

The user's source code is shown with relevant annotations to provide visual explanations of the problem.
Annotations highlight problematic parts, offer context, or give hints.
Identifiers are represented in double quotes.
Optional descriptions can be added next to annotations to give more details.
Source file lines are displayed to make the error easier to locate.
Precision is key in providing code locations that are directly related to the problem, avoiding repetitive information in annotation labels and adding relevant details when necessary.
If no additional content is required, labels are omitted to avoid unnecessary noise.

Severities
----------

A severity can be one of the following:

- error (in red)
- warning (in yellow)
- info (in blue)
- help (in light blue)
- note (in yellow)

error
^^^^^

The "error" severity is used to report problems in user code or program failures caused by the user, such as a missing source code file.

.. code:: console

    error: Expected 'is', got 'end'
     --> test.rflx:3:1
      |
    3 | end Test;
      | ^^^
      |

warning
^^^^^^^

The "warning" severity is used to report potential issues in the code, such as potential misuse of a construct or unexpected behavior from the user's perspective.

.. code:: console

   warning: condition might always be true
    --> foo.rflx:37:12:
    36 |       then Other
    37 |          if Foo = Field and Bad = Baz

note
^^^^

The "note" severity provides additional context to a message.
Error message authors are advised to provide the most relevant context required to understand the problem, avoiding verbose errors that could reduce readability.
When extensive context is needed, it may be beneficial to include notes in a separate entry to prevent lengthy notes from overshadowing other annotations.

.. code:: console

   error: structure of "Test::Cycle" contains cycle
    --> test.rflx:3:9
     |
   3 |      type Cycle is
     |  _________^
   4 | |       message
   5 | |          F1 : I;
     | |                 - note: field "F1" links to "F2"
   6 | |          F2 : I
   7 | |             then F1;
     | |             ------- note: field "F2" links to "F1"
   8 | |       end message;
     | |_________________^
     |

help
^^^^

The "help" severity provides hints on how to fix the code.
It can also suggest fixes for common mistakes, such as assuming sizes are given in bytes in RecordFlux.
As with "note", "help" can be given in a separate entry when necessary to maintain readability.

.. code:: console

   error: modular integer types are not supported
    --> test.rflx:2:9
     |
   2 |    type I is mod 5;
     |         ^^^^^^^^^^
     |
   help: use "type I is range 0 .. 4 with Size => 3" instead
    --> test.rflx:2:9
     |
   2 |    type I is mod 5;
     |         ----------
     |


info
^^^^

The "info" severity is **reserved for logging purposes** and should not be used to report an error message.
"note" is preferred to add context to an error message.
"info" is used to report what RecordFlux is doing, such as generating code or verifying a message.

.. code:: console

   info: Parsing test.rflx
   info: Processing Test
   info: Verifying __BUILTINS__::Boolean
   info: Verifying __INTERNAL__::Opaque

Annotations
-----------

Annotations have their own severity, which means that an error message can have "error", "warning", "note" and "help" annotations at the same time.
This feature is useful to create compact error messages, however, sometimes having a separate entry dedicated to "note" or "help" may be preferable to improve readability.
It is up to the error message author to think about the most suitable way to present the error to the user.
Each annotation can have an optional text that is printed next to it.

Default Annotations
^^^^^^^^^^^^^^^^^^^

It is often useful to show a piece of user's code along with a message concerning the code.
The simplest way to do that is by using the default annotation mechanism implemented in the `ErrorEntry` class which can be used to construct messages of all the severity categories defined above.

For instance, the following error message:

.. code:: console

   test.rflx:4:4: error: "session" keyword is deprecated

is by default automatically augmented by the tool with a default annotation like this:

.. code:: console

   error: "session" keyword is deprecated
   --> test.rflx:4:4
     |
   4 |    session S is
     |    ^^^^^^^

However, if an explicit `help` annotation has been provided in the message constructor, then having it together with the default annotation leads to a duplicate underline under the highlighted code section:

.. code:: console

   error: "session" keyword is deprecated
   --> test.rflx:4:4
     |
   4 |    session S is
     |    ^^^^^^^
     |    ------- help: use "machine" instead

To avoid this set the `generate_default_annotation` parameter to `False` when constructing an instance of `ErrorEntry` class.
This yields a more optimal message like the one below:

.. code:: console

   error: "session" keyword is deprecated
   --> test.rflx:4:4
     |
   4 |    session S is
     |    ------- help: use "machine" instead

Full examples
-------------

.. code:: console

   error: size of "I" exceeds limit (2**63)
    --> test.rflx:2:50
     |
   2 |    type I is range 0 .. 2 ** 63 - 1 with Size => 16 * 8;
     |                                                  ^^^^^^
     |


.. code:: console

   error: fixed size field "One" does not permit a size aspect
    --> test.rflx:6:26
     |
   2 |    type I is range 0 .. 255 with Size => 8;
     |                                          - note: associated type size defined here
   3 |    type M is
   4 |       message
   5 |          One : I
     |          --- help: modify this field's type, or alternatively, remove the size aspect
   6 |             with Size => 16;
     |                          ^^
     |

.. code:: console

   error: field "My_Feld" does not exist in "Test::M"
     --> test.rflx:13:15
      |
    2 |    type M is
      |         - note: type "Test::M" declared here
    3 |       message
    4 |          My_Field : Opaque
      |          -------- help: field with similar name
    5 |             with Size => 8;
   ...
   12 |
   13 |    for M use (My_Feld => N);
      |               ^^^^^^^
      |


.. code:: console

   warning: condition might always be true
    --> foo.rflx:37:12:
    36 |       then Other
    37 |          if Foo = Field and Bad = Baz
                     ^^^^^^^^^^^^^^^^^^^^^^^^^
