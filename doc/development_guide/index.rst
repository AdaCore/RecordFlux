=================
Development Guide
=================

Setup
=====

Additionally to the dependencies listed in the `User's Guide <https://docs.adacore.com/live/wave/recordflux/html/recordflux_ug/index.html>`_ the following software is required:

- `AUnit <https://github.com/AdaCore/aunit>`_ (if FSF GNAT is used)
- `Dnsmasq <https://thekelleys.org.uk/dnsmasq/doc.html>`_ (to test an example app)
- `Node.js <https://nodejs.org/>`_ 20.5.0 or higher (to create the VS Code extension; Node.js should preferably be installed using `nvm <https://github.com/nvm-sh/nvm>`_, the Snap Node.js led to unexpected errors in the past)
- `TeX Live <https://tug.org/texlive/>`_ (to create the PDF documentation)

The FSF GNAT and all Ada dependencies can be installed using Alire.

.. code:: console

   $ make install_gnat
   $ eval `make printenv_gnat`

Set up the development environment.

.. code:: console

   $ make install_devel

All other make targets that require an existing development environment (e.g. `make check`) will automatically download and install all required dependencies as well.
Some dependencies are managed in other git repositories and will be cloned during the initial setup.
The origin of these repositories can be changed by setting the respective variable (e.g., `DEVUTILS_ORIGIN`) appropriately either in the environment or directly in the initial call to `make`.
The initial setup can be repeated after resetting the repository with `make clean_all`.

**Note:**
An editable installation is used, so all changes to the Python source code take effect immediately and no reinstallation is required.
An exception is the `language` module, which contains the specification of the langkit-based specification parser.
The parser must be regenerated and recompiled using `make parser` before changes take effect.

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
- ``check`` Run general checks and static code analysis tools for Python code
- ``test`` Execute tests for Python code and SPARK code
- ``prove`` Run GNATprove on SPARK tests and example apps
- ``format`` Perform automatic code formatting on Python code
- ``install_devel`` Install project in editable mode
- ``doc`` Generate HTML documentation
- ``dist`` Create Python package
- ``clean`` Remove build directories and generated files
- ``clean_all`` Bring the repository to a completely clean state

Additional tools can be found in ``tools/``.

Poetry
------

The Python project is managed by Poetry.
If there is no active virtual environment before executing the `make` commands, the Python project will be installed into a dedicated virtual environment named `.venv`.
If there is already an active virtual environment, RecordFlux will be installed into that virtual environment instead.
Poetry will always be installed into its own environment (`.venv.poetry`).

It is not necessary to explicitly activate the virtual environments before executing any of the `make` targets.
They are used automatically during the execution of `make`.
However, in order to have the `rflx` command directly available in the shell, it is necessary to activate the project's virtual environment.
The following commands can be used to respectively activate and deactivate it, as well as add or remove Poetry to/from the `PATH`.
Note the need to use `source <(...)` in the command below.

.. code:: console

   $ source <(make activate)
   $ deactivate

Alternatively, RecordFlux can be executed also via Poetry by executing `.venv.poetry/bin/poetry run rflx`.

Poetry locks the dependencies to ensure deterministic test results.
`poetry lock` creates the lock file `poetry.lock` based on the dependencies listed in `pyproject.toml`.

Rust
====

Some parts of RecordFlux are implemented in Rust for performance reasons.
The main Rust code is in the `librapidflux` directory.
The Python binding is implemented using [PyO3](https://pyo3.rs/) in the `rapidflux` directory.

The type hints for the Python binding must be specified in the `rflx/rapidflux.pyi` stub file ([PyO3/pyo3#510](https://github.com/PyO3/pyo3/issues/510)).

The test coverage of the main Rust code is checked using [Tarpaulin](https://github.com/xd009642/tarpaulin) and LLVM coverage instrumentation.
The Python binding is tested in the Python test suite (`tests/unit`).

Classes created by PyO3 cannot be pickled by default ([PyO3/pyo3#100](https://github.com/PyO3/pyo3/issues/100)).
Pickling of objects can be enabled by defining `__setstate__`, `__getstate__`, `__getnewargs__` and the module name (`#[pyclass(module = "rflx.rapidflux")]`).

VS Code extension
=================

To build the VS Code extension and install it to VS Code there are several options.
Below are two possible workflows.

Option 1
--------

To build the `recordflux.vsix` package and install it to VS Code directly in one step execute the following command at the project root:

.. code:: console

   make -C ide/vscode install

**Note:**
In this workflow the editable installation of RecordFlux is not made aware of the built extension.

Option 2
--------

Alternatively, execute the following command at the project root to just build the `recordflux.vsix` package:

.. code:: console

   make -C ide/vscode dist

Then, make this available to the editable installation of RecordFlux by executing:

.. code:: console

   make install_devel

Finally, use the dedicated `rflx install` sub-command to install the extension into VS Code:

.. code:: console

   rflx install vscode

Code Design Guidelines
======================

The following guidelines should be followed for clarity and consistency throughout the project:

- The formatting of error messages should be consistent. An error message starts with a lowercase letter and identifiers are highlighted by double quotes.
- Private functions are prefered over inner functions. Long inner functions can impede the comprehension of a function.
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

Error messages
==============

Philosophy
----------

Error messages should be beginner-friendly while maintaining an efficient workflow for experienced users.

Phrasing
--------

This section applies to all types of descriptions within error messages (verifier and validator).
When composing error messages, adhere to the following guidelines:

1.
   Error messages should be brief and to the point, rather than full English sentences, providing concise explanations of the diagnostic.
2.
   Ensure that the message cannot be misinterpreted by users.
   If there is potential ambiguity, make efforts to clarify it.
3.
   Consider that error messages may be viewed within an IDE or any program supporting Language Server Protocol (LSP).
   Some code editors may display error messages alongside the user's code or in a designated area (e.g., VSCode's "problem" menu).
   Keep this in mind to enhance the user experience when these messages are presented.

Diagnostic fields
-----------------

Error messages represent the interface between RecordFlux and the user.
They are composed of 4 sections that serve different purposes:

1.
   The actual error message.
2.
   The user's code with some relevant annotations.
3.
   An optional "note" part that adds relevant information to the error message shown above.
4.
   An optional "help" section that contains possible fixes, further explanations about the error, and how it could be resolved.

Error Message
-------------

This section contains the actual error message.
The error message should be short, descriptive and start with a capital letter.
The message should be as beginner-friendly as possible.
However, sometimes it's not always possible to write a beginner-friendly error message because the error is too complicated to be explained in a single sentence.
In those cases, try to phrase the error in a way that an intermediate or expert user could understand and iterate quickly in the edit/check cycle.
This message isn't meant to be a **complete English sentence** but rather a **short and descriptive message**.
The message should appear in **bold** and be preceded by the following message in red: ``error: ``.
The prefix represents the diagnostic's severity; it can be one of the following:

- error (in red)
- warning (in yellow)
- info (in blue)
- help (in light blue)
- note (in yellow)

Example
^^^^^^^

This following sentence:

.. code:: console

    Type "Foo" is not declared

Should rather be:

.. code:: console

    Undeclared type "Foo"

A complete example should be:

.. code:: console

   error: Undeclared type "Foo"

User code
---------

This section is used to show the user's code with relevant annotations to provide the user with visual explanations about the actual problem in their code.
It **must show the actual user code** and not a pretty-printed version of it based on a syntax tree or any other data structure.
Spans are used to highlight problematic parts of the user's code.
The caret ``^`` character is used for that purpose.
If the user's terminal supports it, these must be displayed in red.
It's also possible to add an optional description next to a span to give more details to the user.
These description are displayed in blue.
Note that source file lines are also displayed in this section to make the error easier to locate.
If there is more than one line gap between two annotations, the representation must not show these lines as they represent useless information.
In this case, three dots (``...``) should be displayed.
This section is always preceded by an arrow followed by the file path relative to current working directory, a line number, and a column offset.
The same rules apply when the content is in the form of a diff.

Examples
^^^^^^^^

.. code:: console

   --> atm.rflx:20:3:
    20 | type Cell (Cell_Format : Cell_Type) is
              ^^^^ In this message declaration
        ...
    23 |          then Generic_Flow_Control
    24 |             if Cell_Format = UNI
                        ^^^^^^^^^^^^^^^^^ If this condition is met
        ...
    31 |       Virtual_Channel_Identifier : Virtual_Channel_Identifier
    32 |          then Generic_Flow_Control;
                  ^^^^^^^^^^^^^^^^^^^^^^^^^ Transition goes back to "Generic_Flow_Control"

Notes
-----

This section is optional and should be used to add relevant details to a diagnostic.
Fixes and tips do not belong in this section; such things should be in the hint sections.
Every note is represented as a span labeled with the corresponding explanation next to it.
This section may include a user's code snippet when it's relevant.
The "note" keyword is displayed in yellow.

Examples
^^^^^^^^

.. code:: console

   note: Error can cause other fields to not be aligned to 8 bits

.. code:: console

   note: Type `Foo` is considered as scalar
         --> ethernet.rflx:6:9:
         6 |    type TCI is range 0 .. 2 ** 16 - 1 with Size => 16;
                     ^^^ Type declared here

Hints
-----

This section is dedicated to helping the user by providing possible fixes, explaining potential error causes, etc.
The "hint" keyword is displayed in blue.
Targeting primarily new RecordFlux users, this section is displayed at the end of the error, as experienced users may skip it.
Links to relevant documentation can also be included.
Possible fixes are presented as a diff: additions appear in green and removals in red.


Examples
^^^^^^^^

.. code:: console

   help: 42 mod 8 = 2, thus this size is not a multiple of 8
   help: Sizes are represented as a number of bits, not bytes; Did you mean:
         --> wireguard.rflx
         28 | + with Size => 42 * 8
         28 | - with Size => 42

.. code:: console

    help: Remove transition to "Number" in "Value" (this may not be the desired behavior)
          --> wireguard.rflx
          11 | -         then Number
          12 | -            if Number = 2

Full examples
-------------

.. code:: console

    error: Maximum bound exceeds limit (2**63 - 1)
           --> bad.rflx3:10
           3 | type Integer is range 0 .. 2 ** 64 - 1 with Size => 8 * 8;
                                          ^^^^^^^ Value is too big

    help: Maximum value of the upper bound is `2**63 - 1`
    help: Consider using `Opaque` instead (see the `Opaque` type at https://docs.adacore.com/live/wave/recordflux/html/recordflux_lr/language_reference.html#message-types)


.. code:: console

   error: Structure contains cycle
          --> wireguard.rflx:7:10
          7  |        type Bad_Message is
              ...
          9  |            Number : Code;
                          ^^^^^^^^^^^^^ Next field is implicitly `Value`

          10 |            Value : Integer
          11 |               then Number
                             ^^^^^^^^^^^ Transition may produce a cycle `Number`

          12 |                    if Number = 2
                                  ^^^^^^^^^^^^^ Condition leads to a circular reference to `Number` if it holds true

    note: Sound message must not contain a cycle

    help: Remove transition to "Number" in "Value" (this may not be the desired behavior)
          --> wireguard.rflx
          11 | -             then Number
          12 | -                 if Number = 2


.. code:: console

    error: Condition is always true
           --> wireguard.rflx:18:56
           16 |    Reserved : Reserved
           17 |        then Sender
           18 |            if Message_Type = Handshake_Init or Message_Type = Handshake_Init
                                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    help: Remove the affected condition:
          18 | + if Message_Type = Handshake_Init
          18 | - if Message_Type = Handshake_Init or Message_Type = Handshake_Init
    help: Check that your condition is not erroneous

.. code:: console

   warning: condition might always be true
            --> foo.rflx:37:12:
            36 |       then Other
            37 |          if Foo = Field and Bad = Baz
                             ^^^^^^^^^^^^^^^^^^^^^^^^^

Validator
---------

Validator's error messages resemble the checker's error messages and largely follow the guidelines mentioned earlier, with a few exceptions.

The validator's diagnostic includes the following sections:

1.
    Tests actually run by RecordFlux
2.
    A failure list with detailed explanations
3.
    If enabled, the coverage report
4.
    A final line reporting the number of tests run, failed, and succeeded


Tests run
^^^^^^^^^

This part displays tests as they are run by RecordFlux.
A test can either pass or fail.
The word ``PASSED`` is shown in green, and ``FAILED`` is displayed in red.

.. code:: console

    PASSED  tests/examples/data/wireguard/handshake/valid/wg_cookie_response.raw
    FAILED  tests/examples/data/wireguard/handshake/valid/wg_handshake_init.raw
    FAILED  tests/examples/data/wireguard/handshake/valid/wg_handshake_response.raw
    PASSED  tests/examples/data/wireguard/handshake/valid/wg_transport.raw
    PASSED  tests/examples/data/wireguard/handshake/invalid/wg_handshake_init_no_sender.raw
    PASSED  tests/examples/data/wireguard/handshake/invalid/wg_handshake_response_no_receiver.raw
    PASSED  tests/examples/data/wireguard/handshake/invalid/wg_handshake_response_no_sender.raw
    PASSED  tests/examples/data/wireguard/handshake/invalid/wg_invalid_type.raw
    PASSED  tests/examples/data/wireguard/handshake/invalid/wg_reserved_field_not_zero.raw

Failures
^^^^^^^^

This part serves as a list of errors accompanied by detailed explanations.
Each error message is preceded by the sample that triggered the error.
The format for each error message remains consistent with the guidelines outlined in the previous section.
However, a hex dump may be included if relevant.
This section is demarcated by two lines of equal signs (``=``).
Each test name is enclosed by dash characters (``-``).

Example
"""""""

.. code:: console

    =========================== Failures ======================================
     ----- tests/examples/data/wireguard/handshake/valid/wg_handshake_init.raw -----
      error: Cannot set value for field "Reserved"
             --> wireguard.rflx:15:12
             15 | Reserved : Reserved
                  ^^^^^^^^ Value cannot be set to `16777215`

      help: Value `16777215` is not in the range `0 .. 0`
            --> wireguard.rflx:11:4
            11 |    type Reserved is range 0 .. 0 with Size => 3 * 8;
                         ^^^^^^^^ Declared here

     ----- tests/examples/data/wireguard/handshake/valid/wg_handshake_response.raw -
      error: Parsed message is shorter than the sample
             --> wireguard.rflx:13:4
             13 |    type Handshake is
                          ^^^^^^^^^ This message
      note: Parsed message has a length of 32 bits but the sample message is 24 bits long
      note: Exceeding bytes:
            --> Hex dump
            xxxx | cafe cafe cafe cafe
            xxxx | cafe cafe cafe cafe
                   ^^^^^^^^^^^^^^^^^^^ Unused bytes

     ----- tests/examples/data/wireguard/handshake/valid/wg_handshake_response.raw -
      error: Sample message is too small
             --> wireguard.rflx:13:4
             13 |    type Handshake is
                          ^^^^^^^^^ This message
                 ...
             56 |         Mac_Second : Opaque
                          ^^^^^^^^^^ Missing data to parse this field

      note: Parser failed because the sample message is smaller than the specification
      note: 6 bits are missing for the parser to parse the message

    ================================================================================
