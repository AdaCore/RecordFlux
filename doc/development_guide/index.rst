=================
Development Guide
=================

Setup
=====

Additionally to the dependencies listed in the `User's Guide <https://docs.adacore.com/live/wave/recordflux/html/recordflux_ug/index.html>`_ the following software is required:

- `AUnit <https://github.com/AdaCore/aunit>`_ (if FSF GNAT is used)
- `Dnsmasq <https://thekelleys.org.uk/dnsmasq/doc.html>`_
- `Node.js <https://nodejs.org/>`_ 20.5.0 or higher (see `nvm <https://github.com/nvm-sh/nvm>`_ for an alternative installation method)
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
The following `make` targets can be used to respectively activate and deactivate it, as well as add or remove Poetry to/from the `PATH`.
Note the need to use `source <(...)` in the commands below.

.. code:: console

   $ source <(make activate)
   $ source <(make deactivate)

Alternatively, RecordFlux can be executed also via Poetry by executing `.venv.poetry/bin/poetry run rflx`.

Poetry locks the dependencies to ensure deterministic test results.
`poetry lock` creates the lock file `poetry.lock` based on the dependencies listed in `pyproject.toml`.

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

Project management
==================

The work is organized and prioritized using GitHub's project boards.
At any time there is at least one project board for the planned next release and a project board for all potential future tasks.
A project board for an upcoming release has four columns:

- To Do: Non-processed issues planned for the release.
- Design: Issues for which the design is currently created or discussed. Assigned to a specific person. Skipped for small issues or bugs.
- Implementation: Issues which are currently implemented. Assigned to a specific person.
- Review: Completed issues for which a PR is open or will be opened soon. Each issue is linked to the corresponding PR.
- Done: Closed issues.
