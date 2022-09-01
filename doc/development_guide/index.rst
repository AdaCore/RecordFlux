=================
Development Guide
=================

Setup
=====

Additionally to the dependencies listed in the :ref:`User's Guide` the following libraries are required:

- `AUnit <https://github.com/AdaCore/aunit>`_ |AUnit Alire Crate| (if FSF GNAT is used)

.. |AUnit Alire Crate| image:: https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/aunit.json
   :target: https://alire.ada.dev/crates/aunit.html

The FSF GNAT and all Ada dependencies can be installed using Alire.

.. code:: console

   $ make install_gnat
   $ eval `make printenv_gnat`

``pip`` can be used to install the Python project in editable mode. The use of a `virtual environment <https://docs.python.org/3/tutorial/venv.html>`_ is recommended.

.. code:: console

   $ virtualenv -p python3 venv
   $ source venv/bin/activate
   $ make install_devel

Tools
=====

Make targets for common development tasks are:

- ``all`` Execute ``check``, ``test`` and ``prove``
- ``check`` Run general checks and static code analysis tools for Python code
- ``test`` Execute tests for Python code and SPARK code
- ``prove`` Run GNATprove on SPARK tests and example apps
- ``format`` Perform automatic code formatting on Python code
- ``install_devel`` Install project in editable mode
- ``upgrade_devel`` Upgrade all development dependencies (note: ``install_devel`` must be executed before changes in ``setup.py`` take effect)
- ``doc`` Generate HTML documentation
- ``dist`` Create Python package
- ``setup_ide`` Install files required for RecordFlux IDE integration

Additional tools can be found in ``tools/``.

Code Design Guidelines
======================

The following guidelines should be followed for clearity and consistency throughout the project:

- The formatting of error messages should be consistent. An error message starts with a lowercase letter and identifiers are highlighted by double quotes.
- Private functions are prefered over inner functions. Long inner functions can impede the comprehension of a function.
- Internal methods are prefixed by a single underscore.
- The methods of a class are sorted to make it easer to identify the public interface of a class:
   1. Special methods (starting and ending with double underscores)
   2. Public methods
   3. Internal methods (starting with single underscore).

Pull requests
=============

We accept pull requests `via GitHub <https://github.com/Componolit/RecordFlux/compare>`_.
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
See the `test documentation <https://github.com/Componolit/RecordFlux/blob/main/tests/README.md>`_ and have a look at the existing test cases in the ``tests`` directory to get an idea of the structure of our test suite.
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

Checklist for releasing new versions
====================================

.. code:: markdown

    - [ ] Update submodules
    - [ ] Test GNAT Studio plugin
       - Check verification, generation and display of message graph for example apps
    - [ ] Add release to `CHANGELOG`
    - [ ] Bring changes to `main` branch
    - [ ] Add tag to git repository
       - Note: Commit IDs change when a PR is merged on GitHub, so it should be ensured that the right branch is checked out.
       - `git tag -a vX.Y.Z -m ""`
    - [ ] Generate distribution archive
       - `make dist`
    - [ ] Check distribution archive
       - Check content of archive
       - Install in new virtual environment
       - Ensure that no additional packages to `rflx` are installed
       - Test `rflx` executable
    - [ ] Push tag
       - `git push --follow-tags`
    - [ ] Upload to TestPyPI
       - `twine upload --repository-url https://test.pypi.org/legacy/ dist/RecordFlux-X.Y.Z.tar.gz`
    - [ ] Check project on TestPyPI
    - [ ] Test installation from TestPyPI
       - `pip3 install --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple/ RecordFlux`
    - [ ] Upload to PyPI
       - `twine upload dist/RecordFlux-X.Y.Z.tar.gz`
    - [ ] Test installation from PyPI
       - `pip3 install RecordFlux`
    - [ ] Publish release notes on GitHub
