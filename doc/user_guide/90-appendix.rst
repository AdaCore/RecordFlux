Appendix
========

Command Line Options
--------------------

The following command line options are available for the `rflx` command:

.. literalinclude:: 90-rflx--help.txt
   :language: none

The `rflx check` subcommand has the following options:

.. literalinclude:: 90-rflx-check--help.txt
   :language: none

The `rflx generate` subcommand has the following options:

.. literalinclude:: 90-rflx-generate--help.txt
   :language: none

The `rflx graph` subcommand has the following options:

.. literalinclude:: 90-rflx-graph--help.txt
   :language: none

The `rflx validate` subcommand has the following options:

.. literalinclude:: 90-rflx-validate--help.txt
   :language: none

The `rflx install` subcommand has the following options:

.. literalinclude:: 90-rflx-install--help.txt
   :language: none

The `rflx convert` subcommand has the following options:

.. literalinclude:: 90-rflx-convert--help.txt
   :language: none

The `rflx run_ls` subcommand has the following options:

.. literalinclude:: 90-rflx-run_ls--help.txt
   :language: none

Specification Files
-------------------

Style Checks
^^^^^^^^^^^^

By default, the style of specification files is checked.
Style checks can be disabled for individual files by adding a pragma to the first line of the file.
Besides the deactivation of specific checks, it is also possible to disable all checks by using ``all``.

**Example**

.. doc-check: rflx
.. code:: rflx

    -- style: disable = line-length, blank-lines

    package P is

    end P;

Integration Files
^^^^^^^^^^^^^^^^^

For each RecordFlux specification file with the ``.rflx`` file extension, users may provide a file with the same name but the ``.rfi`` file extension.
This is useful to specify buffer sizes for sessions.
This file is in the YAML data format.
Buffer sizes are provided in bytes.
If no such file is provided, RecordFlux uses a default buffer size of 4096 bytes.

**Integration file structure**

The following example of an integration file defines, for the session ``My_Session``, a default buffer size of 4096 bytes, a buffer size of 2048 bytes for the global variable ``My_Global_Var``, and a buffer size of 1024 bytes for the variable ``My_State_Variable`` defined in the state ``My_State``.

.. code:: yaml

   Session:
     My_Session:
       Buffer_Size:
         Default: 4096
         Global:
           My_Global_Var: 2048
         Local:
           My_State:
             My_State_Variable: 1024

Reporting Errors
----------------

Please report issues on GitHub:

https://github.com/AdaCore/RecordFlux/issues/new?labels=bug

If a tool invocation produces a bug box, please include its complete content and all input files in the report.

As an AdaCore customer, please open a ticket in GNAT Tracker.


Background
----------

More information about the theoretical background of RecordFlux can be found in our paper:

   Reiher T., Senier A., Castrillon J., Strufe T. (2020) RecordFlux: Formal Message Specification and Generation of Verifiable Binary Parsers. In: Arbab F., Jongmans SS. (eds) Formal Aspects of Component Software. FACS 2019. Lecture Notes in Computer Science, vol 12018. Springer, Cham (`paper <https://doi.org/10.1007/978-3-030-40914-2_9>`_, `preprint <https://arxiv.org/abs/1910.02146>`_)


GNU FDL
-------

.. toctree::
   gfdl
