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

The `rflx doc` subcommand has the following options:

.. literalinclude:: 90-rflx-doc--help.txt
   :language: none

Specification Files
-------------------

Style Checks
^^^^^^^^^^^^

By default, the style of specification files is checked.
Error messages about style violations have a style check identifier appended to the message in the following form "[style:<identifier>]".
For example: "[style:line-length]".
Style checks can be disabled for individual files by adding a pragma to the first line of the file.

**Example**

.. doc-check: rflx
.. code:: rflx

    -- style: disable = line-length, blank-lines

    package P is

    end P;

It is also possible to disable all style checks by using the identifier ``all``.

**Example**

.. doc-check: rflx
.. code:: rflx

    -- style: disable = all

    package P is

    end P;


Integration Files
^^^^^^^^^^^^^^^^^

For each RecordFlux specification file with the ``.rflx`` file extension, users may provide a file with the same name but the ``.rfi`` file extension.
This is useful to specify buffer sizes for state machines.
This file is in the YAML data format.
Buffer sizes are provided in bytes.
If no such file is provided, RecordFlux uses a default buffer size of 4096 bytes.

**Integration file structure**

The following example of an integration file defines, for the state machine ``My_State_Machine``, a default buffer size of 4096 bytes, a buffer size of 2048 bytes for the global variable ``My_Global_Var``, and a buffer size of 1024 bytes for the variable ``My_State_Variable`` defined in the state ``My_State``.

.. code:: yaml

   Machine:
     My_State_Machine:
       Buffer_Size:
         Default: 4096
         Global:
           My_Global_Var: 2048
         Local:
           My_State:
             My_State_Variable: 1024

External IO Buffers
-------------------

By default, all message buffers of a state machine are stored inside the state machine's ``Context`` type.
The ``Read`` and ``Write`` primitives give access to a message buffer when the state machine reaches an IO state.
An alternative is to use externally defined buffers.
This approach can save memory and prevent copy operations, but it removes some safety guarantees (see the notes at the end of the section).

External IO buffers can be enabled for a state machine by setting the ``External_IO_Buffers`` option in the integration file.

.. code:: yaml

   Machine:
     My_State_Machine:
       External_IO_Buffers: True

If external IO buffers are enabled, the buffer for all messages that are accessed in any IO state must be allocated externally and provided during the initialization of the state machine context as arguments to the ``Initialize`` procedure.
When an IO state is reached, the corresponding buffer of the accessed message can be removed from the state machine context.
The buffer must be added again before the state machine can be continued.

.. doc-check: ignore
.. code:: ada

   if Buffer_Accessible (Next_State (Ctx), B_Request) then
      Remove_Buffer (Ctx, B_Request, Request_Buffer);

      ...

      Add_Buffer (Ctx, B_Request, Request_Buffer, Written_Last (Ctx, B_Request));
   end if;

   Run (Ctx);

Before a buffer can be removed or added, it must be checked that the buffer is accessible.
The buffer is accessible if the next state of the state machine will perform either a read or write operation on the given buffer.
The function ``Buffer_Accessible`` can be used to check this condition.
If all read or write operations on a given channel are used with a single message buffer, then the function ``Needs_Data`` or ``Has_Data``, respectively, is sufficient to determine this condition.
Each external buffer is uniquely identified by an enumeration literal of the ``External_Buffer`` type.
In the example shown above, ``B_Request`` identifies the buffer of a message called ``Request``.
When adding a buffer, the index of the last written byte has to be provided.
If the buffer has not been changed, ``Written_Last`` can be used to set it to the same value as before.

**CAUTION:**
The message buffer must only be modified if the state machine expects data, i.e., ``Needs_Data`` is true.
Changing the contents of the buffer at other times may cause the state machine to behave unexpectedly.
This property cannot be guaranteed by the generated SPARK contracts.

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
