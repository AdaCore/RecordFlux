.. _About_This_Guide:

About This Guide
~~~~~~~~~~~~~~~~

This guide describes the use of RecordFlux,
a toolsuite for specifying binary messages used in
communication protocols and producing formally verifiable
parsers and message generators. You specify messages in a
domain-specfic language with an Ada-like syntax, and the
parsers and generators are expressed in the SPARK language:
an Ada subset that facilitates formal proof of program
properties such as absence of run-time errors. 


What This Guide Contains
========================

This guide contains the following chapters:

* :ref:`Getting_Started_with_RecordFlux` describes ....


Appendices cover several additional topics:

* :ref:`Syntax_Reference` describes ...



What You Should Know before Reading This Guide
==============================================

.. index:: Ada 2012 Language Reference Manual

A basic knowledge of Ada 2012 and its formally analyzable
SPARK subset is assumed.
If you are not already familiar with these languages,
a useful resource is
the interactive training site https://learn.adacore.com\ .


Related Information
===================


Reference material for these languages is available online:

* :title:`Ada 2012 Reference Manual` <<(NEED LINK>>.

* <<TBD>>

The GNAT Pro development environments for Ada and the SPARK Pro toolsuite
are commercially available from AdaCore.
Plase see https://www.adacore.com or contact info@adacore.com for details.


Conventions
===========
.. index:: Conventions, typographical

.. index:: Typographical conventions

Following are examples of the typographical and graphic conventions used
in this guide:

* ``Functions``, ``utility program names``, ``standard names``,
  and ``classes``.

* ``Option flags``

* :file:`File names`

* ``Variables``

* *Emphasis*

* ``[`` optional information or parameters ``]``

* Examples are described by text

  ::

    and then shown this way.

* Commands that are entered by the user are shown as preceded by a prompt string
  comprising the ``$`` character followed by a space.

* Full file names are shown with the ``/`` character
  as the directory separator; e.g., :file:`parent-dir/subdir/myfile.rflx`.
  If you are using RecordFlux on a Windows platform, please note that
  the ``\`` character should be used instead.
