.. _Installation:

Installation
~~~~~~~~~~~~

This chapter explains how to install RecordFlux.


Prerequisites
=============

RecordFlux requires the following tools and components:

- For compiling the generated code, one of the following versions of GNAT is required:
   - `GNAT Community <https://www.adacore.com/download>`_ 2020 or 2021
   - `GNAT Pro <https://www.adacore.com/gnatpro>`_ 20.2, 21.2 or 22.0
   - `FSF GNAT <https://www.gnu.org/software/gnat/>`_ 11.2 |GNAT Alire Crate|
- `GNATcoll iconv binding <https://github.com/AdaCore/gnatcoll-bindings/tree/master/iconv>`_ |GNATcoll iconv binding Alire Crate| must be installed separately if FSF GNAT is used.
- For the formal verification of the generated code, the SPARK toolset provided by `GNAT Community <https://www.adacore.com/download>`_ 2021 is required.
- `GMP <https://gmplib.org/>`_ is provided as a package for various distributions, e.g., ``libgmp-dev`` (Debian/Ubuntu), ``gmp-devel`` (Fedora) or ``gmp`` (Arch Linux).
- Optional: `Graphviz <https://graphviz.org/>`_ is required for plotting graphs. It is provided as package ``graphviz`` for various distributions.

RecordFlux can be installed from PyPI:

.. code:: console

   $ pip3 install RecordFlux

By default the following dependencies are installed:

- `attrs <https://github.com/python-attrs/attrs>`_
- `icontract <https://github.com/Parquery/icontract>`_
- `PyDotPlus <https://github.com/carlos-jenkins/pydotplus>`_
- `Z3 <https://github.com/Z3Prover/z3>`_
- `RecordFlux parser <https://github.com/Componolit/RecordFlux-language>`_

Optionally, the GNAT Studio IDE integration for RecordFlux can be installed.
This will enable syntax highlighting for RecordFlux specifications and allow running RecordFlux from within GNAT Studio.
After installing RecordFlux do:

.. code:: console

   $ rflx setup_ide

.. |GNAT Alire Crate| image:: https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/gnat_native.json
   :target: https://alire.ada.dev/crates/gnat_native.html
.. |GNATcoll iconv binding Alire Crate| image:: https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/gnatcoll_iconv.json
   :target: https://alire.ada.dev/crates/gnatcoll_iconv.html

Unix Platforms
==============

.. TODO:: Details for using rflx on Unix

Windows Platforms
=================

.. TODO:: Details for using rflx on Windows

