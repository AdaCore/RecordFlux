==================
Language Reference
==================

.. _highlighted:

The specification language describes protocol message formats based on types.
For each type of the specification language a description of its syntax and semantics and an example is given.
A simple variant of Backus-Naur Form is used to describe the syntax.
Reserved keywords and literals are marked in **bold**.
References to syntactic categories are :ref:`highlighted<highlighted>`.
To convey some semantic information the name of some syntactic categories are prefixed by a non-highlighted part.
Syntactic categories with prefixes are equivalent to the category without the prefix.
The following basic elements are used to describe the syntax of the language:

.. productionlist::
   name: A name consists of alphanumeric characters and underscores.
       : By convention a name starts with a capital and after each underscore
       : follows a capital as well (e.g., `Mixed_Case_With_Underscores`).
   number: A number consists of numerical digits. An underscore can be added
         : between two digits to improve readability (e.g., `1_234`).
   string: A string literal is a sequence of characters delimited by double
         : quotes (e.g., `"String"`).
   mathematical_expression : A mathematical expression consists of numbers and
                           : names combined by mathematical operators
                           : (addition "+", subtraction "-", multiplication "*",
                           : division "/", exponentiation "**").
   boolean_expression: A boolean expression consists of relations
                     : ("<", "<=", "=", "/=", ">=", ">") between names and
                     : numbers combined by boolean operators (conjunction "and",
                     : disjunction "or").

The type system is inspired by Ada, but differs in some details.
In contrast to Ada, integer variables are considered type-compatible.
Explicit type conversions of integer variables are neither required nor supported.

.. toctree::
   :maxdepth: 1

   language_reference
