.. _Specification_Language:

Specification Language
~~~~~~~~~~~~~~~~~~~~~~

.. COMMENT
   This is a multiline comment


This chapter explains the domain-specific language that is used
to define RecordFlux messages and sessions.

The specification language describes protocol message formats
based on types. For each type of the specification language
a description of its syntax and semantics and an example are given.
A simple variant of Backus-Naur Form is used to describe the syntax:

An optional item is denoted by enclosing in square brackets ``[`` ``]``.
Square brackets that are part of the RecordFlux syntax are enclosed in quote
marks: "[", "]".
A sequence of 0 or more items is denoted by enclosing braces ``{`` and ``}``.
Reserved keywords are marked in **bold**.

.. TODO:: With Sphinx markup (``.. productionlist::``), reserved words
   are in quotation marks.

A reference to a syntactic category is linked to its defining
production.
To convey semantic information, the names of some syntactic
categories are prefixed by a non-highlighted part.
Syntactic categories with prefixes are equivalent to the category
without the prefix.
The following basic elements are used to describe the syntax of
the language:

* *name*: A name consists of alphanumeric characters and underscores.
  By convention a letter at the start of a name and immediatelt after
  each underscore is capitalized (e.g., ``Mixed_Case_With_Underscores``).

* *number*: A number consists of numerical digits. As in Ada, an underscore
  can be added between two digits to improve readability (e.g., ``1_234``).

* *string*: A string literal is a sequence of graphic characters delimited by
  double quotes (e.g., "String").

* *mathematical_expression*: A mathematical expression consists of numbers
  and names combined by mathematical operators (addition ``+``, subtraction ``-``,
  multiplication ``*``, division ``/``, exponentiation ``**``).

* *boolean_expression*: A boolean expression consists of relations (``<``, ``<=``,
  ``=``, ``/=``, ``>=``, ``>``) between names and numbers combined by boolean
  operators (conjunction ``and``, disjunction ``or``).

.. TODO:: For each of the above, are the rules the same as for Ada?
   If not, what are the differences?

The type system is inspired by Ada, but differs in some details.
In contrast to Ada, integer variables are considered type compatible.
Explicit type conversions of integer variables are neither required
nor supported.

Scalar Types
============

Integer Types
-------------

An integer type is used to represent numbers. Two types of
integers are supported: modular types and range types.

.. rubric:: Syntax

.. productionlist::
   modular_type: "type" `name` "is" "mod" `modulus`
               :
   range_type: "type" `name` "is" "range" `first` .. `last`
             : "with" Size => `number`
             :
   modulus: `mathematical_expression`
   first: `mathematical_expression`
   last: `mathematical_expression`


.. rubric:: Static Semantics

A modular type represents the values from zero to one less than
the modulus. The bit size of a modular type is the binary logarithm
of modulus.

The set of values of a range type consists of all numbers from the
lower bound to the upper bound. For a range type the bit size has
to be specified explicitly.

.. rubric:: Example

.. code-block:: ada

   type Address is mod 2**48
   type Type_Length is range 46 .. 2**16 - 1 with Size => 16


Enumeration Types
-----------------

An enumeration type defines a set of names for constant values
represented as integers.

.. rubric:: syntax

.. productionlist::
   enumeration_type: "type" `name` "is" ( `literals` )
                   : "with" `enumeration_aspects`
                   :
   literals: `literal` { , `literal` }
   literal: `name` [=> `number`]
          :
   enumeration_aspects: `enumeration_aspect` { , `enumeration_aspect` }
   enumeration_aspect: `size_aspect` | `always_valid_aspect`
   always_valid_aspect: Always_Valid [ => ( True | False ) ]

.. rubric:: Static Semantics

The set of values of an enumeration type consists of the list of declared
enumeration literals. Each enumeration literal is represented by a
distinct non-negative value.
If no explicit value is given, the first literal is represented as zero,
and the value of each subsequent literal is incremented by one.
Literals with and without explicit values must not be intermixed
in one definition.
The bit size of the enumeration type must be specified explicitly.
Optionally, an enumeration type can be specified with an
``Always_Valid`` aspect.
A message field having such a type is always considered valid, whether or not
its value corresponds to one of the specified literals.

.. rubric:: Example

.. code::

   type Tag is (Msg_Error, Msg_Data) with Size => 1

   type Ether_Type is
      (ET_IPv4            => 16#0800#,
       ET_ARP             => 16#0806#,
       ET_VLAN_Tag        => 16#8100#,
       ET_IPv6            => 16#86DD#,
       ET_VLAN_Tag_Double => 16#9100#)
   with Size => 16, Always_Valid


Boolean Type
------------

``Boolean`` is a built-in enumeration type with the literals
``False`` (represented by ``0``) and ``True`` (``1``)
with a size of 1 bit.


Message Types
=============

A message type defines a collection of fields. Additional ``then`` clauses
specify conditions and dependencies between fields.

.. rubric:: Syntax

.. productionlist::
   message_type: "type" `name` [ ( `parameter` { , `parameter` } ) ] "is"
               :  ( "message"
               :    [ `null_field` ]
               :      `field`
               :    { `field` }
               :  "end" "message" [ "with"
               :     `message_aspects` ]
               : | "null" "message" )
               :
   parameter: parameter_`name` : `type_name`
            :
   type_name: `qualified_name`
            :`
   field: `field_name` : `type_name` [ ( `type_argument`
        :                        { , `type_argument` } ) ]
        :   [ "with" `aspects` ]
        :   [ "if" `condition` ]
        :   { `then_clause` } ;
        :
   type_argument: `named_argument`
                :
   null_field: "null" `then_clause` ;
             :
   then_clause: "then" field_`name`
              :   [ "with" `aspects` ]
              :   [ "if" `condition` ]
              :
   aspects: `aspect` { , `aspect` }
          :
   aspect: `first_aspect` | `size_aspect`
         :
   first_aspect: First => `mathematical_expression`
               :
   size_aspect: Size => `mathematical_expression`
              :
   condition: `boolean_expression`
            :
   message_aspects: `message_aspect` { , `message_aspect` }
                  :
   message_aspect: `checksum_aspect` | `byteorder_aspect`
                 :
   checksum_aspect: Checksum => ( `checksum_definition`
                  :             { , `checksum_definition` } )
                  :
   checksum_definition: `name` => ( `checksum_element`
                      :         { , `checksum_element` } )
                      :
   checksum_element: `name` | `name`'Size | `field_range`
                   :
   field_range: `field_range_first` .. `field_range_last`
              :
   field_range_first: `name`'First | `name`'Last + 1
                    :
   field_range_last: `name`'Last | `name`'First - 1
                   :
   byteorder_aspect: Byte_Order => `byteorder_definition`
                   :
   byteorder_definition: High_Order_First | Low_Order_First


.. rubric:: Static Semantics

A message type specifies the message format of a protocol.
A message is represented by a graph-based model.
Each node in the graph corresponds to one field in a message.
The links in the graph define the order of the fields.
A link is represented by a **then** clause in the specification.
If no **then** clause is given, it is assumed that the next field
of the message immediately follows.
If no further field follows, it is assumed that the message
ends with this field.
The end of a message can also be denoted explicitly by adding
a clause **then null**.

A **then** clause can optionally contain a condition, which
specifies when the corresponding field follows,
and/or aspects that specify the size of the next field
and the location of its first bit.
These aspects can also be specified for the field directly.
Each aspect can be specified either for the field or in all
incoming **then** clauses, but not in both.
The condition can refer to previous fields (including the field
containing the **then** clause).
A condition can also be added for the field directly.
A field condition is equivalent to adding a condition
to all incoming **then** clauses.
If a field condition as well as a condition at a **then** clause
are True, both conditions are combined by a logical conjunction.
If required, a null field can be used to specify the size of
the first field in the message.
An empty message can be represented by a null message.

A message can be parameterized.
Message parameters can be used in conditions and aspects and allow
defining message formats that depend on prior negotiation
during the session.
Only scalar types are allowed for parameters.
Unlike type discriminants in Ada,
message parameters are not represented as fields in the message.

.. _opaque_type:

The field type ``Opaque`` represents an unconstrained sequence of bytes.
The size of an opaque field can be specified explicitly by a size aspect,
and must be specified in this manner if another field can follow.
If an opaque field is the last field of a message, then its size can
alternatively be determined implicitly, based on the available space
(defined by the outer message when parsing or by the written data
when serializing). Opaque fields must be byte aligned, and the size
of a message must be a multiple of 8 bits.

Similar rules for size determination and alignment apply to sequence
fields (see :ref:`sequence_types`).

A checksum aspect specifies which parts of a message are covered by
a checksum. The definition of the checksum calculation is not part
of the specification. Code based on the message specification must
provide a function which is able to verify a checksum using the
specified checksum elements. A checksum element can be a field value,
a field size or a range of fields. The point where a checksum should
be checked during parsing or generated during serialization must be
defined for each checksum. For this purpose the ``Valid_Checksum``
attribute may be included in a condition. All message parts on which
the checksum depends have to be known at this point.

The ``Byte_Order`` aspect allows the user to specify the endianness
of the message, with the two possible choices ``High_Order_First``
(big endian, or network byte order) and ``Low_Order_First``
(little endian). If the ``Byte_Order`` aspect is not specified,
the byte order of the message is set to ``High_Order_First``.

``Message'First``, ``Message'Last`` and ``Message'Size`` can be used
in expressions to refer to the position of the first or last bit of
the message or the size of the message. All bytes which were received
when parsing or were written when serializing are considered as part
of the message.

.. rubric:: Example

.. code-block:: ada

   type Frame is
      message
         Destination : Address;
         Source : Address;
         Type_Length_TPID : Type_Length
            then TPID
               with First => Type_Length_TPID'First
               if Type_Length_TPID = 16#8100#
            then Payload
               with Size => Type_Length_TPID * 8
               if Type_Length_TPID <= 1500
            then Ether_Type
               with First => Type_Length_TPID'First
               if Type_Length_TPID >= 1536 and
                  Type_Length_TPID /= 16#8100#;
         TPID : TPID;
         TCI : TCI;
         Ether_Type : Ether_Type;
         Payload : Opaque
            then null
               if Payload'Size / 8 >= 46 and Payload'Size / 8 <= 1500;
      end message

.. code-block:: ada

   type Empty_Message is null message


Type Refinements
================

A type refinement describes the relation of an opaque field
in a message type to another message type.

.. rubric:: Syntax

.. productionlist::
   type_refinement: "for" refined_`type_name`
                  : "use" ( refined_field_`name` => message_`type_name` )
                  :    [ "if" `condition` ]
                  :
   qualified_name: `name` { :: `name` }
                 :
   condition: `boolean_expression`

.. rubric:: Static Semantics

A type refinement describes under which condition a specific message
can be expected inside of a payload field.
Only fields of type ``Opaque`` can be refined.
Types defined in other packages are referenced by a qualified name
in the form ``Package_Name::Message_Type_Name``.
The condition can refer to fields of the refined type.
To indicate that a refined field is empty (i.e. does not exit)
under a certain condition, a null message can be used as message type.

.. rubric:: Example

.. code-block:: ada

   for Ethernet::Frame use (Payload => IPv4::Packet)
      if Ether_Type = Ethernet::IPV4


Type Derivations
================

A type derivation creates a new message type based on an existing
message type.

.. rubric:: Syntax

.. productionlist::
   type_derivation: "type" `name`` "is" "new" `base_type_name`
   base_type_name: `qualified_name`

.. rubric:: Static Semantics

A derived message type derives its specification from a base type.
Type refinements of a base message type are not inherited by the
derived message type.

.. rubric:: Example

.. code-block:: ada

   type Specific_Extension is new Extension

.. _sequence_types:

Sequence Types
==============

A sequence type represents an array of similar elements.


.. rubric:: Syntax

.. productionlist::
   sequence_type: "type" `name` "is" "sequence" "of" element_`type_name`

.. rubric:: Static Semantics

A sequence consists of a number of elements of the same type.
An element type may be a scalar type or a  message type.
When a field in a message type is of a sequence type,
its bit length must be specified by a size aspect unless it is
the last field. In the latter case, the size is determined
implicitly based on the available space in its containing message,
similar to the situation with opaque fields
(see :ref:`Opaque Type <opaque_type>`).

Element ``I`` in an ``N``-element sequence ``S`` is selected
via the indexing notation ``S(I)`` where ``I`` is an integer
value.

.. rubric:: Dynamic Semantics

The indexing operation ``S(I)`` where ``S`` is a sequence of
``N`` elements and ``I`` is an integer value
raises a ``Constraint_Error`` exception if ``I`` is not
within the range ``0 .. N-1``.

.. TODO:: Verify the claim about indexing and range checks


.. rubric:: Example

.. code-block:: ada

   type Options is sequence of Option


Protocol Sessions
=================

.. TODO:: This may change, or at least should be clarified since it is
   not clear where the generic gets instandtiated

A session defines the dynamic behavior of a protocol using a finite state machine.
The external interface of a session is defined by parameters.
The initial and final state are defined by aspects.
The declaration part declares session global variables.
The main part of a session definition comprises the state definitions.

.. rubric:: Syntax

.. productionlist::
   session: "generic"
          :  { session_parameter }
          : "session" `name` "with"
          :   Initial => state_`name`,
          :   Final => state_`name`
          : "is"
          :    { `session_declaration` }
          : "begin"
          :      `state`
          :    { `state` }
          : "end" `name`
          
.. rubric:: Example

.. code-block:: ada

   generic
      X : Channel with Readable, Writable;
      type T is private;
      with function F return T;
      with function G (P : T) return Boolean;
   session S with
      Initial => A,
      Final => B
   is
      Y : Boolean := False;
   begin
      state A
         with Desc => "rfc1149.txt+51:4-52:9"
      is
         Z : Boolean := Y;
         M : TLV::Message;
      begin
         X'Read (M);
      transition
         goto B
            with Desc => "rfc1149.txt+45:4-47:8"
            if Z = True
               and G (F) = True
         goto A
      end A;
   
      state B is null state;
   end S

Session Parameters
------------------

Private types, functions and channels can be defined
as session parameters.

.. rubric:: Syntax

.. productionlist::
   session_parameter: ( `private_type_declaration` |
                    :   `function_declaration`     |
                    :   `channel_declaration` ) ;

Private Types
^^^^^^^^^^^^^

A private type parameter represents an externally defined type.

.. rubric:: Syntax

.. productionlist::
   private_type_declaration: "type" `name` "is" "private"

.. rubric:: Example

.. code-block:: ada

   type Hash is private
   
Functions
^^^^^^^^^
 
A function parameter represents externally defined code to be
invoked during the session.
 
.. rubric:: Syntax
 
.. productionlist::
   function_declaration: "with" "function" `name`
                       :   [ ( `parameter` { , `parameter` } ) ]
                       :   "return" `type`
 
.. rubric:: Static Semantics
 
Allowed parameter types:
 
* Scalars
* Definite messages
* Opaque fields of messages
 
Allowed return types:
 
* Scalars
* Definite messages
 
A *definite message* is a message with no optional fields and an explicit size
(i.e., no size aspect contains a reference to Message).

.. TODO:: Clarify the "i.e." in the above.
 
SPARK
 
For each function declaration in the session specification
a formal procedure declaration is added to the corresponding
generic session package.
The return type and parameters of a function are represented
by the first and subsequent parameters of the generated
procedure declaration.

.. rubric:: Example

.. code-block:: ada

   with function Decrypt (Key_Update_Message : Key_Update_Message;
                          Sequence_Number    : Sequence_Number;
                          Encrypted_Record   : Opaque)
                 return TLS_Inner_Plaintext
 
Channels
^^^^^^^^

Channels provide a way for communicating with other systems
using messages.

.. rubric:: Syntax

.. productionlist::
   channel_declaration: `name` : Channel 
                      :    "with" `channel_aspect` { , `channel_aspect` }
   channel_aspect: Readable | Writable
   
.. rubric:: Static Semantics

Channels can be readable, writable, or both.

.. rubric:: Example

.. code-block:: ada

   Data_Channel : Channel with Readable, Writable


Declarations
------------

Variables and renamings can be globally declared (i.e. for the scope of the complete session).

.. rubric:: Syntax

.. productionlist::
   session_declaration: ( `variable_declaration` | `renaming_declaration` ) ;


Variable Declaration
^^^^^^^^^^^^^^^^^^^^

A declared variable must have a type and can be optionally initialized using an expression.

.. rubric:: Syntax

.. productionlist::
   variable_declaration: variable_`name` : `type_name` 
                       :           [ := initialization_`expression` ]


.. rubric:: Example

.. code-block:: ada

   Error_Sent : Boolean := False

Renaming Declaration
^^^^^^^^^^^^^^^^^^^^

.. rubric:: Syntax

.. productionlist::
   renaming_declaration: `name` : message_`type_name` "renames"
                       :        message_variable_`name` . field_`name`

.. rubric:: Example

.. code-block:: ada

   Client_Hello_Message : TLS_Handshake::Client_Hello renames
                            Client_Hello_Handshake_Message.Payload

States
------

A state defines the actions to be performed and the transitions to
subsequent states.

.. rubric:: Syntax

.. productionlist::
   state: "state" `name`
        :  [ "with" `description_aspect` ]
        : "is"
        :  { `state_declaration` }
        : "begin"
        :  { `state_action` }
        : "transition"
        :  { `conditional_transition` }
        :    `transition`
        : [ "exception"
        :    `transition` ]
        : "end" `name`
        : | "state" `name` "is" "null" "state"
   description_aspect: Desc => `string`

.. rubric:: Static Semantics

An exception transition must be defined if any action might lead to
a critical (potentially non-recoverable) error:

* Insufficient memory for setting a field of a message

* Insufficient memory for appending an element to a sequence
  or extending a sequence by another sequence

Exception transitions are currently also used for other cases.
This behavior will change in the future (cf. #569).

A null state does not contain any actions or transitions.
It represents the final state of a session state machine.


.. rubric:: Dynamic Semantics

After a state is entered, its declarations and actions are executed.
If a non-recoverable error occurs, execution is aborted and the next state
is based on the exception transition.
If all actions complete normally, the conditions of the transitions
are checked in lexical order.
As soon as a condition is evaluated to ``True``, the corresponding
transition is taken to the new state.
If all conditions are ``False``, or no conditional transitions are defined,
the default transition is taken.


.. code-block:: ada

   state A
      with Desc => "rfc1149.txt+51:4-52:9"
   is
      Z : Boolean := Y;
      M : TLV::Message;
   begin
      X'Read (M);
   transition
      goto B
         with Desc => "rfc1149.txt+45:4-47:8"
         if Z = True and G (F) = True
      goto A
   end A

   state B is null state

State Declarations
^^^^^^^^^^^^^^^^^^

Variable declarations and renaming declarations in a state have a
state-local scope, i.e., local declarations cannot be accessed
from other states.

.. rubric:: Syntax

.. productionlist::
   state_declaration: ( `variable_declaration` | `renaming_declaration` ) ;
   
.. rubric:: Static Semantics

A local declaration must not hide a global declaration.


State Transitions
^^^^^^^^^^^^^^^^^

State transitions define the conditions for the change to subsequent states.
An arbitrary number of conditional transitions can be defined.
The last transition in a state definition is the default transition,
which must not contain any condition.

.. rubric:: Syntax

.. productionlist::
   conditional_transition: "transition"
                         :   "if" conditional_`expression`
   transition: "goto" state_`name`
             :  [ "with" `description_aspect` ]

.. rubric:: Example

.. code-block:: ada

   goto B
         with Desc => "rfc1149.txt+45:4-47:8"
         if Z = True and G (F) = True

..

   .. TODO:: Why not just write "if Z and G (F)"
   
State Actions
^^^^^^^^^^^^^

The state actions are executed after entering a state.

.. rubric:: Syntax

.. productionlist::
   state_action: ( assignment | append | extend
               : | reset      | read   | write ) ;

Assignment Statememts
*********************

 An assignment sets the value of a vaiable.
   
.. rubric:: Syntax

.. productionlist::
   assignment: variable_`name` := `expression` 

.. rubric Dynamic Semantics

An assignment always creates a copy of the original object.

.. rubric:: Example

.. code-block:: ada

   Error_Sent := True


Append Attribute Statements
***************************

An element is added to the end of a sequence using
the ``Append`` attribute.

.. rubric:: Syntax

.. productionlist::
   append: sequence_`name`'Append ( `expression` )

.. rubric:: Dynamic Semantics

Appending an element to a sequence may lead to an
exception transition.

.. rubric:: Example

.. code-block:: ada

   Parameter_Request_List'Extend (Parameters)
   

Reset Attribute Statements
**************************

The state of a message or sequence can be cleared
using the ``Reset`` attribute.

.. rubric:: Syntax

.. productionlist::
   reset: `name`'Reset [ ( `named_argument_list` ) ]

.. rubric:: Static Semantics

When resetting a parameterized message, the intended values
for the parameters of the message must be defined.

.. rubric:: Dynamic Semantics

The existing state of a message or sequence is removed
(and the corresponding buffer is cleared).

.. rubric:: Example

.. code-block:: ada

   Message'Reset
   

Read Attribute Statements
*************************

The read attribute statement is used to retrieve a message from a channel.

.. rubric:: Syntax

.. productionlist::
   read: channel_`name`'Read ( `expression` )
   
.. rubric:: Example

.. code-block:: ada

   Data_Channel'Read (Message)
   

Write Attribute Statements
**************************

A message can be sent through a channel using a write attribute statement.

.. rubric:: Syntax

.. productionlist::
   write: channel_`name`'Write ( `expression` )

.. rubric:: Dynamic Semantics

Writing an invalid message leads to an exception transition.
This behavior will change in the future (cf. #569).

.. rubric:: Example

.. code-block:: ada

   Data_Channel'Write (Message)


Expressions
-----------

.. rubric:: Syntax

.. productionlist::
   expression: `literal`                 | `variable`           |
             : `mathematical_expression` | `boolean_expression` |
             : `message_aggregate`       | `aggregate`          |
             : `attribute_reference`     | `selected`           |
             : `comprehension`           | `binding`            |
             : `quantified_expression`   | `call`               |
             : `conversion`
 
Literals
--------

.. rubric:: Syntax
 
.. productionlist:: 
   literal: `name` | `number`

Variables
---------

.. rubric:: Syntax

.. productionlist::
   variable: `name`

   
Message Aggregates
------------------

.. rubric:: Syntax



.. productionlist::
   message_aggregate: message_`type_name`'(
                    :  `message_aggregate_association_list`
                    : )
                    :
   message_aggregate_association_list: `named_argument_list` |
                                     : "null" "message"
                                     :
   named_argument: parameter_`name` => `expression`
                 :
   named_argument_list: `named_argument` { , `named_argument` }

.. rubric:: Dynamic Semantics

An invalid condition during message creation leads to an
exception transition.
This behavior will change in the future (cf. #569).

Insufficient memory during the message creation leads
to an exception transition.

.. rubric:: Example

.. code-block:: ada

   TLS_Record::TLS_Record'(Tag                   => TLS_Record::Alert, 
                           Legacy_Record_Version => TLS_Record::TLS_1_2,
                           Length                => Alert_Message'Size / 8,
                           Fragment              => Alert_Message'Opaque)
   
   Null_Message'(null message)


Aggregates
----------

An aggregate is a collection of elements.
Note: the square brackets, and the legality of
empty aggregates, are extensions to Ada syntax.

.. rubric:: Syntax

.. productionlist::
   aggregate: "[" `number` { , `number` } "]"

.. rubric:: Example
 
.. code::

   [0, 1, 2]
   [] 
 
 
Attribute Expressions
---------------------

.. rubric:: Syntax

.. productionlist::
   attribute_reference: `expression`'`attribute_designator`
                      :
   attribute_designator: Valid | Opaque | Head | Has_Data

.. rubric:: Static Semantics

The ``Valid`` attribute reflects whether a message or sequence is valid.

The ``Opaque`` attribute returns the byte representation of a message.

   .. TODO:: Does it return the bytes for the entire message, or just
      the ``Opaque`` field?  If the latterm what if there is more than
      one ``Opaque`` field?


The ``Head`` attribute returns the first element of a sequence.

The ``Has_Data`` attribute reflects whether a channel contains data.

.. rubric:: Dynamic Semantics

The use of the ``Opaque`` attribute on an invalid message, or the use
of the ``Head`` attribute on an empty sequence, lead to an exception
transition.
This behavior will change in the future (cf. #569).

.. rubric:: Example

.. code-block:: ada

   Message'Valid


Selected Expressions
--------------------

The ``Selected`` expression retrieves a value of a message field.

.. rubric:: Syntax

.. productionlist::
   selected: message_`expression` . field_`name`

.. rubric:: Dynamic Semantics

Accesses to message fields that were detected as invalid during
parsing lead to an exception transition.
This behavior will change in the future (cf. #569).

.. rubric:: Example

.. code-block:: ada

   Ethernet_Frame.Payload
   

.. rubric:: List Comprehensions

A list comprehension creates a new sequence based
on an exisiting sequence.

.. rubric:: Syntax

.. productionlist::
   comprehension: "[" "for" `name` "in" iterable_`expression` => 
                :    selector_`expression` "when" condition_`expression` "]"

.. rubric:: Dynamic Semantics

An access to an invalid element in iterable_expression leads
to an exception transition.
This behavior will change in the future (cf. #569).

.. rubric:: Example

.. code::

   [for O in Offer.Options 
     if O.Code = DHCP::DHCP_Message_Type_Option => 
     O.DHCP_Message_Type]
 
 
Bindings
--------

A binding names a subexpression and allows using a subexpression
multiple times without needing to duplicate the expression or
declare a separate variable.
 
.. rubric::  Syntax

.. productionlist:: 
   binding: `expression`
          :   "where"
          :     `name` = sub_`expression` { , `name` = sub_`expression` }
 
.. rubric:: Example
 
.. code-block:: ada

    TLS_Alert::Alert'(Level => Level, Description => Description)
       where
          Level = TLS_Alert::Fatal,
          Description = GreenTLS_Alert_Message.Description


Quantified Expressions
----------------------

Quantified expressions facilitate reasoning about properties of sequences.

.. rubric:: Syntax

.. productionlist::
   quantified_expression: "for" `quantifier` "in" iterable_`expression` =>
                        :   predicate_`expression`
                        :
   quantifier: "all" | "some"

.. rubric:: Example

.. code-block:: ada

   for all E in Server_Hello_Message.Extensions => 
     E.Tag /= TLS_Handshake::ET_Supported_Versions

Calls
-----

All functions declared in the session parameters can be called.

.. rubric:: Syntax

.. productionlist::
   call: `name` [ ( argument_`xpression` { , argument_`expression` } ) ]

.. rubric:: Example

.. code-block:: ada

   Decrypt (Key_Update_Message, 
            Sequence_Number, 
            TLS_Record_Message.Encrypted_Record)


Conversions
-----------

An opaque field of a message can be converted to a message.

.. rubric:: Syntax

.. productionlist::
   conversion: message_`type_name` ( message_`expression` . field_`name` )

.. rubric:: Static Semantics

A conversion is only allowed if a refinement for the message field
and the intended target type exists.

.. rubric:: Dynamic Semantics

An invalid condition of a refinement leads to an exception transition.
This behavior will change in the future (cf. #569).

.. rubric:: Example

.. code-block:: ada

   Key_Update_Message (Handshake_Control_Message.Data)
   

Packages
========

A package is used to structure a specification.

.. rubric:: Syntax

.. productionlist::
   package: "package" `name` "is"
          :  { basic_declaration }
          : "end" `name` ;
          :
   basic_declaration: ( `modular_type` | `range_type` |
                    :   `enumeration_type` | `message_type` |
                    :   `type_refinement` | `session` ) ;

.. rubric:: Static Semantics

A package is a collection of types and sessions.
By convention one protocol is specified in one package.

.. rubric:: Example

.. code:: 

   package Ethernet is
   
      type Address     is mod 2**48;
      type Type_Length is range 46 .. 2**16 - 1 with Size => 16;
      type TPID        is range 16#8100# .. 16#8100# with Size => 16;
      type TCI         is mod 2**16;
      type Ether_Type is
         (ET_IPv4            => 16#0800#,
          ET_ARP             => 16#0806#,
          ET_VLAN_Tag        => 16#8100#,
          ET_IPv6            => 16#86DD#,
          ET_VLAN_Tag_Double => 16#9100#)
      with Size => 16, Always_Valid;
   
      type Frame is
         message
            Destination      : Address;
            Source           : Address;
            Type_Length_TPID : Type_Length
               then TPID
                  with First => Type_Length_TPID'First
                  if Type_Length_TPID = 16#8100#
               then Payload
                  with Size => Type_Length_TPID * 8
                  if Type_Length_TPID <= 1500
               then Ether_Type
                  with First => Type_Length_TPID'First
                  if Type_Length_TPID >= 1536 and 
                     Type_Length_TPID /= 16#8100#;
            TPID       : TPID;
            TCI        : TCI;
            Ether_Type : Ether_Type;
            Payload     : Opaque
               then null
                  if Payload'Size / 8 >= 46 and Payload'Size / 8 <= 1500;
         end message;
   
      generic
         Input  : Channel with Readable;
         Output : Channel with Writable;
      session Validator with
         Initial => Validate,
         Final  => Error
      is
         Frame : Ethernet::Frame;
      begin
         state Validate
         is
         begin
            Input'Read (Frame);
         transition
            goto Forward
               if Frame'Valid
            goto Validate
         end Validate;
   
         state Forward
         is
         begin
            Output'Write (Frame);
         transition
            goto Validate
         exception
            goto Error
         end Forward;
   
         state Error is null state;
      end Validator;
   
   end Ethernet;

Context Clauses
===============

A context clause specifies dependences on other packages,
via a list of `with_clause`\ s

.. rubric:: Syntax

.. productionlist::
   context: { `with_clause`  }
          :
   with_clause: "with" package_`name` ;

.. rubric:: Static Semantics

Each referenced package needs to be identified in  a corresponding `with_clause` at the beginning of the file.

.. rubric:: Example

.. code-block:: ada

   with Ethernet;
   with IPv4;

   
Files
=====

A RecordFlux specification file is recognized by the file extension
:file"`.rflx`. 
Each specification file contains exactly one package.
The file name must match the package name in lower case characters.

.. rubric:: Syntax

.. productionlist::
   file: `context`
       : `package`

.. rubric:: Example

File: :file:`in_ethernet.rflx`

.. code-block:: ada

   with Ethernet;
   with IPv4;
   
   package In_Ethernet is
   
      for Ethernet::Frame use (Payload => IPv4::Packet)
         if Ether_Type = Ethernet::ET_IPv4;
   
   end In_Ethernet;

