..
    General

General
=======

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

..
    Types [§T]

Scalar Types
============

Integer Types
-------------

..
    Integers [§T-I]

An integer type is used to represent numbers.

**Syntax**

.. productionlist::
   integer_type: type `name` is range `first` .. `last` with Size => `number`
   first: `mathematical_expression`
   last: `mathematical_expression`

**Static Semantics**

The set of values of a integer type consists of all numbers from the lower bound to the upper bound.
The bit size has to be specified explicitly.

**Example**

.. doc-check: rflx,basic_declaration,3
.. code:: rflx

   type Type_Length is range 46 .. 2 ** 16 - 1 with Size => 16

Enumeration Types
-----------------

..
    Enumerations [§T-E]

An enumeration type represents a value out of a list of possible values.

**Syntax**

.. productionlist::
   enumeration_type: type `name` is ( `literals` ) with `enumeration_aspects`
   literals: `enumeration_literal` { , `enumeration_literal` }
   enumeration_literal: `name` [=> `number`]
   enumeration_aspects: `enumeration_aspect` { , `enumeration_aspect` }
   enumeration_aspect: `size_aspect` | `always_valid_aspect`
   always_valid_aspect: Always_Valid [ => ( True | False ) ]

**Static Semantics**

The set of values of an enumeration type consists of the list of declared enumeration literals.
Each enumeration literal has a distinct value.
If no explicit value is given, the first literal is zero, and the value of each subsequent literal is incremented by one.
Literals with and without explicit value must not be intermixed in one definition.
The bit size of the enumeration type must be specified explicitly.
Optionally, an enumeration type can be flagged as always valid.
A message field with such type is always considered valid, whether or not its value corresponds to one of the specified literals.

**Example**

.. doc-check: rflx,basic_declaration
.. code:: rflx

   type Tag is (Msg_Error, Msg_Data) with Size => 1

.. doc-check: rflx,basic_declaration
.. code:: rflx

   type Ether_Type is
      (ET_IPv4            => 16#0800#,
       ET_ARP             => 16#0806#,
       ET_VLAN_Tag        => 16#8100#,
       ET_IPv6            => 16#86DD#,
       ET_VLAN_Tag_Double => 16#9100#)
   with Size => 16, Always_Valid

Boolean
-------

..
    Booleans [§T-B]

``Boolean`` is a built-in enumeration type with the literals ``False => 0`` and ``True => 1`` with a size of 1 bit.

Message Types
=============

..
    Messages [§T-M]

A message type is a collection of fields.
Additional `then clauses <#grammar-token-then_clause>`_ enable the definition of conditions and dependencies between fields.

**Syntax**

.. productionlist::
   message_type: type `name` [ ( `parameter` { , `parameter` } ) ] is
               :  ( message
               :     [ `null_field` ]
               :       `field`
               :     { `field` }
               :    end message [ with
               :       `message_aspects` ]
               :  | null message )
   parameter: parameter_`name` : `type_name`
   type_name: `qualified_name`
   field: field_`name` : `type_name` [ ( `type_argument` { , `type_argument` } ) ]
        :  [ with `aspects` ]
        :  { `then_clause` } ;
   type_argument: `named_argument`
   null_field: null `then_clause` ;
   target_field: field_`name` | null
   then_clause: then `target_field`
              :  [ with `aspects` ]
              :  [ if `condition` ]
   aspects: `aspect` { , `aspect` }
   aspect: `first_aspect` | `size_aspect`
   first_aspect: First => `mathematical_expression`
   size_aspect: Size => `mathematical_expression`
   condition: `boolean_expression`
   message_aspects: `message_aspect` { , `message_aspect` }
   message_aspect: `checksum_aspect` | `byteorder_aspect`
   checksum_aspect: Checksum => ( `checksum_definition` { , `checksum_definition` } )
   checksum_definition: `name` => ( `checksum_element` { , `checksum_element` } )
   checksum_element: `name` | `name`'Size | `field_range`
   field_range: `field_range_first` .. `field_range_last`
   field_range_first: `name`'First | `name`'Last + 1
   field_range_last: `name`'Last | `name`'First - 1
   byteorder_aspect: Byte_Order => `byteorder_definition`
   byteorder_definition: High_Order_First | Low_Order_First

**Static Semantics**

A message type specifies the message format of a protocol.
A message is represented by a graph-based model.
Each node in the graph corresponds to one field in a message.
The links in the graph define the order of the fields.
A link is represented by a then clause in the specification.
If no then clause is given, it is assumed that always the next field of the message follows.
If no further field follows, it is assumed that the message ends with this field.
The end of a message can also be denoted explicitly by adding a then clause to *null*.
Optionally, a then clause can contain a condition under which the corresponding field follows and aspects which enable the definition of the size of the next field and the location of its first bit.
These aspects can also be specified for the field directly.
Each aspect can be specified either for the field or in all incoming then clauses, but not in both.
The condition can refer to previous fields (including the field containing the then clause).
If required, a null field can be used to specify the size of the first field in the message.
An empty message can be represented by a null message.

A message can be parameterized.
Message parameters can be used in conditions and aspects and enable the definition of message formats that depend on prior negotiation.
Only scalar types are allowed for parameters.

The field type ``Opaque`` represents an unconstrained sequence of bytes.
The size of opaque fields and sequence fields must be defined by a size aspect, if another field can follow.
If no size aspect is given, the field size is implicitly defined by the available space (defined by the outer message when parsing or by the written data when serializing).
Opaque fields and sequence fields must be byte aligned.
The size of a message must be a multiple of 8 bit.

A checksum aspect specifies which parts of a message is covered by a checksum.
The definition of the checksum calculation is not part of the specification.
Code based on the message specification must provide a function which is able to verify a checksum using the specified checksum elements.
A checksum element can be a field value, a field size or a range of fields.
The point where a checksum should be checked during parsing or generated during serialization must be defined for each checksum.
For this purpose the ``Valid_Checksum`` attribute is added to a condition.
All message parts on which the checksum depends have to be known at this point.

The ``Byte_Order`` aspect allows the user to specify the endianness of the message, with the two possible choices ``High_Order_First`` (big endian, or network byte order) and ``Low_Order_First`` (little endian).
If the ``Byte_Order`` aspect is not specified, the byte order of the message is set to ``High_Order_First``.

``Message’First``, ``Message’Last`` and ``Message’Size`` can be used in expressions to refer to the position of the first or last bit of the message or the size of the message.
All bytes which were received when parsing or were written when serializing are considered as part of the message.

**Example**

.. doc-check: rflx,basic_declaration
.. code:: rflx

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
               if Type_Length_TPID >= 1536 and Type_Length_TPID /= 16#8100#;
         TPID : TPID;
         TCI : TCI;
         Ether_Type : Ether_Type;
         Payload : Opaque
            then null
               if Payload'Size / 8 >= 46 and Payload'Size / 8 <= 1500;
      end message

.. doc-check: rflx,basic_declaration
.. code:: rflx

   type Empty_Message is null message

Type Refinements
================

..
    Type Refinements [§T-R]

A type refinement describes the relation of an opaque field in a message type to another message type.

**Syntax**

.. productionlist::
   type_refinement: for refined_`type_name` use ( refined_field_`name` => message_`type_name` )
                  :  [ if `condition` ]
   qualified_name: `name` { :: `name` }

**Static Semantics**

A type refinement describes under which condition a specific message can be expected inside of a payload field.
Only fields of type ``Opaque`` can be refined.
Types defined in other packages are referenced by a qualified name in the form ``Package_Name::Message_Type_Name``.
The condition can refer to fields of the refined type.
To indicate that a refined field is empty (i.e. does not exist) under a certain condition, a null message can be used as message type.

**Example**

.. doc-check: rflx,basic_declaration
.. code:: rflx

   for Ethernet::Frame use (Payload => IPv4::Packet)
      if Ether_Type = Ethernet::IPV4

Type Derivations
================

..
    Derived Messages [§T-D]

A type derivation enables the creation of a new message type based on an existing message type.

**Syntax**

.. productionlist::
   type_derivation: type `name` is new `base_type_name`
   base_type_name: `qualified_name`

**Static Semantics**

A derived message type derives its specification from a base type.
Type refinements of a base message type are not inherited by the derived message type.

**Example**

.. doc-check: rflx,basic_declaration
.. code:: rflx

   type Specific_Extension is new Extension

Sequence Types
==============

..
    Sequences [§T-S]

A sequence type represents a list of similar elements.

**Syntax**

.. productionlist::
   sequence_type: type `name` is sequence of element_`type_name`

**Static Semantics**

A sequence consists of a number of elements with the same type.
Scalar types as well as message types can be used as element type.

..
    Sequence of scalars [§T-S-S]
    Sequence of messages [§T-S-M]

**Example**

.. doc-check: rflx,basic_declaration
.. code:: rflx

   type Options is sequence of Option

Protocol Sessions
=================

..
    Protocol Sessions [§S]

A session defines the dynamic behavior of a protocol using a finite state machine.
The first defined state is considered the initial state.
The external interface of a session is defined by parameters.
The declaration part enables the declaration of session global variables.
The main part of a session definition are the state definitions.

**Syntax**

.. productionlist::
   session:
          : generic
          :  { `session_parameter` }
          : session `name` is
          :  { `session_declaration` }
          : begin
          :    `state`
          :  { `state` }
          : end `name`

**Example**

.. doc-check: rflx,basic_declaration
.. code:: rflx

   generic
      X : Channel with Readable, Writable;
      with function F return T;
      with function G (P : T) return Boolean;
   session S is
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
         goto null
            with Desc => "rfc1149.txt+45:4-47:8"
            if Z = True
               and G (F) = True
         goto A
      end A;
   end S

Session Parameters
------------------

..
    Session Parameters [§S-P]

Functions and channels can be defined as session parameters.

**Syntax**

.. productionlist::
   session_parameter: ( `function_declaration` | `channel_declaration` ) ;

Functions
^^^^^^^^^

..
    Functions [§S-P-F]

Functions enable the execution of externally defined code.

**Syntax**

.. productionlist::
   function_declaration: with function `name` [ ( `parameter` { , `parameter` } ) ]

**Static Semantics**

Allowed parameter types:

-  Scalars
-  Definite messages
-  Opaque fields of messages

..
    Allowed parameter types [§S-P-F-P]

    * Scalars [§S-P-F-P-S]
    * Definite messages [§S-P-F-P-M]
    * Opaque fields of messages [§S-P-F-P-O]

Allowed return types:

- Scalars
- Definite messages

..
    Allowed return types [§S-P-F-R]:

    * Scalars [§S-P-F-R-S]
    * Definite messages [§S-P-F-R-M]

Definite messages are messages with no optional fields and an explicit size (i.e. all size aspects contain no reference to ``Message``).

**SPARK**

For each function declaration in the session specification a formal procedure declaration is added to the corresponding generic session package.
The return type and parameters of a function are represented by the first and subsequent parameters of the generated procedure declaration.

**Example**

.. doc-check: rflx,session_parameter
.. code:: rflx

   with function Decrypt
      (Key_Update_Message : Key_Update_Message;
       Sequence_Number    : Sequence_Number;
       Encrypted_Record   : Opaque)
   return
      TLS_Inner_Plaintext

Channels
^^^^^^^^

..
    Channels [§S-P-C]

Channels provide a way for communicating with other systems using messages.

**Syntax**

.. productionlist::
   channel_declaration: `name` : Channel with `channel_aspect` { , `channel_aspect` }
   channel_aspect: Readable | Writable

**Static Semantics**

Channels can be readable or writable (non-exclusive).

..
    * Readable [§S-P-C-R]
    * Writable [§S-P-C-W]
    * Readable and writable [§S-P-C-RW]

**Example**

.. doc-check: rflx,session_parameter
.. code:: rflx

   Data_Channel : Channel with Readable, Writable

Declarations
------------

..
    Declarations [§S-D]

Variables and renamings can be globally declared (i.e. for the scope of the complete session).

**Syntax**

.. productionlist::
   session_declaration: ( `variable_declaration` | `renaming_declaration` ) ;

Variable Declaration
^^^^^^^^^^^^^^^^^^^^

..
    Variable Declaration [§S-D-V]

A declared variable must have a type and can be optionally initialized using an expression.

**Syntax**

.. productionlist::
   variable_declaration: variable_`name` : `type_name` [ := initialization_`expression` ]

..
    Types [§S-D-V-T]:

    * Scalar [§S-D-V-T-SC]
    * Message [§S-D-V-T-M]
    * Scalar Sequence [§S-D-V-T-SS]
    * Message Sequence [§S-D-V-T-MS]

    Initialization expressions [§S-D-V-E]:

    * No initialization [§S-D-V-E-N]
    * Mathematical Expressions [§S-D-V-E-ME]
    * Boolean Expressions [§S-D-V-E-BE]
    * Literals [§S-D-V-E-L]
    * Variables [§S-D-V-E-V]
    * Message Aggregates [§S-D-V-E-MA]
    * Aggregates [§S-D-V-E-A]
    * Valid Attributes [§S-D-V-E-VAT]
    * Opaque Attributes [§S-D-V-E-OAT]
    * Size Attributes [§S-D-V-E-SAT]
    * Head Attributes [§S-D-V-E-HAT]
    * Has_Data Attributes [§S-D-V-E-HDAT]
    * Selected Expressions [§S-D-V-E-S]
    * List Comprehensions [§S-D-V-E-LC]
    * Quantified Expressions [§S-D-V-E-Q]
    * Calls [§S-D-V-E-CL]
    * Conversions [§S-D-V-E-CV]

**Example**

.. doc-check: rflx,declaration
.. code:: rflx

   Error_Sent : Boolean := False

Renaming Declaration
^^^^^^^^^^^^^^^^^^^^

..
    Renaming Declaration [§S-D-R]

**Syntax**

.. productionlist::
   renaming_declaration: `name` : message_`type_name` renames message_variable_`name` . field_`name`

**Example**

.. doc-check: rflx,declaration
.. code:: rflx

   Client_Hello_Message : TLS_Handshake::Client_Hello renames Client_Hello_Handshake_Message.Payload

States
------

..
    States [§S-S]

A state defines the to be executed actions and the transitions to subsequent states.

**Syntax**

.. productionlist::
         state: state `name`
              :  [ with `description_aspect` ]
              : is
              :  { `state_declaration` }
              : begin
              :  { `state_action` }
              : transition
              :  { `conditional_transition` }
              :    `transition`
              :[ exception
              :     `transition` ]
              :  end `name`
         description_aspect: Desc => `string`

**Static Semantics**

..
    Exception Transition [§S-S-E]

An exception transition must be defined just in case any action might lead to a critical (potentially non-recoverable) error:

- Insufficient memory for setting a field of a message
- Insufficient memory for appending an element to a sequence or extending a sequence by another sequence

Exception transitions are currently also used for other cases.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

**Dynamic Semantics**

After entering a state the declarations and actions of the state are executed.
If a non-recoverable error occurs, the execution is aborted and the state is changed based on the exception transition.
When all action were executed successfully, the conditions of the transitions are checked in the given order.
If a condition is fulfilled, the corresponding transition is taken to change the state.
If no condition could be fulfilled or no conditional transitions were defined, the default transition is used.

**Example**

.. doc-check: rflx,state,6
.. code:: rflx

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

State Declarations
^^^^^^^^^^^^^^^^^^

..
    State Declarations [§S-S-D]
    Variable declarations [§S-S-D-V]
    Renaming declarations [§S-S-D-R]

Variable declarations and renaming declarations in a state have a state-local scope, i.e., local declarations cannot be accessed from other states.

**Syntax**

.. productionlist::
   state_declaration: ( `variable_declaration` | `renaming_declaration` ) ;

**Static Semantics**

A local declaration must not hide a global declaration.

..
    Types [§S-S-D-V-T]:

    * Scalar [§S-S-D-V-T-SC]
    * Message [§S-S-D-V-T-M]
    * Scalar Sequence [§S-S-D-V-T-SS]
    * Message Sequence [§S-S-D-V-T-MS]

    Initialization expressions [§S-S-D-V-E]:

    * No initialization [§S-S-D-V-E-N]
    * Mathematical Expressions [§S-S-D-V-E-ME]
    * Boolean Expressions [§S-S-D-V-E-BE]
    * Literals [§S-S-D-V-E-L]
    * Variables [§S-S-D-V-E-V]
    * Message Aggregates [§S-S-D-V-E-MA]
    * Aggregates [§S-S-D-V-E-A]
    * Valid Attributes [§S-S-D-V-E-VAT]
    * Opaque Attributes [§S-S-D-V-E-OAT]
    * Size Attributes [§S-S-D-V-E-SAT]
    * Head Attributes [§S-S-D-V-E-HAT]
    * Has_Data Attributes [§S-S-D-V-E-HDAT]
    * Selected Expressions [§S-S-D-V-E-S]
    * List Comprehensions [§S-S-D-V-E-LC]
    * Quantified Expressions [§S-S-D-V-E-Q]
    * Calls [§S-S-D-V-E-CL]
    * Conversions [§S-S-D-V-E-CV]

State Transitions
^^^^^^^^^^^^^^^^^

..
    State Transitions [§S-S-T]

State transitions define the conditions for the change to subsequent states.
An arbitrary number of conditional transitions can be defined.
The last transition in a state definition is the default transition, which does not contain any condition.
The transition target must be either a state name or `null`, which represents the final state.

**Syntax**

.. productionlist::
   conditional_transition: `transition`
                         :    if conditional_`expression`
   transition: goto state_`name`
             :  [ with `description_aspect` ]

..
    Condition expressions:

    * No condition [§S-S-T-N]
    * Mathematical Expressions [§S-S-T-ME]
    * Boolean Expressions [§S-S-T-BE]
    * Literals [§S-S-T-L]
    * Variables [§S-S-T-V]
    * Message Aggregates [§S-S-T-MA]
    * Aggregates [§S-S-T-A]
    * Valid Attributes [§S-S-T-VAT]
    * Opaque Attributes [§S-S-T-OAT]
    * Size Attributes [§S-S-T-SAT]
    * Head Attributes [§S-S-T-HAT]
    * Has_Data Attributes [§S-S-T-HDAT]
    * Field Valid Attributes [§S-S-T-FVAT]
    * Field Present Attributes [§S-S-T-FPAT]
    * Selected Expressions [§S-S-T-S]
    * List Comprehensions [§S-S-T-LC]
    * Quantified Expressions [§S-S-T-Q]
    * Calls [§S-S-T-CL]
    * Conversions [§S-S-T-CV]

**Example**

.. doc-check: rflx,conditional_transition,9
.. code:: rflx

   goto B
      with Desc => "rfc1149.txt+45:4-47:8"
      if Z = True and G (F) = True

State Actions
^^^^^^^^^^^^^

..
    State Actions [§S-S-A]

The state actions are executed after entering a state.

**Syntax**

.. productionlist::
   state_action: ( `assignment` | `append` | `extend` | `reset` | `read` | `write` ) ;

Assignment Statements
"""""""""""""""""""""

..
    Assignment Statements [§S-S-A-A]

An assignment sets the value of variable.

**Syntax**

.. productionlist::
   assignment: variable_`name` := `expression`

..
    Expressions:

    * Mathematical Expressions [§S-S-A-A-ME]
    * Boolean Expressions [§S-S-A-A-BE]
    * Literals [§S-S-A-A-L]
    * Variables [§S-S-A-A-V]
    * Message Aggregates [§S-S-A-A-MA]
    * Aggregates [§S-S-A-A-A]
    * Valid Attributes [§S-S-A-A-VAT]
    * Opaque Attributes [§S-S-A-A-OAT]
    * Size Attributes [§S-S-A-A-SAT]
    * Head Attributes [§S-S-A-A-HAT]
    * Has_Data Attributes [§S-S-A-A-HDAT]
    * Selected Expressions [§S-S-A-A-S]
    * List Comprehensions [§S-S-A-A-LC]
    * Quantified Expressions [§S-S-A-A-Q]
    * Calls [§S-S-A-A-CL]
    * Conversions [§S-S-A-A-CV]

**Dynamic Semantics**

An assignment always creates a copy of the original object.

**Example**

.. doc-check: rflx,assignment_statement
.. code:: rflx

   Error_Sent := True

Message Field Assignment Statements
"""""""""""""""""""""""""""""""""""

..
    Message Field Assignment Statements [§S-S-A-MFA]

A message field assignment sets the value of a message field.

**Syntax**

.. productionlist::
   message_field_assignment: variable_`name`.field_`name` := `expression`

..
    Expressions:

    * Mathematical Expressions [§S-S-A-MFA-ME]
    * Boolean Expressions [§S-S-A-MFA-BE]
    * Literals [§S-S-A-MFA-L]
    * Variables [§S-S-A-MFA-V]
    * Message Aggregates [§S-S-A-MFA-MA]
    * Aggregates [§S-S-A-MFA-A]
    * Valid Attributes [§S-S-A-MFA-VAT]
    * Opaque Attributes [§S-S-A-MFA-OAT]
    * Size Attributes [§S-S-A-MFA-SAT]
    * Head Attributes [§S-S-A-MFA-HAT]
    * Has_Data Attributes [§S-S-A-MFA-HDAT]
    * Selected Expressions [§S-S-A-MFA-S]
    * List Comprehensions [§S-S-A-MFA-LC]
    * Quantified Expressions [§S-S-A-MFA-Q]
    * Calls [§S-S-A-MFA-CL]
    * Conversions [§S-S-A-MFA-CV]

**Dynamic Semantics**

Message fields must be set in order.
Trying to set a message field which is not a valid next field leads to an exception transition.
All subsequent fields of the set message field are invalidated.

**Example**

.. doc-check: rflx,message_field_assignment_statement
.. code:: rflx

    Packet.Length := 42

Append Attribute Statements
"""""""""""""""""""""""""""

..
    Append Attribute Statements [§S-S-A-AP]

An element is added to the end of a sequence using the Append attribute.

**Syntax**

.. productionlist::
   append: sequence_`name`'Append ( `expression` )

..
    Expressions:

    * Mathematical Expressions [§S-S-A-AP-ME]
    * Boolean Expressions [§S-S-A-AP-BE]
    * Literals [§S-S-A-AP-L]
    * Variables [§S-S-A-AP-V]
    * Message Aggregates [§S-S-A-AP-MA]
    * Aggregates [§S-S-A-AP-A]
    * Valid Attributes [§S-S-A-AP-VAT]
    * Opaque Attributes [§S-S-A-AP-OAT]
    * Size Attributes [§S-S-AP-SAT]
    * Head Attributes [§S-S-A-AP-HAT]
    * Has_Data Attributes [§S-S-A-AP-HDAT]
    * Selected Expressions [§S-S-A-AP-S]
    * List Comprehensions [§S-S-A-AP-LC]
    * Quantified Expressions [§S-S-A-AP-Q]
    * Calls [§S-S-A-AP-CL]
    * Conversions [§S-S-A-AP-CV]

**Dynamic Semantics**

Appending an element to a sequence might lead to an exception transition.

**Example**

.. doc-check: rflx,attribute_statement
.. code:: rflx

   Parameter_Request_List'Append (DHCP::Domain_Name_Option)

Extend Attribute Statements
"""""""""""""""""""""""""""

..
    Extend Attribute Statements [§S-S-A-EX]

The Extend attributes adds a sequence of elements to the end of a sequence.

**Syntax**

.. productionlist::
   extend: sequence_`name`'Extend ( `expression` )

..
    Expressions:

    * Mathematical Expressions [§S-S-A-EX-ME]
    * Boolean Expressions [§S-S-A-EX-BE]
    * Literals [§S-S-A-EX-L]
    * Variables [§S-S-A-EX-V]
    * Message Aggregates [§S-S-A-EX-MA]
    * Aggregates [§S-S-A-EX-A]
    * Valid Attributes [§S-S-A-EX-VAT]
    * Opaque Attributes [§S-S-A-EX-OAT]
    * Size Attributes [§S-S-A-EX-SAT]
    * Head Attributes [§S-S-A-EX-HAT]
    * Has_Data Attributes [§S-S-A-EX-HDAT]
    * Selected Expressions [§S-S-A-EX-S]
    * List Comprehensions [§S-S-A-EX-LC]
    * Quantified Expressions [§S-S-A-EX-Q]
    * Calls [§S-S-A-EX-CL]
    * Conversions [§S-S-A-EX-CV]

**Dynamic Semantics**

Extending a sequence might lead to an exception transition.

**Example**

.. doc-check: rflx,attribute_statement
.. code:: rflx

   Parameter_Request_List'Extend (Parameters)

Reset Attribute Statements
""""""""""""""""""""""""""

..
    Reset Attribute Statements [§S-S-A-RS]

The state of a message or sequence can be cleared using the Reset attribute.

**Syntax**

.. productionlist::
   reset: `name`'Reset [ ( `named_argument_list` ) ]

..
    Expressions:

    * Mathematical Expressions [§S-S-A-RS-ME]
    * Boolean Expressions [§S-S-A-RS-BE]
    * Literals [§S-S-A-RS-L]
    * Variables [§S-S-A-RS-V]
    * Message Aggregates [§S-S-A-RS-MA]
    * Aggregates [§S-S-A-RS-A]
    * Valid Attributes [§S-S-A-RS-VAT]
    * Opaque Attributes [§S-S-A-RS-OAT]
    * Size Attributes [§S-S-A-RS-SAT]
    * Head Attributes [§S-S-A-RS-HAT]
    * Has_Data Attributes [§S-S-A-RS-HDAT]
    * Selected Expressions [§S-S-A-RS-S]
    * List Comprehensions [§S-S-A-RS-LC]
    * Quantified Expressions [§S-S-A-RS-Q]
    * Calls [§S-S-A-RS-CL]
    * Conversions [§S-S-A-RS-CV]

**Static Semantics**

When resetting a parameterized message, the intended values for the parameters of the message must be defined.

**Dynamic Semantics**

The existing state of a message or sequence is removed (and the corresponding buffer is cleared).

**Example**

.. doc-check: rflx,attribute_statement
.. code:: rflx

   Message'Reset

Read Attribute Statements
"""""""""""""""""""""""""

..
    Read Attribute Statements [§S-S-A-RD]

The read attribute statement is used to retrieve a message from a channel.

**Syntax**

.. productionlist::
   read: channel_`name`'Read ( `expression` )

..
    Expressions:

    * Mathematical Expressions [§S-S-A-RD-ME]
    * Boolean Expressions [§S-S-A-RD-BE]
    * Literals [§S-S-A-RD-L]
    * Variables [§S-S-A-RD-V]
    * Message Aggregates [§S-S-A-RD-MA]
    * Aggregates [§S-S-A-RD-A]
    * Valid Attributes [§S-S-A-RD-VAT]
    * Opaque Attributes [§S-S-A-RD-OAT]
    * Size Attributes [§S-S-A-RD-SAT]
    * Head Attributes [§S-S-A-RD-HAT]
    * Has_Data Attributes [§S-S-A-RD-HDAT]
    * Selected Expressions [§S-S-A-RD-S]
    * List Comprehensions [§S-S-A-RD-LC]
    * Quantified Expressions [§S-S-A-RD-Q]
    * Calls [§S-S-A-RD-CL]
    * Conversions [§S-S-A-RD-CV]

**Example**

.. doc-check: rflx,attribute_statement
.. code:: rflx

   Data_Channel'Read (Message)

Write Attribute Statements
""""""""""""""""""""""""""

..
    Write Attribute Statements [§S-S-A-WR]

A message can be sent through a channel using a write attribute statement.

**Syntax**

.. productionlist::
   write: channel_`name`'Write ( `expression` )

..
    Expressions:

    * Mathematical Expressions [§S-S-A-WR-ME]
    * Boolean Expressions [§S-S-A-WR-BE]
    * Literals [§S-S-A-WR-L]
    * Variables [§S-S-A-WR-V]
    * Message Aggregates [§S-S-A-WR-MA]
    * Aggregates [§S-S-A-WR-A]
    * Valid Attributes [§S-S-A-WR-VAT]
    * Opaque Attributes [§S-S-A-WR-OAT]
    * Size Attributes [§S-S-A-WR-SAT]
    * Head Attributes [§S-S-A-WR-HAT]
    * Has_Data Attributes [§S-S-A-WR-HDAT]
    * Selected Expressions [§S-S-A-WR-S]
    * List Comprehensions [§S-S-A-WR-LC]
    * Quantified Expressions [§S-S-A-WR-Q]
    * Calls [§S-S-A-WR-CL]
    * Conversions [§S-S-A-WR-CV]

**Dynamic Semantics**

Writing an invalid message leads to an exception transition.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

**Example**

.. doc-check: rflx,attribute_statement
.. code:: rflx

   Data_Channel'Write (Message)

Expressions
-----------

..
    Expressions [§S-E]

**Syntax**

.. productionlist::
   expression: `literal` | `variable` | `mathematical_expression` | `boolean_expression` | `message_aggregate` | `aggregate` | `attribute_reference` | `selected` | `comprehension` | `quantified_expression` | `call` | `conversion` | `case_expression`

Literals
^^^^^^^^

**Syntax**

.. productionlist::
   literal: `name` | `number`

Variables
^^^^^^^^^

**Syntax**

.. productionlist::
   variable: `name`

Message Aggregates
^^^^^^^^^^^^^^^^^^

**Syntax**

.. productionlist::
   message_aggregate: message_`type_name`'( `message_aggregate_association_list` )
   message_aggregate_association_list: `named_argument_list` | null message
   named_argument: parameter_`name` => `expression`
   named_argument_list: `named_argument` { , `named_argument` }

**Dynamic Semantics**

An invalid condition during message creation leads to an exception transition.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

Insufficient memory during the message creation leads to an exception transition.

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   TLS_Record::TLS_Record'(Tag                   => TLS_Record::Alert,
                           Legacy_Record_Version => TLS_Record::TLS_1_2,
                           Length                => Alert_Message'Size / 8,
                           Fragment              => Alert_Message'Opaque)

.. doc-check: rflx,extended_primary
.. code:: rflx

   Null_Message'(null message)

Aggregates
^^^^^^^^^^

..
    Aggregates [§S-E-A]

An aggregate is a collection of elements.

**Syntax**

.. productionlist::
   aggregate: [ `number` { , `number` } ]

..
    Types [§S-E-A-T]:

    * Scalar [§S-E-A-T-SC]
    * Message [§S-E-A-T-M]
    * Opaque [§S-E-A-T-O]

    Expressions [§S-E-A-E]:

    * Mathematical Expressions [§S-E-A-E-ME]
    * Boolean Expressions [§S-E-A-E-BE]
    * Literals [§S-E-A-E-L]
    * Variables [§S-E-A-E-V]
    * Message Aggregates [§S-E-A-E-MA]
    * Aggregates [§S-E-A-E-A]
    * Valid Attributes [§S-E-A-E-VAT]
    * Opaque Attributes [§S-E-A-E-OAT]
    * Size Attributes [§S-E-A-E-SAT]
    * Head Attributes [§S-E-A-E-HAT]
    * Has_Data Attributes [§S-E-A-E-HDAT]
    * Selected Expressions [§S-E-A-E-S]
    * List Comprehensions [§S-E-A-E-LC]
    * Quantified Expressions [§S-E-A-E-Q]
    * Calls [§S-E-A-E-CL]
    * Conversions [§S-E-A-E-CV]

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   [0, 1, 2]

.. doc-check: rflx,extended_primary
.. code:: rflx

   []

Attribute Expressions
^^^^^^^^^^^^^^^^^^^^^

..
    Attribute Expressions [§S-E-AT]

**Syntax**

.. productionlist::
   attribute_reference: `expression`'`attribute_designator`
   attribute_designator: Valid | Opaque | Head | Has_Data

**Static Semantics**

The Valid attribute allows to determine the validity of a message or sequence.

..
    Valid attribute [§S-E-AT-V]

    Expressions:

    * Mathematical Expressions [§S-E-AT-V-ME]
    * Boolean Expressions [§S-E-AT-V-BE]
    * Literals [§S-E-AT-V-L]
    * Variables [§S-E-AT-V-V]
    * Message Aggregates [§S-E-AT-V-MA]
    * Aggregates [§S-E-AT-V-A]
    * Valid Attributes [§S-E-AT-V-VAT]
    * Opaque Attributes [§S-E-AT-V-OAT]
    * Size Attributes [§S-E-AT-V-SAT]
    * Head Attributes [§S-E-AT-V-HAT]
    * Has_Data Attributes [§S-E-AT-V-HDAT]
    * Selected Expressions [§S-E-AT-V-S]
    * List Comprehensions [§S-E-AT-V-LC]
    * Quantified Expressions [§S-E-AT-V-Q]
    * Calls [§S-E-AT-V-CL]
    * Conversions [§S-E-AT-V-CV]

The byte representation of a message can be retrieved using the Opaque attribute.

..
    Opaque attribute [§S-E-AT-O]

    Expressions:

    * Mathematical Expressions [§S-E-AT-O-ME]
    * Boolean Expressions [§S-E-AT-O-BE]
    * Literals [§S-E-AT-O-L]
    * Variables [§S-E-AT-O-V]
    * Message Aggregates [§S-E-AT-O-MA]
    * Aggregates [§S-E-AT-O-A]
    * Valid Attributes [§S-E-AT-O-VAT]
    * Opaque Attributes [§S-E-AT-O-OAT]
    * Head Attributes [§S-E-AT-O-HAT]
    * Has_Data Attributes [§S-E-AT-O-HDAT]
    * Selected Expressions [§S-E-AT-O-S]
    * List Comprehensions [§S-E-AT-O-LC]
    * Quantified Expressions [§S-E-AT-O-Q]
    * Calls [§S-E-AT-O-CL]
    * Conversions [§S-E-AT-O-CV]

The Head attribute allows to get the first element of a sequence.

..
    Head attribute [§S-E-AT-H]

    Prefix types:

    * Scalar Sequence [§S-E-AT-H-SS]
    * Message Sequence [§S-E-AT-H-MS]

    Expressions:

    * Mathematical Expressions [§S-E-AT-H-ME]
    * Boolean Expressions [§S-E-AT-H-BE]
    * Literals [§S-E-AT-H-L]
    * Variables [§S-E-AT-H-V]
    * Message Aggregates [§S-E-AT-H-MA]
    * Aggregates [§S-E-AT-H-A]
    * Valid Attributes [§S-E-AT-H-VAT]
    * Opaque Attributes [§S-E-AT-H-OAT]
    * Size Attributes [§S-E-AT-H-SAT]
    * Head Attributes [§S-E-AT-H-HAT]
    * Has_Data Attributes [§S-E-AT-H-HDAT]
    * Selected Expressions [§S-E-AT-H-S]
    * List Comprehensions [§S-E-AT-H-LC]
    * Quantified Expressions [§S-E-AT-H-Q]
    * Calls [§S-E-AT-H-CL]
    * Conversions [§S-E-AT-H-CV]

Whether a channel contains data can be checked with the Has_Data attribute.

..
    Has_Data attribute [§S-E-AT-HD]

    Expressions:

    * Mathematical Expressions [§S-E-AT-HD-ME]
    * Boolean Expressions [§S-E-AT-HD-BE]
    * Literals [§S-E-AT-HD-L]
    * Variables [§S-E-AT-HD-V]
    * Message Aggregates [§S-E-AT-HD-MA]
    * Aggregates [§S-E-AT-HD-A]
    * Valid Attributes [§S-E-AT-HD-VAT]
    * Opaque Attributes [§S-E-AT-HD-OAT]
    * Size Attributes [§S-E-AT-HD-SAT]
    * Head Attributes [§S-E-AT-HD-HAT]
    * Has_Data Attributes [§S-E-AT-HD-HDAT]
    * Selected Expressions [§S-E-AT-HD-S]
    * List Comprehensions [§S-E-AT-HD-LC]
    * Quantified Expressions [§S-E-AT-HD-Q]
    * Calls [§S-E-AT-HD-CL]
    * Conversions [§S-E-AT-HD-CV]

**Dynamic Semantics**

The use of the Opaque attribute on an invalid message or the use of the Head attribute on an empty sequence leads to an exception transition.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

**Example**

.. doc-check: rflx,extended_suffix
.. code:: rflx

   Message'Valid

Selected Expressions
^^^^^^^^^^^^^^^^^^^^

..
    Selected Expressions [§S-E-S]

The Selected expression is used to get a value of a message field.

**Syntax**

.. productionlist::
   selected: message_`expression` . field_`name`

..
    Expressions:

    * Mathematical Expressions [§S-E-S-ME]
    * Boolean Expressions [§S-E-S-BE]
    * Literals [§S-E-S-L]
    * Variables [§S-E-S-V]
    * Message Aggregates [§S-E-S-MA]
    * Aggregates [§S-E-S-A]
    * Valid Attributes [§S-E-S-VAT]
    * Opaque Attributes [§S-E-S-OAT]
    * Size Attributes [§S-E-S-SAT]
    * Head Attributes [§S-E-S-HAT]
    * Has_Data Attributes [§S-E-S-HDAT]
    * Selected Expressions [§S-E-S-S]
    * List Comprehensions [§S-E-S-LC]
    * Quantified Expressions [§S-E-S-Q]
    * Calls [§S-E-S-CL]
    * Conversions [§S-E-S-CV]

**Dynamic Semantics**

Accesses to message fields that were detected as invalid during parsing lead to an exception transition.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

**Example**

.. doc-check: rflx,extended_suffix
.. code:: rflx

   Ethernet_Frame.Payload

List Comprehensions
^^^^^^^^^^^^^^^^^^^

..
    List Comprehensions [§S-E-LC]

A list comprehension provides a way to create a new sequence based on an exisiting sequence.

**Syntax**

.. productionlist::
   comprehension: [ for `name` in iterable_`expression` => selector_`expression` when condition_`expression` ]

..
    * Source: Scalar sequence [§S-E-LC-SSS]
    * Source: Message sequence [§S-E-LC-SMS]
    * Source: Variable [§S-E-LC-V]
    * Source: Selected [§S-E-LC-S]
    * Target: Scalar sequence [§S-E-LC-TSS]
    * Target: Message sequence [§S-E-LC-TMS]
    * Condition: Selected [§S-E-LC-CS]
    * Source sequence as target [§S-E-LC-SAT]
    * Global declarations [§S-E-LC-GD]
    * Local declarations [§S-E-LC-LD]
    * State transitions [§S-E-LC-T]
    * Assignment statements [§S-E-LC-A]

**Dynamic Semantics**

An access to an invalid element in iterable `expression <#grammar-token-expression>`_ leads to an exception transition.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   [for O in Offer.Options if O.Code = DHCP::DHCP_Message_Type_Option => O.DHCP_Message_Type]

Quantified Expressions
^^^^^^^^^^^^^^^^^^^^^^

..
    Quantified Expressions [§S-E-Q]

Quantified expressions enable reasoning about properties of sequences.

**Syntax**

.. productionlist::
   quantified_expression: for `quantifier` in iterable_`expression` => predicate_`expression`
   quantifier: all | some

..
    Iterable expressions [§S-E-Q-I]:

    * Mathematical Expressions [§S-E-Q-I-ME]
    * Boolean Expressions [§S-E-Q-I-BE]
    * Literals [§S-E-Q-I-L]
    * Variables [§S-E-Q-I-V]
    * Message Aggregates [§S-E-Q-I-MA]
    * Aggregates [§S-E-Q-I-A]
    * Valid Attributes [§S-E-Q-I-VAT]
    * Opaque Attributes [§S-E-Q-I-OAT]
    * Size Attributes [§S-E-Q-I-SAT]
    * Head Attributes [§S-E-Q-I-HAT]
    * Has_Data Attributes [§S-E-Q-I-HDAT]
    * Selected Expressions [§S-E-Q-I-S]
    * List Comprehensions [§S-E-Q-I-LC]
    * Quantified Expressions [§S-E-Q-I-Q]
    * Calls [§S-E-Q-I-CL]
    * Conversions [§S-E-Q-I-CV]

    Predicate expressions [§S-E-Q-P]:

    * Mathematical Expressions [§S-E-Q-P-ME]
    * Boolean Expressions [§S-E-Q-P-BE]
    * Literals [§S-E-Q-P-L]
    * Variables [§S-E-Q-P-V]
    * Message Aggregates [§S-E-Q-P-MA]
    * Aggregates [§S-E-Q-P-A]
    * Valid Attributes [§S-E-Q-P-VAT]
    * Opaque Attributes [§S-E-Q-P-OAT]
    * Size Attributes [§S-E-Q-P-SAT]
    * Head Attributes [§S-E-Q-P-HAT]
    * Has_Data Attributes [§S-E-Q-P-HDAT]
    * Selected Expressions [§S-E-Q-P-S]
    * List Comprehensions [§S-E-Q-P-LC]
    * Quantified Expressions [§S-E-Q-P-Q]
    * Calls [§S-E-Q-P-CL]
    * Conversions [§S-E-Q-P-CV]

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   for all E in Server_Hello_Message.Extensions => E.Tag /= TLS_Handshake::ET_Supported_Versions

Calls
^^^^^

..
    Calls [§S-E-CL]

All functions which are declared in the session parameters can be called.

**Syntax**

.. productionlist::
   call: `name` [ ( argument_`expression` { , argument_`expression` } ) ]

..
    Argument expressions:

    * No argument [§S-E-CL-N]
    * Mathematical Expressions [§S-E-CL-ME]
    * Boolean Expressions [§S-E-CL-BE]
    * Literals [§S-E-CL-L]
    * Variables [§S-E-CL-V]
    * Message Aggregates [§S-E-CL-MA]
    * Aggregates [§S-E-CL-A]
    * Valid Attributes [§S-E-CL-VAT]
    * Opaque Attributes [§S-E-CL-OAT]
    * Size Attributes [§S-E-CL-SAT]
    * Head Attributes [§S-E-CL-HAT]
    * Has_Data Attributes [§S-E-CL-HDAT]
    * Selected Expressions [§S-E-CL-S]
    * List Comprehensions [§S-E-CL-LC]
    * Quantified Expressions [§S-E-CL-Q]
    * Calls [§S-E-CL-CL]
    * Conversions [§S-E-CL-CV]

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   Decrypt (Key_Update_Message, Sequence_Number, TLS_Record_Message.Encrypted_Record)

Conversions
^^^^^^^^^^^

..
    Conversions [§S-E-CV]

An opaque field of a message can be converted to a message.

**Syntax**

.. productionlist::
   conversion: message_`type_name` ( message_`expression` . field_`name` )

**Static Semantics**

A conversion is only allowed if a refinement for the message field and the intended target type exists.

..
    Expressions:

    * Mathematical Expressions [§S-E-CV-ME]
    * Boolean Expressions [§S-E-CV-BE]
    * Literals [§S-E-CV-L]
    * Variables [§S-E-CV-V]
    * Message Aggregates [§S-E-CV-MA]
    * Aggregates [§S-E-CV-A]
    * Valid Attributes [§S-E-CV-VAT]
    * Opaque Attributes [§S-E-CV-OAT]
    * Size Attributes [§S-E-CV-SAT]
    * Head Attributes [§S-E-CV-HAT]
    * Has_Data Attributes [§S-E-CV-HDAT]
    * Selected Expressions [§S-E-CV-S]
    * List Comprehensions [§S-E-CV-LC]
    * Quantified Expressions [§S-E-CV-Q]
    * Calls [§S-E-CV-CL]
    * Conversions [§S-E-CV-CV]

**Dynamic Semantics**

An invalid condition of a refinement leads to an exception transition.
This behavior will change in the future (cf. `#569 <https://github.com/AdaCore/RecordFlux/issues/569>`_).

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   Key_Update_Message (Handshake_Control_Message.Data)

Case Expressions
^^^^^^^^^^^^^^^^

..
    Case Expressions [§S-E-CE]

A `case expression <#grammar-token-case_expression>`_ selects one of several alternative dependent `expressions <#grammar-token-expression>`_ for evaluation based on the value of a selecting `expression <#grammar-token-expression>`_.

**Syntax**

.. productionlist::
   case_expression: ( case selecting_`expression`
                  : is `case_expression_alternative` { ,
                  : `case_expression_alternative` } )
   case_expression_alternative: when `discrete_choice_list` => dependent_`expression`
   discrete_choice_list: `discrete_choice` { | `discrete_choice` }
   discrete_choice: `number` | `qualified_name`

**Static Semantics**

The type of all the dependent `expression <#grammar-token-expression>`_\ s shall be compatible to the type of the `case expression <#grammar-token-case_expression>`_.
Each value of the type of the selecting `expression <#grammar-token-expression>`_ shall be covered by a `discrete choice <#grammar-token-discrete_choice>`_.
Two distinct `discrete choices <#grammar-token-discrete_choice>`_ of a `case expression <#grammar-token-case_expression>`_ shall not cover the same value.

**Example**

.. doc-check: rflx,extended_primary
.. code:: rflx

   (case Value is
       when T::V1 | T::V2 => 2,
       when T::V3         => 4)

Packages
========

A package is used to structure a specification.

**Syntax**

.. productionlist::
   package: package `name` is
          :    { `basic_declaration` }
          : end `name` ;
   basic_declaration: ( `integer_type` | `enumeration_type` | `message_type` | `type_refinement` | `type_derivation` | `sequence_type` | `session` ) ;

**Static Semantics**

A package is a collection of types and sessions.
By convention one protocol is specified in one package.

**Example**

.. doc-check: rflx
.. code:: rflx

   package Ethernet is

      type Address is range 0 .. 2 ** 48 - 1 with Size => 48;
      type Type_Length is range 46 .. 2 ** 16 - 1 with Size => 16;
      type TPID is range 16#8100# .. 16#8100# with Size => 16;
      type TCI is range 0 .. 2 ** 16 - 1 with Size => 16;
      type Ether_Type is
         (ET_IPv4            => 16#0800#,
          ET_ARP             => 16#0806#,
          ET_VLAN_Tag        => 16#8100#,
          ET_IPv6            => 16#86DD#,
          ET_VLAN_Tag_Double => 16#9100#)
      with Size => 16, Always_Valid;

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
                  if Type_Length_TPID >= 1536 and Type_Length_TPID /= 16#8100#;
            TPID : TPID;
            TCI : TCI;
            Ether_Type : Ether_Type;
            Payload : Opaque
               then null
                  if Payload'Size / 8 >= 46 and Payload'Size / 8 <= 1500;
         end message;

      generic
         Input : Channel with Readable;
         Output : Channel with Writable;
      session Validator is
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
         end Forward;
      end Validator;

   end Ethernet;

Context Clauses
===============

The context clause is used to specify the relation to other packages and consists of a list of with clauses.

**Syntax**

.. productionlist::
   context: { with package_`name` ; }

**Static Semantics**

For each package referenced in a file, a corresponding with clause has to be added to the beginning of the file.

**Example**

.. doc-check: rflx,context_clause
.. code:: rflx

   with Ethernet;
   with IPv4;

Files
=====

A RecordFlux specification file is recognized by the file extension ``.rflx``.
Each specification file contains exactly one package.
The file name must match the package name in lower case characters.

**Syntax**

.. productionlist::
   file: `context`
       : `package`

**Example**

File: ``in_ethernet.rflx``.

.. doc-check: rflx,specification,0
.. code:: rflx

   with Ethernet;
   with IPv4;

   package In_Ethernet is

      for Ethernet::Frame use (Payload => IPv4::Packet)
         if Ether_Type = Ethernet::ET_IPv4;

   end In_Ethernet;
