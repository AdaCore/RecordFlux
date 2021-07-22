# Language Reference

The specification language describes protocol message formats based on types. For each type of the specification language a description of its syntax and semantics and an example is given. A simple variant of Backus-Naur Form is used to describe the syntax. Reserved keywords and literals are marked in __bold__. Syntactic categories are highlighted by *italics*. To convey some semantic information the name of some syntactic categories are prefixed by a non-italicized part. Syntactic categories with non-italicized prefixes are equivalent to the category without the prefix. The following basic elements are used to describe the syntax of the language:

*name*: A name consists of alphanumeric characters and underscores. By convention a name starts with a capital and after each underscore follows a capital as well (e.g., Mixed_Case_With_Underscores).

*number*: A number consists of numerical digits. An underscore can be added between two digits to improve readability.

*mathematical_expression*: A mathematical expression consists of numbers and names combined by mathematical operators (addition __+__, subtraction __-__, multiplication __\*__, division __/__, exponentiation __\*\*__).

*boolean_expression*: A boolean expression consists of relations (__<__, __<=__, __=__, __/=__, __>=__, __>__) between names and numbers combined by boolean operators (conjunction __and__, disjunction __or__).

The type system is inspired by Ada, but differs in some details. In contrast to Ada, integer variables are considered type-compatible. Explicit type conversions of integer variables are neither required nor supported.

## Scalar Types

### Integer Type

An integer type is used to represent numbers. Two types of integers are supported: modular type and range type.

#### Syntax

*modular_type* ::= __type__ *name* __is__ __mod__ *modulus* __;__

*range_type* ::= __type__ *name* __is__ __range__ *first* __..__ *last* __with Size =>__ *number* __;__

*modulus* ::= *mathematical_expression*

*first* ::= *mathematical_expression*

*last* ::= *mathematical_expression*

#### Static Semantics

A modular type represents the values from 0 to one less than the *modulus*. The bit size of a modular type is determined by calculating the binary logarithm of *modulus*.

The set of values of a range type consists of all numbers from the lower bound to the upper bound. For a range type the bit size has to be specified explicitly.

#### Example

```Ada RFLX basic_declaration
type Address is mod 2**48
```

```Ada RFLX basic_declaration
type Type_Length is range 46 .. 2**16 - 1 with Size => 16
```

### Enumeration Type

An enumeration type represents a value out of a list of possible values.

#### Syntax

*enumeration_type* ::= __type__ *name* __is__ __(__ *literals* __)__ __with__ *enumeration_aspects* __;__

*literals* ::= *literal* { __,__ *literal* }

*literal* ::= *name* [__=>__ *number*]

*enumeration_aspects* ::= *enumeration_aspect* { __,__ *enumeration_aspect* }

*enumeration_aspect* ::= *size_aspect* | *always_valid_aspect*

*size_aspect* ::= __Size =>__ *number*

*always_valid_aspect* ::= __Always_Valid__ [ __=>__ ( __True__ | __False__ ) ]

#### Static Semantics

The set of values of an enumeration type consists of the list of declared enumeration literals. Each enumeration literal has a distinct value. If no explicit value is given, the first literal is zero, and the value of each subsequent literal is incremented by one. Literals with and without explicit value must not be intermixed in one definition. The bit size of the enumeration type must be specified explicitly. Optionally, an enumeration type can be flagged as always valid. A message field with such type is always considered valid, whether or not its value corresponds to one of the specified literals.

#### Example

```Ada RFLX basic_declaration
type Tag is (Msg_Error, Msg_Data) with Size => 1
```

```Ada RFLX basic_declaration
type Ether_Type is
   (ET_IPv4            => 16#0800#,
    ET_ARP             => 16#0806#,
    ET_VLAN_Tag        => 16#8100#,
    ET_IPv6            => 16#86DD#,
    ET_VLAN_Tag_Double => 16#9100#)
with Size => 16, Always_Valid
```

### Boolean

`Boolean` is a built-in enumeration type with the literals `False => 0` and `True => 1` with a size of 1 bit.

## Message Type

A message type is a collection fields. Additional then clauses enable the definition of conditions and dependencies between fields.

#### Syntax

*message_type* ::= __type__ *name* __is__ *message_definition* __;__

*message_definition* ::= __message__ [ *null_field* ] *field* { *field* } __end message__ [ __with__ *message_aspects* ] | __null message__

*field* ::= *field_name* __:__ *field_type*
             [ __with__ *aspects* ]
             [ __if__ *condition* ]
             { *then_clause* } __;__

*null_field* ::= __null__
                    *then_clause* __;__

*then_clause* ::= __then__ *field_name*
                   [ __with__ *aspects* ]
                   [ __if__ *condition* ]

*field_name* ::= *name* | __null__

*field_type* ::= *name*

*aspects* ::= *aspect* { __,__ *aspect* }

*aspect* ::= *first_aspect* | *size_aspect*

*first_aspect* ::= __First__ __=>__ *mathematical_expression*

*size_aspect* ::= __Size__ __=>__ *mathematical_expression*

*condition* ::= *boolean_expression*

*message_aspects* ::= *message_aspect* { __,__ *message_aspect* }

*message_aspect* ::= *checksum_aspect*

*checksum_aspect* ::= __Checksum__ __=>__ (*checksum_definition* { __,__ *checksum_definition* })

*checksum_definition* ::= *name* __=>__ (*checksum_element* { __,__ *checksum_element* }

*checksum_element* ::= *name* | *name* __'Size__ | *field_range*

*field_range* ::= *field_range_first* __..__ *field_range_last*

*field_range_first* ::= *name* __'First__ | *name* __'Last + 1__

*field_range_last* ::= *name* __'Last | *name* __'First - 1__

#### Static Semantics

A message type specifies the message format of a protocol. A message is represented by a graph-based model. Each node in the graph corresponds to one field in a message. The links in the graph define the order of the fields. A link is represented by a then clause in the specification. If no then clause is given, it is assumed that always the next field of the message follows. If no further field follows, it is assumed that the message ends with this field. The end of a message can also be denoted explicitly by adding a then clause to __null__. Optionally, a then clause can contain a condition under which the corresponding field follows and aspects which enable the definition of the size of the next field and the location of its first bit. These aspects can also be specified for the field directly. Each aspect can be specified either for the field or in all incoming then clauses, but not in both. The condition can refer to previous fields (including the field containing the then clause). A condition can also be added for the field directly. A field condition is equivalent to adding a condition to all then clauses. If a field condition as well as a condition at a then clause exists, both conditions are combined by a logical conjunction. If required, a null field can be used to specify the size of the first field in the message. An empty message can be represented by a null message.

The field type `Opaque` represents an unconstrained sequence of bytes. The size of opaque fields must be always defined by a size aspect. Opaque fields and sequence fields must be byte aligned. The size of a message must be a multiple of 8 bit.

A checksum aspect specifies which parts of a message is covered by a checksum. The definition of the checksum calculation is not part of the specification. Code based on the message specification must provide a function which is able to verify a checksum using the specified checksum elements. A checksum element can be a field value, a field size or a range of fields. The point where a checksum should be checked during parsing or generated during serialization must be defined for each checksum. For this purpose the `Valid_Checksum` attribute is added to a condition. All message parts on which the checksum depends have to be known at this point.

#### Example

```Ada RFLX basic_declaration
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
      Ether_Type : Ether_Type
         then Payload
            with Size => Message'Last - Ether_Type'Last;
      Payload : Opaque
         then null
            if Payload'Size / 8 >= 46 and Payload'Size / 8 <= 1500;
   end message
```

```Ada RFLX basic_declaration
type Empty_Message is null message
```

## Type Refinement

A type refinement describes the relation of an opaque field in a message type to another message type.

#### Syntax

*type_refinement* ::= __for__ *refined_type_name* __use__ __(__ *refined_field_name* __=>__ *message_type_name* __)__ [ __if__ *condition* ] __;__

*refined_type_name* ::= *qualified_name*

*refined_field_name* ::= *name*

*message_type_name* ::= *qualified_name*

*qualified_name* ::= *name* { __.__ *name* }

*condition* ::= *boolean_expression*

#### Static Semantics

A type refinement describes under which condition a specific protocol message can be expected inside of a payload field. Only fields of type `Opaque` can be refined. Types defined in other packages are referenced by a qualified name in the form package_name.message_type_name. The condition can refer to fields of the refined type. To indicate that a refined field is empty (i.e. does not exit) under a certain condition, a null message can be used as message type.

#### Example

```Ada RFLX basic_declaration
for Ethernet::Frame use (Payload => IPv4::Packet)
   if Ether_Type = Ethernet::IPV4
```

## Type Derivation

A type derivation enables the creation of a new message type based on an existing message type.

#### Syntax

*type_derivation* ::= __type__ *name* __is new__ *base_type_name*__;__

*base_type_name* ::= *qualified_name*

#### Static Semantics

A derived message type derives its specification from a base type. Type refinements of a base message type are not inherited by the derived message type.

#### Example

```Ada RFLX basic_declaration
type Specific_Extension is new Extension
```

## Sequence Type

A sequence type represents a list of similar elements.

#### Syntax

*sequence_type* ::= __type__ *name* __is sequence of__ *element_type* __;__

*element_type* ::= *name*

#### Static Semantics

A sequence consists of a number of elements with similar type. Scalar types as well as message types can be used as element type. When a sequence is used in a message type, its bit length has to be specified by a size aspect.

#### Example

```Ada RFLX basic_declaration
type Options is sequence of Option
```

## Protocol Sessions [§S]

A session defines the dynamic behavior of a protocol using a finite state machine. The external interface of a session is defined by parameters. The initial and final state is defined by aspects. The declaration part enables the declaration of session global variables. The main part of a session definition are the state definitions.

__Syntax__

*session* ::=
   __generic__
    { *session_parameter* }
   __session__ *name* __with__
      __Initial =>__ state_*name*__,__
      __Final =>__ state_*name*
   __is__
    { *session_declaration* }
   __begin__
    { *state* }
   __end__ *name*__;__

__Example__

```Ada RFLX basic_declaration
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
      then B
         with Desc => "rfc1149.txt+45:4-47:8"
         if Z = True
            and G (F) = True
      then A
   end A;

   state B is null state;
end S
```

### Session Parameters [§S-P]

Private types, functions and channels can be defined as session parameters.

__Syntax__

*session_parameter* ::= *private_type_declaration* | *function_declaration* | *channel_declaration*

#### Private Types [§S-P-P]

A private type represents an externally defined type.

__Syntax__

*private_type_declaration* ::= __type__ *name* __is private;__

__Static Semantics__

TBD

__Example__

```Ada RFLX session_parameter
type Hash is private
```

#### Functions [§S-P-F]

Functions enable the execution of externally defined code.

__Syntax__

*function_declaration* ::= __with function__ *name* [ __(__ *function_parameter* { __,__ *function_parameter* } __)__ ] __;__

*function_parameter* ::= parameter_*name* __:__ type_*name*

__Static Semantics__

Allowed parameter types [§S-P-F-P]:

- Scalars [§S-P-F-P-S]
- Definite messages [§S-P-F-P-M]
- Opaque fields of messages [§S-P-F-P-O]

Allowed return types [§S-P-F-R]:

- Scalars [§S-P-F-R-S]
- Definite messages [§S-P-F-R-M]

Definite messages are messages with no optional fields and a bounded size (i.e. all size aspects contain no reference to `Message`).

__SPARK__

For each function declaration in the session specification a formal procedure declaration is added to the corresponding generic session package. The return type and parameters of a function are represented by the first and subsequent parameters of the generated procedure declaration.

__Example__

```Ada RFLX session_parameter
with function Decrypt (Key_Update_Message : Key_Update_Message; Sequence_Number : Sequence_Number; Encrypted_Record : Opaque) return TLS_Inner_Plaintext
```

#### Channels [§S-P-C]

Channels provide a way for communicating with other systems using messages.

__Syntax__

*channel_declaration* ::= *name* __: Channel with__ *channel_aspects*__;__

*channel_aspects* ::= *channel_aspect* { __,__ *channel_aspect* }

*channel_aspect* ::= __Readable__ | __Writable__

__Static Semantics__

Properties of channels:

- Readable [§S-P-C-R]
- Writable [§S-P-C-W]
- Readable and writable [§S-P-C-RW]

__Example__

```Ada RFLX session_parameter
Data_Channel : Channel with Readable, Writable
```

### Declarations [§S-D]

Variables and renamings can be globally declared (i.e. for the scope of the complete session).

__Syntax__

*session_declaration* ::= *variable_declaration* | *renaming_declaration*

#### Variable Declaration [§S-D-V]

A declared variable must have a type and can be optionally initialized using an expression.

__Syntax__

*variable_declaration* ::= variable_*name* __:__ type_*name* [ __:=__ initialization_*expression* ] __;__

__Static Semantics__

<details>
Types [§S-D-V-T]:

- Scalar [§S-D-V-T-SC]
- Message [§S-D-V-T-M]
- Scalar Sequence [§S-D-V-T-SS]
- Message Sequence [§S-D-V-T-MS]

Initialization expressions [§S-D-V-E]:

- No initialization [§S-D-V-E-N]
- Mathematical Expressions [§S-D-V-E-ME]
- Literals [§S-D-V-E-L]
- Variables [§S-D-V-E-V]
- Message Aggregates [§S-D-V-E-MA]
- Aggregates [§S-D-V-E-A]
- Valid Attributes [§S-D-V-E-VAT]
- Opaque Attributes [§S-D-V-E-OAT]
- Head Attributes [§S-D-V-E-HAT]
- Has_Data Attributes [§S-D-V-E-HDAT]
- Selected Expressions [§S-D-V-E-S]
- List Comprehensions [§S-D-V-E-LC]
- Bindings [§S-D-V-E-B]
- Quantified Expressions [§S-D-V-E-Q]
- Calls [§S-D-V-E-CL]
- Conversions [§S-D-V-E-CV]
</details>

__Example__

```Ada RFLX declaration
Error_Sent : Boolean := False
```

#### Renaming Declaration [§S-D-R]

TBD

__Syntax__

*renaming_declaration* ::= *name* __:__ message_type_*name* __renames__ message_variable_*name* __.__ field_*name* __;__

__Static Semantics__

TBD

__Example__

```Ada RFLX declaration
Client_Hello_Message : TLS_Handshake::Client_Hello renames Client_Hello_Handshake_Message.Payload
```

### States [§S-S]

A state defines the to be executed actions and the transitions to subsequent states.

__Syntax__

*state* ::=
   __state__ *name*
    [ __with__ *description_aspect* ]
   __is__
    { *state_declaration* }
   __begin__
    { *state_action* }
   __transition__
    { *conditional_transition* }
      *transition*
 [ __exception__
      *transition* ]
   __end__ *name*__;__
 | __state__ *name* __is null state;__

*description_aspect* ::= __Desc =>__ *string*

__Static Semantics__

An § exception transition [§S-S-E] must be defined just in case any action might lead to a critical (potentially non-recoverable) error:

- Insufficient memory for setting a field of a message
- Insufficient memory for appending an element to a sequence or extending a sequence by another sequence

Exception transitions are currently also used for other cases. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

A § null state [§S-S-N] does not contain any actions or transitions, and represents the final state of a session state machine.

__Dynamic Semantics__

After entering a state the declarations and actions of the state are executed. If a non-recoverable error occurs, the execution is aborted and the state is changed based on the exception transition. When all action were executed successfully, the conditions of the transitions are checked in the given order. If a condition is fulfilled, the corresponding transition is taken to change the state. If no condition could be fulfilled or no conditional transitions were defined, the default transition is used.

__Example__

```Ada RFLX state
state A
   with Desc => "rfc1149.txt+51:4-52:9"
is
   Z : Boolean := Y;
   M : TLV::Message;
begin
   X'Read (M);
transition
   then B
      with Desc => "rfc1149.txt+45:4-47:8"
      if Z = True and G (F) = True
   then A
end A
```

```Ada RFLX state
state B is null state
```

#### State Declarations [§S-S-D]

Variable declarations [§S-S-D-V] and § renaming declarations [§S-S-D-R] in a state have a state-local scope, i.e., local declarations cannot be accessed from other states.

__Syntax__

*state_declaration* ::= *variable_declaration* | *renaming_declaration*

__Static Semantics__

<details>
A local declaration must not hide a global declaration.

Types [§S-S-D-V-T]:

- Scalar [§S-S-D-V-T-SC]
- Message [§S-S-D-V-T-M]
- Scalar Sequence [§S-S-D-V-T-SS]
- Message Sequence [§S-S-D-V-T-MS]

Initialization expressions [§S-S-D-V-E]:

- No initialization [§S-S-D-V-E-N]
- Mathematical Expressions [§S-S-D-V-E-ME]
- Literals [§S-S-D-V-E-L]
- Variables [§S-S-D-V-E-V]
- Message Aggregates [§S-S-D-V-E-MA]
- Aggregates [§S-S-D-V-E-A]
- Valid Attributes [§S-S-D-V-E-VAT]
- Opaque Attributes [§S-S-D-V-E-OAT]
- Head Attributes [§S-S-D-V-E-HAT]
- Has_Data Attributes [§S-S-D-V-E-HDAT]
- Selected Expressions [§S-S-D-V-E-S]
- List Comprehensions [§S-S-D-V-E-LC]
- Bindings [§S-S-D-V-E-B]
- Quantified Expressions [§S-S-D-V-E-Q]
- Calls [§S-S-D-V-E-CL]
- Conversions [§S-S-D-V-E-CV]
</details>

#### State Transitions [§S-S-T]

State transitions define the conditions for the change to subsequent states. An arbitrary number of conditional transitions can be defined. The last transition in a state definition is the default transition, which does not contain any condition.

__Syntax__

*conditional_transition* ::=
   *transition*
    [ __with__ *description_aspect* ]
      __if__ *expression*

*transition* ::=
   __then__ *state_name*
    [ __with__ *description_aspect* ]

__Static Semantics__

<details>
Condition expressions:

- No condition [§S-S-T-N]
- Mathematical Expressions [§S-S-T-ME]
- Literals [§S-S-T-L]
- Variables [§S-S-T-V]
- Message Aggregates [§S-S-T-MA]
- Aggregates [§S-S-T-A]
- Valid Attributes [§S-S-T-VAT]
- Opaque Attributes [§S-S-T-OAT]
- Head Attributes [§S-S-T-HAT]
- Has_Data Attributes [§S-S-T-HDAT]
- Selected Expressions [§S-S-T-S]
- List Comprehensions [§S-S-T-LC]
- Bindings [§S-S-T-B]
- Quantified Expressions [§S-S-T-Q]
- Calls [§S-S-T-CL]
- Conversions [§S-S-T-CV]
</details>

__Example__

```Ada RFLX conditional_transition
then B
   with Desc => "rfc1149.txt+45:4-47:8"
   if Z = True and G (F) = True
```

#### State Actions [§S-S-A]

The state actions are executed after entering a state.

__Syntax__

*state_action* ::= *assignment* | *append* | *extend* | *reset* | *read* | *write*

##### Assignment Statements [§S-S-A-A]

An assignment sets the value of variable.

__Syntax__

*assignment* ::= *variable_name* __:=__ *expression*__;__

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-S-A-A-ME]
- Literals [§S-S-A-A-L]
- Variables [§S-S-A-A-V]
- Message Aggregates [§S-S-A-A-MA]
- Aggregates [§S-S-A-A-A]
- Valid Attributes [§S-S-A-A-VAT]
- Opaque Attributes [§S-S-A-A-OAT]
- Head Attributes [§S-S-A-A-HAT]
- Has_Data Attributes [§S-S-A-A-HDAT]
- Selected Expressions [§S-S-A-A-S]
- List Comprehensions [§S-S-A-A-LC]
- Bindings [§S-S-A-A-B]
- Quantified Expressions [§S-S-A-A-Q]
- Calls [§S-S-A-A-CL]
- Conversions [§S-S-A-A-CV]
</details>

__Dynamic Semantics__

An assignment always creates a copy of the original object.

__Example__

```Ada RFLX assignment_statement
Error_Sent := True
```

##### Append Attribute Statements [§S-S-A-AP]

An element is added to the end of a sequence using the Append attribute.

__Syntax__

*append* ::= sequence_*name*__'Append (__*expression*__);__

__Dynamic Semantics__

Appending an element to a sequence might lead to an exception transition.

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-S-A-AP-ME]
- Literals [§S-S-A-AP-L]
- Variables [§S-S-A-AP-V]
- Message Aggregates [§S-S-A-AP-MA]
- Aggregates [§S-S-A-AP-A]
- Valid Attributes [§S-S-A-AP-VAT]
- Opaque Attributes [§S-S-A-AP-OAT]
- Head Attributes [§S-S-A-AP-HAT]
- Has_Data Attributes [§S-S-A-AP-HDAT]
- Selected Expressions [§S-S-A-AP-S]
- List Comprehensions [§S-S-A-AP-LC]
- Bindings [§S-S-A-AP-B]
- Quantified Expressions [§S-S-A-AP-Q]
- Calls [§S-S-A-AP-CL]
- Conversions [§S-S-A-AP-CV]
</details>

__Example__

```Ada RFLX attribute_statement
Parameter_Request_List'Append (DHCP::Domain_Name_Option)
```

##### Extend Attribute Statements [§S-S-A-EX]

The Extend attributes adds a sequence of elements to the end of a sequence.

__Syntax__

*extend* ::= sequence_*name*__'Extend (__*expression*__);__

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-S-A-EX-ME]
- Literals [§S-S-A-EX-L]
- Variables [§S-S-A-EX-V]
- Message Aggregates [§S-S-A-EX-MA]
- Aggregates [§S-S-A-EX-A]
- Valid Attributes [§S-S-A-EX-VAT]
- Opaque Attributes [§S-S-A-EX-OAT]
- Head Attributes [§S-S-A-EX-HAT]
- Has_Data Attributes [§S-S-A-EX-HDAT]
- Selected Expressions [§S-S-A-EX-S]
- List Comprehensions [§S-S-A-EX-LC]
- Bindings [§S-S-A-EX-B]
- Quantified Expressions [§S-S-A-EX-Q]
- Calls [§S-S-A-EX-CL]
- Conversions [§S-S-A-EX-CV]
</details>

__Dynamic Semantics__

Extending a sequence might lead to an exception transition.

__Example__

```Ada RFLX attribute_statement
Parameter_Request_List'Extend (Parameters)
```

##### Reset Attribute Statements [§S-S-A-RS]

The state of a message or sequence can be cleared using the Reset attribute.

__Syntax__

*reset* ::= message_*name*__'Reset (__*expression*__);__

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-S-A-RS-ME]
- Literals [§S-S-A-RS-L]
- Variables [§S-S-A-RS-V]
- Message Aggregates [§S-S-A-RS-MA]
- Aggregates [§S-S-A-RS-A]
- Valid Attributes [§S-S-A-RS-VAT]
- Opaque Attributes [§S-S-A-RS-OAT]
- Head Attributes [§S-S-A-RS-HAT]
- Has_Data Attributes [§S-S-A-RS-HDAT]
- Selected Expressions [§S-S-A-RS-S]
- List Comprehensions [§S-S-A-RS-LC]
- Bindings [§S-S-A-RS-B]
- Quantified Expressions [§S-S-A-RS-Q]
- Calls [§S-S-A-RS-CL]
- Conversions [§S-S-A-RS-CV]
</details>

__Dynamic Semantics__

The existing state of a message or sequence is removed (and the corresponding buffer is cleared).

__Example__

```Ada RFLX attribute_statement
Message'Reset
```

##### Read Attribute Statements [§S-S-A-RD]

The read attribute statement is used to retrieve a message from a channel.

__Syntax__

*read* ::= channel_*name*__'Read (__*expression*__);__

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-S-A-RD-ME]
- Literals [§S-S-A-RD-L]
- Variables [§S-S-A-RD-V]
- Message Aggregates [§S-S-A-RD-MA]
- Aggregates [§S-S-A-RD-A]
- Valid Attributes [§S-S-A-RD-VAT]
- Opaque Attributes [§S-S-A-RD-OAT]
- Head Attributes [§S-S-A-RD-HAT]
- Has_Data Attributes [§S-S-A-RD-HDAT]
- Selected Expressions [§S-S-A-RD-S]
- List Comprehensions [§S-S-A-RD-LC]
- Bindings [§S-S-A-RD-B]
- Quantified Expressions [§S-S-A-RD-Q]
- Calls [§S-S-A-RD-CL]
- Conversions [§S-S-A-RD-CV]
</details>

__Example__

```Ada RFLX attribute_statement
Data_Channel'Read (Message)
```

##### Write Attribute Statements [§S-S-A-WR]

A message can be sent through a channel using a write attribute statement.

__Syntax__

*write* ::= channel_*name*__'Write (__*expression*__);__

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-S-A-WR-ME]
- Literals [§S-S-A-WR-L]
- Variables [§S-S-A-WR-V]
- Message Aggregates [§S-S-A-WR-MA]
- Aggregates [§S-S-A-WR-A]
- Valid Attributes [§S-S-A-WR-VAT]
- Opaque Attributes [§S-S-A-WR-OAT]
- Head Attributes [§S-S-A-WR-HAT]
- Has_Data Attributes [§S-S-A-WR-HDAT]
- Selected Expressions [§S-S-A-WR-S]
- List Comprehensions [§S-S-A-WR-LC]
- Bindings [§S-S-A-WR-B]
- Quantified Expressions [§S-S-A-WR-Q]
- Calls [§S-S-A-WR-CL]
- Conversions [§S-S-A-WR-CV]
</details>

__Dynamic Semantics__

Writing an invalid message leads to an exception transition. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

__Example__

```Ada RFLX attribute_statement
Data_Channel'Write (Message)
```

### Expressions [§S-E]

__Syntax__

*expression* ::= *literal* | *variable* | *mathematical_expression* | *message_aggregate* | *aggregate* | *attribute_reference* | *selected* | *comprehension* | *binding* | *quantified_expression* | *call* | *conversion*

#### Literals

__Syntax__

*literal* ::= *name*

#### Variables

__Syntax__

*variable* ::= *name*

#### Message Aggregates

__Syntax__

*message_aggregate* ::= message_type_*name*__'(__*field_values*__);__

*field_values* ::= *field_value* { __,__ *field_value* } | __null message__

*field_value* ::= *field_name* __=>__ *expression*

__Dynamic Semantics__

An invalid condition during message creation leads to an exception transition. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

Insufficient memory during the message creation leads to an exception transition.

__Example__

```Ada RFLX extended_primary
TLS_Record::TLS_Record'(Tag => TLS_Record::Alert, Legacy_Record_Version => TLS_Record::TLS_1_2, Length => Alert_Message'Size / 8, Fragment => Alert_Message'Opaque)
```

```Ada RFLX extended_primary
Null_Message'(null message)
```

#### Aggregates [§S-E-A]

__Syntax__

*aggregate* ::= __[__ [ *expression* { __,__ *expression* } ] __]__

__Static Semantics__

<details>
All elements must be of the same type.

Types [§S-E-A-T]:

- Scalar [§S-E-A-T-SC]
- Message [§S-E-A-T-M]

Expressions [§S-E-A-E]:

- Mathematical Expressions [§S-E-A-E-ME]
- Literals [§S-E-A-E-L]
- Variables [§S-E-A-E-V]
- Message Aggregates [§S-E-A-E-MA]
- Aggregates [§S-E-A-E-A]
- Valid Attributes [§S-E-A-E-VAT]
- Opaque Attributes [§S-E-A-E-OAT]
- Head Attributes [§S-E-A-E-HAT]
- Has_Data Attributes [§S-E-A-E-HDAT]
- Selected Expressions [§S-E-A-E-S]
- List Comprehensions [§S-E-A-E-LC]
- Bindings [§S-E-A-E-B]
- Quantified Expressions [§S-E-A-E-Q]
- Calls [§S-E-A-E-CL]
- Conversions [§S-E-A-E-CV]
</details>

__Example__

```Ada RFLX extended_primary
[0, 1, 2]
```

```Ada RFLX extended_primary
[]
```

#### Attribute Expressions [§S-E-AT]

__Syntax__

*attribute_reference* ::= *expression*__'__*attribute_designator*

*attribute_designator* ::= __Valid__ | __Opaque__ | __Head__ | __Has_Data__

__Static Semantics__

The § Valid attribute [§S-E-AT-V] allows to determine the validity of a message or sequence.

<details>
Expressions:

- Mathematical Expressions [§S-E-AT-V-ME]
- Literals [§S-E-AT-V-L]
- Variables [§S-E-AT-V-V]
- Message Aggregates [§S-E-AT-V-MA]
- Aggregates [§S-E-AT-V-A]
- Valid Attributes [§S-E-AT-V-VAT]
- Opaque Attributes [§S-E-AT-V-OAT]
- Head Attributes [§S-E-AT-V-HAT]
- Has_Data Attributes [§S-E-AT-V-HDAT]
- Selected Expressions [§S-E-AT-V-S]
- List Comprehensions [§S-E-AT-V-LC]
- Bindings [§S-E-AT-V-B]
- Quantified Expressions [§S-E-AT-V-Q]
- Calls [§S-E-AT-V-CL]
- Conversions [§S-E-AT-V-CV]
</details>

The byte representation of a message can be retrieved using the § Opaque attribute [§S-E-AT-O].

<details>
Expressions:

- Mathematical Expressions [§S-E-AT-O-ME]
- Literals [§S-E-AT-O-L]
- Variables [§S-E-AT-O-V]
- Message Aggregates [§S-E-AT-O-MA]
- Aggregates [§S-E-AT-O-A]
- Valid Attributes [§S-E-AT-O-VAT]
- Opaque Attributes [§S-E-AT-O-OAT]
- Head Attributes [§S-E-AT-O-HAT]
- Has_Data Attributes [§S-E-AT-O-HDAT]
- Selected Expressions [§S-E-AT-O-S]
- List Comprehensions [§S-E-AT-O-LC]
- Bindings [§S-E-AT-O-B]
- Quantified Expressions [§S-E-AT-O-Q]
- Calls [§S-E-AT-O-CL]
- Conversions [§S-E-AT-O-CV]
</details>

The § Head attribute [§S-E-AT-H] allows to get the first element of a sequence.

<details>
Prefix types:

- Scalar Sequence [§S-E-AT-H-SS]
- Message Sequence [§S-E-AT-H-MS]

Expressions:

- Mathematical Expressions [§S-E-AT-H-ME]
- Literals [§S-E-AT-H-L]
- Variables [§S-E-AT-H-V]
- Message Aggregates [§S-E-AT-H-MA]
- Aggregates [§S-E-AT-H-A]
- Valid Attributes [§S-E-AT-H-VAT]
- Opaque Attributes [§S-E-AT-H-OAT]
- Head Attributes [§S-E-AT-H-HAT]
- Has_Data Attributes [§S-E-AT-H-HDAT]
- Selected Expressions [§S-E-AT-H-S]
- List Comprehensions [§S-E-AT-H-LC]
- Bindings [§S-E-AT-H-B]
- Quantified Expressions [§S-E-AT-H-Q]
- Calls [§S-E-AT-H-CL]
- Conversions [§S-E-AT-H-CV]
</details>

Whether a channel contains data can be checked with the § Has_Data attribute [§S-E-AT-HD].

<details>
Expressions:

- Mathematical Expressions [§S-E-AT-HD-ME]
- Literals [§S-E-AT-HD-L]
- Variables [§S-E-AT-HD-V]
- Message Aggregates [§S-E-AT-HD-MA]
- Aggregates [§S-E-AT-HD-A]
- Valid Attributes [§S-E-AT-HD-VAT]
- Opaque Attributes [§S-E-AT-HD-OAT]
- Head Attributes [§S-E-AT-HD-HAT]
- Has_Data Attributes [§S-E-AT-HD-HDAT]
- Selected Expressions [§S-E-AT-HD-S]
- List Comprehensions [§S-E-AT-HD-LC]
- Bindings [§S-E-AT-HD-B]
- Quantified Expressions [§S-E-AT-HD-Q]
- Calls [§S-E-AT-HD-CL]
- Conversions [§S-E-AT-HD-CV]
</details>

__Dynamic Semantics__

The use of the Opaque attribute on an invalid message or the use of the Head attribute on an empty sequence leads to an exception transition. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

__Example__

```Ada RFLX extended_suffix
Message'Valid
```

#### Selected Expressions [§S-E-S]

The Selected expression is used to get a value of a message field.

__Syntax__

*selected* ::= message_*expression* __.__ field_*name*

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-E-S-ME]
- Literals [§S-E-S-L]
- Variables [§S-E-S-V]
- Message Aggregates [§S-E-S-MA]
- Aggregates [§S-E-S-A]
- Valid Attributes [§S-E-S-VAT]
- Opaque Attributes [§S-E-S-OAT]
- Head Attributes [§S-E-S-HAT]
- Has_Data Attributes [§S-E-S-HDAT]
- Selected Expressions [§S-E-S-S]
- List Comprehensions [§S-E-S-LC]
- Bindings [§S-E-S-B]
- Quantified Expressions [§S-E-S-Q]
- Calls [§S-E-S-CL]
- Conversions [§S-E-S-CV]
</details>

__Dynamic Semantics__

An access to an invalid message field leads to an exception transition. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

__Example__

```Ada RFLX extended_suffix
Ethernet_Frame.Payload
```

#### List Comprehensions [§S-E-LC]

A list comprehension provides a way to create a new sequence based on an exisiting sequence.

__Syntax__

*comprehension* ::= __[for__ *name* __in__ iterable_*expression* __=>__ selector_*expression* __when__ condition_*expression* __]__

__Static Semantics__

<details>
- Source: Scalar sequence [§S-E-LC-SSS]
- Source: Message sequence [§S-E-LC-SMS]
- Source: Variable [§S-E-LC-V]
- Source: Selected [§S-E-LC-S]
- Target: Scalar sequence [§S-E-LC-TSS]
- Target: Message sequence [§S-E-LC-TMS]
- Condition: Selected [§S-E-LC-CS]
- Source sequence as target [§S-E-LC-SAT]
- Global declarations [§S-E-LC-GD]
- Local declarations [§S-E-LC-LD]
- State transitions [§S-E-LC-T]
- Assignment statements [§S-E-LC-A]
</details>

__Dynamic Semantics__

An access to an invalid element in iterable_*expression* leads to an exception transition. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

__Example__

```Ada RFLX extended_primary
[for O in Offer.Options => O.DHCP_Message_Type when O.Code = DHCP::DHCP_Message_Type_Option]
```

#### Bindings [§S-E-B]

A binding can be used to name a subexpression and enables the use of a subexpression multiple times without the need for duplicating the expression or declaring a separate variable.

__Syntax__

*binding* ::= *expression* __where__ *name* __=__ sub_*expression* { __,__  *name* __=__ sub_*expression* }

__Static Semantics__

<details>
Expressions:

- Mathematical Expressions [§S-E-B-ME]
- Literals [§S-E-B-L]
- Variables [§S-E-B-V]
- Message Aggregates [§S-E-B-MA]
- Aggregates [§S-E-B-A]
- Valid Attributes [§S-E-B-VAT]
- Opaque Attributes [§S-E-B-OAT]
- Head Attributes [§S-E-B-HAT]
- Has_Data Attributes [§S-E-B-HDAT]
- Selected Expressions [§S-E-B-S]
- List Comprehensions [§S-E-B-LC]
- Bindings [§S-E-B-B]
- Quantified Expressions [§S-E-B-Q]
- Calls [§S-E-B-CL]
- Conversions [§S-E-B-CV]

For subexpressions the same semantics apply as for assignments.
</details>

__Static Semantics__

The type of the subexpression is inferred by the subexpression type and the expected type for all references of the name.

__Example__

```Ada RFLX extended_suffix
TLS_Alert::Alert'(Level => Level, Description => Description)
   where
      Level = TLS_Alert::Fatal,
      Description = GreenTLS_Alert_Message.Description
```

#### Quantified Expressions [§S-E-Q]

Quantified expressions enable reasoning about properties of sequences.

__Syntax__

*quantified_expression* ::= __for__ *quantifier* __in__ iterable_*expression* __=>__ predicate_*expression*

*quantifier* ::= __all__ | __some__

__Static Semantics__

<details>
Iterable expressions [§S-E-Q-I]:

- Mathematical Expressions [§S-E-Q-I-ME]
- Literals [§S-E-Q-I-L]
- Variables [§S-E-Q-I-V]
- Message Aggregates [§S-E-Q-I-MA]
- Aggregates [§S-E-Q-I-A]
- Valid Attributes [§S-E-Q-I-VAT]
- Opaque Attributes [§S-E-Q-I-OAT]
- Head Attributes [§S-E-Q-I-HAT]
- Has_Data Attributes [§S-E-Q-I-HDAT]
- Selected Expressions [§S-E-Q-I-S]
- List Comprehensions [§S-E-Q-I-LC]
- Bindings [§S-E-Q-I-B]
- Quantified Expressions [§S-E-Q-I-Q]
- Calls [§S-E-Q-I-CL]
- Conversions [§S-E-Q-I-CV]

Predicate expressions [§S-E-Q-P]:

- Mathematical Expressions [§S-E-Q-P-ME]
- Literals [§S-E-Q-P-L]
- Variables [§S-E-Q-P-V]
- Message Aggregates [§S-E-Q-P-MA]
- Aggregates [§S-E-Q-P-A]
- Valid Attributes [§S-E-Q-P-VAT]
- Opaque Attributes [§S-E-Q-P-OAT]
- Head Attributes [§S-E-Q-P-HAT]
- Has_Data Attributes [§S-E-Q-P-HDAT]
- Selected Expressions [§S-E-Q-P-S]
- List Comprehensions [§S-E-Q-P-LC]
- Bindings [§S-E-Q-P-B]
- Quantified Expressions [§S-E-Q-P-Q]
- Calls [§S-E-Q-P-CL]
- Conversions [§S-E-Q-P-CV]
</details>

__Example__

```Ada RFLX extended_primary
for all E in Server_Hello_Message.Extensions => E.Tag /= TLS_Handshake::ET_Supported_Versions
```

#### Calls [§S-E-CL]

All functions which are declared in the session parameters can be called.

__Syntax__

*call* ::= *name* [ __(__argument_*expression* { __,__ argument_*expression* } __)__ ]

__Static Semantics__

<details>
Argument expressions:

- No argument [§S-E-CL-N]
- Mathematical Expressions [§S-E-CL-ME]
- Literals [§S-E-CL-L]
- Variables [§S-E-CL-V]
- Message Aggregates [§S-E-CL-MA]
- Aggregates [§S-E-CL-A]
- Valid Attributes [§S-E-CL-VAT]
- Opaque Attributes [§S-E-CL-OAT]
- Head Attributes [§S-E-CL-HAT]
- Has_Data Attributes [§S-E-CL-HDAT]
- Selected Expressions [§S-E-CL-S]
- List Comprehensions [§S-E-CL-LC]
- Bindings [§S-E-CL-B]
- Quantified Expressions [§S-E-CL-Q]
- Calls [§S-E-CL-CL]
- Conversions [§S-E-CL-CV]
</details>

__Example__

```Ada RFLX extended_primary
Decrypt (Key_Update_Message, Sequence_Number, TLS_Record_Message.Encrypted_Record)
```

#### Conversions [§S-E-CV]

An opaque field of a message can be converted to a message.

__Syntax__

*conversion* ::= message_type_*name* __(__message_*expressions* __.__ field_*name*__)__

__Static Semantics__

<details>
A conversion is only allowed if a refinement for the message field and the intended target type exists.

Expressions:

- Mathematical Expressions [§S-E-CV-ME]
- Literals [§S-E-CV-L]
- Variables [§S-E-CV-V]
- Message Aggregates [§S-E-CV-MA]
- Aggregates [§S-E-CV-A]
- Valid Attributes [§S-E-CV-VAT]
- Opaque Attributes [§S-E-CV-OAT]
- Head Attributes [§S-E-CV-HAT]
- Has_Data Attributes [§S-E-CV-HDAT]
- Selected Expressions [§S-E-CV-S]
- List Comprehensions [§S-E-CV-LC]
- Bindings [§S-E-CV-B]
- Quantified Expressions [§S-E-CV-Q]
- Calls [§S-E-CV-CL]
- Conversions [§S-E-CV-CV]
</details>

__Dynamic Semantics__

An invalid condition of a refinement leads to an exception transition. This behavior will change in the future (cf. [#569](https://github.com/Componolit/RecordFlux/issues/569)).

__Example__

```Ada RFLX extended_primary
Key_Update_Message (Handshake_Control_Message.Data)
```

## Package

A package is used to structure a specification.

#### Syntax

*package* ::= __package__ *name* __is__ *body* __end__ *name* __;__

*body* := { *modular_type* | *range_type* | *enumeration_type* | *message_type* | *type_refinement* | *session* }

#### Static Semantics

A package is a collection of types and sessions. By convention one protocol is specified in one package.

#### Example

```Ada RFLX
package Ethernet is

   type Address is mod 2**48;
   type Type_Length is range 46 .. 2**16 - 1 with Size => 16;
   type TPID is range 16#8100# .. 16#8100# with Size => 16;
   type TCI is mod 2**16;
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
         Ether_Type : Ether_Type
            then Payload
               with Size => Message'Last - Ether_Type'Last;
         Payload : Opaque
            then null
               if Payload'Size / 8 >= 46 and Payload'Size / 8 <= 1500;
      end message;

end Ethernet;
```

## Context Clause

The context clause is used to specify the relation to other packages and consists of a list of with clauses.

#### Syntax

*context* ::= { __with__ *package_name* __;__ }

*package_name* ::= *name*

#### Static Semantics

For each package referenced in a file, a corresponding with clause has to be added to the beginning of the file.

#### Example

```Ada RFLX context_clause
with Ethernet;
with IPv4;
```

## File

A RecordFlux specification file is recognized by the file extension `.rflx`. Each specification file contains exactly one package. The file name must match the package name in lower case characters.

#### Syntax

*file* ::= *context* *package*

#### Example

File: `in_ethernet.rflx`

```Ada RFLX specification
with Ethernet;
with IPv4;

package In_Ethernet is

   for Ethernet::Frame use (Payload => IPv4::Packet)
      if Ether_Type = Ethernet::ET_IPv4;

end In_Ethernet;
```
