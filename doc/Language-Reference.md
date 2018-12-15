# Language Reference

## Basic Types

### Integer Type

A integer type is used to represent numbers. Two types of integers are supported: modular type and range type.

#### Syntax

__type__ name __is__ __mod__ modulus

__type__ name __is__ __range__ first __..__ last

#### Static Semantics

The set of values of a modular type consists of the values from 0 to one less than the modulus. The set of values of a range type consists of all numbers from the lower bound to the upper bound.

#### Example

```
type U48 is mod 2**48;
type Length is range 1 .. 2_000;
```

### Enumeration Type

A enumeration type represents a value out of a list of possible values.

#### Syntax

__type__ name __is__ __(__ literals __) with Size =>__ size

#### Static Semantics

The set of values of an enumeration type consists of the list of declared enumeration literals. Each enumeration literal has a distinct value. If no explicit value is given, the first literal is zero, and the value of each subsequent literal is incremented by one.

#### Example

```
type Packet_Type is (Msg_Error, Msg_Data) with Size => 1;
type Day is (Mon => 1, Tue => 2, Wed => 3, Thu => 4, Fri => 5, Sat => 6, Sun => 7) with Size => 3;
```

## Message Type

A message type is a collection components. Additional then clauses allow to define conditions and dependencies between components.

#### Syntax

__type__ name __is__ __message__ components __end message__

#### Static Semantics

A message type specifies the PDU format of a protocol. Each component corresponds to one field in a message.

#### Example

```
type Frame is
   message
      Destination : U48;
      Source : U48;
      EtherType : U16
         then Payload
            with Length = EtherType * 8
            if EtherType <= 1500;
      Payload : Payload_Type;
   end message;
```

## Type Refinement

A component in a message type can be refined by another message type.

#### Syntax

__type__ name __is new__ refined_type_name __(__ refined_component_name __=>__ message_type_name __)__

#### Static Semantics

The relation between two protocols is defined by a type refinement. Usually it describes under which conditions a specific protocol message can be expected inside of a payload field.

#### Example

```
type IPv4_In_Ethernet is new Ethernet.Frame (Payload_Type => IPv4.Packet);
```

## Package

A package is a collection of types.

#### Syntax

__package__ name __is__ body __end__ name

#### Static Semantics

A package is used to structure the specification. Usually only one protocol is specified in one package.

#### Example

```
package Ethernet is

   type U16 is mod 2**16;
   type U48 is mod 2**48;

   type Frame is
      message
         Destination : U48;
         Source : U48;
         EtherType : U16
            then Payload
               with Length = EtherType * 8
               if EtherType <= 1500;
         Payload : Payload_Type;
      end message;

end Ethernet;
```

## Context Clause

The context clause is used to specify the relation to other packages and consists of a list of with clauses.

#### Syntax

__with__ package_name

#### Static Semantics

For each package referenced in the file, a corresponding with clause has to be added.

#### Example

```
with IP;
```

## File

A RecordFlux specification file can be recognized by the file extension `.rflx`. Each specification file has to contain a package with a name which corresponds to the file name.

#### Example

```
ethernet.rflx
```
