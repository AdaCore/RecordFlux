# RecordFlux

[![Build Status](https://travis-ci.org/Componolit/RecordFlux.svg?branch=master)](https://travis-ci.org/Componolit/RecordFlux)
[![Code Coverage](https://codecov.io/github/Componolit/RecordFlux/coverage.svg?branch=master)](https://codecov.io/github/Componolit/RecordFlux)
[![Python Versions](https://img.shields.io/badge/python-3.6%20%7C%203.7-blue.svg)](https://python.org/)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)

RecordFlux is a toolset for the dissection, generation and verification of communication protocols. It comprises a protocol specification language and a code generator.

## Protocol Specification Language

The RecordFlux Protocol Specification Language aims to be a closed, declarative Domain Specific Language (DSL) which allows to specify real-world binary protocols. Its syntax is inspired by [Ada](https://www.adacore.com/about-ada). At this stage, the specification is restricted to the definition of protocol message formats. A detailed description of the language elements can be found in the [Language Reference](/doc/Language-Reference.md).

### Example

The following sample specification describes a protocol `Foo` with one protocol message type `Bar`. Three fields are specified for `Bar`:

- A `Tag` field with 2 bit,
- a `Value_Length` field with 14 bit, and
- a `Value` field, whose length is specified by the value in `Value_Length`.

The `Tag` can have two valid values: `1` (`Msg_Data`) and `3` (`Msg_Error`). In the case `Tag` has a value of `1` the fields `Value_Length` and `Value` follow. `Bar` contains only one field, if the value of `Tag` is `3`. All other values of `Tag` lead to an invalid message.

```
package Foo is

   type Tag_Type is (Msg_Data => 1, Msg_Error => 3) with Size => 2;
   type Length_Type is mod 2**14;

   type Bar is
      message
         Tag          : Tag_Type
            then Value_Length
               if Tag = Msg_Data,
            then null
               if Tag = Msg_Error;
         Value_Length : Length_Type
            then Value
               with Length = Value_Length;
         Value        : Payload_Type;
       end message;

end Foo;
```

## Code Generation

The code generator is able to generate dissector code based on a set of protocol specifications. The generated code allows to validate and parse protocol messages and thereby respects all specified restrictions in and between protocols. Adding the generation of messages is in planning. By using [SPARK](https://www.adacore.com/about-spark) we are able to prove the absence of runtime errors and prevent the incorrect usage of the generated code (e.g., enforce that a field of a protocol message is validated before accessed).

The code generator creates a number of packages for a protocol specification. All basic types like integers, enumerations and arrays are collectively declared in one package. For each message type a child package is generated which contains validation and access functions for each field of the message.

A user of the generated code has to validate a protocol field or the whole protocol message before accessing the data of a particular protocol field. The SPARK toolset in combination with the generated verification conditions make it possible to ensure this property, and so prevent incorrect usage.

### Example

The file `foo.ads` contains all basic types of the package `Foo`:

```
package Foo is

   type Tag_Type is (Msg_Data, Msg_Error) with Size => 8;
   for Tag_Type use (Msg_Data => 1, Msg_Error => 255);

   type Length_Type is mod (2**8);

end Foo;
```

The file `foo-bar.ads` contains the specification of all functions related to the message `Bar`:

```
package Foo.Bar is

   procedure Label (Buffer : Types.Bytes)
     with
       Post => Is_Contained (Buffer);

   function Valid_Tag (Buffer : Types.Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   function Tag (Buffer : Types.Bytes) return Tag_Type
     with
       Pre => (Is_Contained (Buffer) and then Valid_Tag (Buffer));

   function Valid_Value_Length (Buffer : Types.Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   function Value_Length (Buffer : Types.Bytes) return Length_Type
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value_Length (Buffer));

   function Valid_Value (Buffer : Types.Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   function Value_First (Buffer : Types.Bytes) return Types.Index_Type
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value (Buffer));

   function Value_Last (Buffer : Types.Bytes) return Types.Index_Type
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value (Buffer));

   procedure Value (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value (Buffer)),
       Post => (First = Value_First (Buffer) and then Last = Value_Last (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

end Foo.Bar;
```

The check of the `Is_Contained` predicate in the precondition of each access and validation function ensures that always the right input buffer is used. This is important if multiple protocol layers or [type refinements](/doc/Language-Reference.md#type-refinement) are used. At the lowest protocol layer the predicate has to be added manually to the input buffer by calling `Label`.

The generated code could be used in the following way:

```
with Foo.Bar;

procedure Main is
   Buffer : Types.Bytes := Read;
   First  : Types.Index_Type;
   Last   : Types.Index_Type;
begin
   Foo.Bar.Label (Buffer);
   if Foo.Bar.Is_Valid (Buffer) then
      if Foo.Bar.Tag (Buffer) = Msg_Data then
         Foo.Bar.Value (Buffer, First, Last);
         --  use value in Buffer (First .. Last)
      else
         --  report error
      end if;
   end if;
end Main;
```

In this example `Read` is a function which returns a byte array. After stating that `Buffer` may contain a message `Bar` of the protocol `Foo` (`Label`) its validity is checked (`Is_Valid`). In the case that a valid message is contained the value of `Tag` is read. Only if the value of `Tag` is `Msg_Data`, the `Value` field of the message is present and thus allowed to be accessed.

*Note: For the sake of simplicity, the code examples are limited to the parts relevant to the user, omitting implementation details.*

## Dependencies

- Python (3.6, 3.7)
- [PyParsing](https://github.com/pyparsing/pyparsing/)
- [GNAT Community 2018](https://www.adacore.com/download)

## Usage

The `rflx` tool is used to verify a protocol specification and generate code based on it. Therefore it offers the two sub-commands `check` and `generate`.

### Example

With the sub-command `check` the correctness of the given specification files can be verified.

```
$ rflx check specs/ipv4.rflx specs/udp.rflx specs/in_ipv4.rflx                                                                                                                     :(
Parsing specs/ipv4.rflx... OK
Parsing specs/udp.rflx... OK
Parsing specs/in_ipv4.rflx... OK
```

The sub-command `generate` is used to generate the code based on the specification. The target directory and the specification files have to be given. By adding `-d` or `-l` the generation can be limited to the dissector code or to the internally used library files, respectively.

```
$ rflx generate -d tests specs/ipv4.rflx specs/udp.rflx specs/in_ipv4.rflx
Parsing specs/ipv4.rflx... OK
Parsing specs/udp.rflx... OK
Parsing specs/in_ipv4.rflx... OK
Generating... OK
Created tests/ipv4.ads
Created tests/ipv4-option.ads
Created tests/ipv4-option.adb
Created tests/ipv4-packet.ads
Created tests/ipv4-packet.adb
Created tests/ipv4-options.ads
Created tests/ipv4-options.adb
Created tests/udp.ads
Created tests/udp-datagram.ads
Created tests/udp-datagram.adb
Created tests/in_ipv4.ads
Created tests/in_ipv4-contains.ads
Created tests/in_ipv4-contains.adb
```

## Licence

This software is licensed under the `AGPL-3.0`. See the `LICENSE` file for the full license text.
