# RecordFlux

[![Build Status](https://travis-ci.org/Componolit/RecordFlux.svg?branch=master)](https://travis-ci.org/Componolit/RecordFlux)
[![Code Coverage](https://codecov.io/github/Componolit/RecordFlux/coverage.svg?branch=master)](https://codecov.io/github/Componolit/RecordFlux)
[![Python Versions](https://img.shields.io/badge/python-3.6%20%7C%203.7-blue.svg)](https://python.org/)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)

RecordFlux is a framework for the dissection, generation and verification of communication protocols. It comprises a protocol specification language and a code generator.

## Protocol Specification Language

The RecordFlux Protocol Specification Language aims to be a closed, declarative Domain Specific Language (DSL) which allows to specify real-world binary protocols. Its syntax is inspired by [Ada](http://www.ada-auth.org/standards/12rm/html/RM-TTL.html). In the current state the specification is restricted to the description of protocol message formats. A detailed specification of the language elements can be found [here](/doc/Language-Reference.md).

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

The code generator is able to generate dissector code based on a set of protocol specifications. The generated code allows to validate and parse protocol messages and thereby respects all specified restrictions in and between protocols. Adding the generation of messages is in planning. By using the SPARK language we are able to prove the absence of run-time errors and prevent the incorrect usage of the generated code (e.g., enforce that a field of a protocol message is validated before accessed).

The code generator creates a number of packages for a protocol specification. All basic types like integers, enumerations and arrays are collectively declared in one package. For each message type a child package is generated which contains validation and access functions for each field of the message.

A user of the generated code has to validate a protocol field or the whole protocol message before accessing the data of a particular protocol field. The SPARK toolset in combination with the generated verification conditions make it possible to verify this property, and so prevent incorrect usage.

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

   procedure Initialize (Buffer : Bytes)
     with
       Post => Is_Contained (Buffer);

   function Valid_Tag (Buffer : Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   function Tag (Buffer : Bytes) return Tag_Type
     with
       Pre => (Is_Contained (Buffer) and then Valid_Tag (Buffer));

   function Valid_Value_Length (Buffer : Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   function Value_Length (Buffer : Bytes) return Length_Type
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value_Length (Buffer));

   function Valid_Value (Buffer : Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

   function Value_First (Buffer : Bytes) return Natural
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value (Buffer));

   function Value_Last (Buffer : Bytes) return Natural
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value (Buffer));

   procedure Value (Buffer : Bytes; First : out Natural; Last : out Natural)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Value (Buffer)),
       Post => (First = Value_First (Buffer) and then Last = Value_Last (Buffer));

   function Is_Valid (Buffer : Bytes) return Boolean
     with
       Pre => Is_Contained (Buffer);

end Foo.Bar;
```

The check of the `Is_Contained` predicate in the precondition of each access and validation function ensures that always the right input buffer is used. This is important if multiple protocol layers or [type refinements](/doc/Language-Reference.md#type-refinement) are used. At the lowest protocol layer the predicate has to be added manually to the input buffer by calling `Initialize`.

The generated code could be used in the following way:

```
with Foo.Bar;

procedure Main is
   Buffer : Bytes := Read;
   First  : Natural;
   Last   : Natural;
begin
   Foo.Bar.Initialize (Buffer);
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

In this example `Read` is a function which returns a byte array. After stating that `Buffer` may contain a message `Bar` of the protocol `Foo` (`Initialize`) its validity is checked (`Is_Valid`). In the case that a valid message is contained the value of `Tag` is read. Only if the value of `Tag` is `Msg_Data`, the `Value` field of the message is present and thus allowed to be accessed.

*Note: For the sake of simplicity, the code examples are limited to the parts relevant to the user, omitting implementation details.*

## Dependencies

- Python (3.6, 3.7)
- [PyParsing](https://github.com/pyparsing/pyparsing/)
- [GNAT Community 2018](https://www.adacore.com/download)

## Usage

The `rflx` tool is used to generate dissector code based on a protocol specification.

```
usage: rflx [-h] [-g] [-o DIR] FILE [FILE ...]

RecordFlux

positional arguments:
  FILE

optional arguments:
  -h, --help            show this help message and exit
  -g, --generate        generate dissector code
  -o DIR, --output DIR  output directory for dissector code
```

### Example

The default behavior of `rflx` is to just check the syntax of all given specification files.

```
rflx tests/ipv4.rflx tests/udp.rflx tests/in_ipv4.rflx
Parsing tests/ipv4.rflx... OK
Parsing tests/udp.rflx... OK
Parsing tests/in_ipv4.rflx... OK
```

By adding `-g` the dissector code will be generated. All files will be created in the current directory by default.

```
rflx -g tests/ipv4.rflx tests/udp.rflx tests/in_ipv4.rflx
Parsing tests/ipv4.rflx... OK
Parsing tests/udp.rflx... OK
Parsing tests/in_ipv4.rflx... OK
Generating... OK
Creating ipv4.ads
Creating ipv4-packet.ads
Creating ipv4-packet.adb
Creating udp.ads
Creating udp-datagram.ads
Creating udp-datagram.adb
Creating in_ipv4.ads
Creating in_ipv4.adb
```

## Licence

This software is licensed under the `AGPL-3.0`. See the `LICENSE` file for the full license text.
