# RecordFlux

[![Build Status](https://github.com/Componolit/RecordFlux/workflows/CI/badge.svg)](https://github.com/Componolit/RecordFlux/actions)
[![Code Coverage](https://codecov.io/github/Componolit/RecordFlux/coverage.svg?branch=master)](https://codecov.io/github/Componolit/RecordFlux)
[![Python Versions](https://img.shields.io/badge/python-3.6%20%7C%203.7-blue.svg)](https://python.org/)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)

RecordFlux is a toolset for the formal specification of messages and the generation of verifiable binary parsers and message generators.

## Message Specification Language

The RecordFlux Message Specification Language is a domain-specific language to formally specify message formats of existing real-world binary protocols. Its syntax is inspired by [Ada](https://www.adacore.com/about-ada). A detailed description of the language elements can be found in the [Language Reference](/doc/Language-Reference.md).

## Model Verification

Message specifications are automatically verified using the [Z3 theorem prover](https://github.com/Z3Prover/z3). The following invariants are proven at the specification level:

* Field conditions are mutually exclusive
* Field conditions do not contradict each other
* Each field is reachable on at least one path from the initial node
* Message fields are always located after the first message bit
* Field length is never negative
* Message fields cover all bits of a message on all paths
* Overlaid fields are congruent with exactly one other field

## Code Generation

The code generator generates message parsers and generators based on message specifications. The generated parser allows to validate and dissect messages and thereby respects all specified restrictions between message fields and related messages. The generated message generator enables the creation of messages according to the message specification. By using [SPARK](https://www.adacore.com/about-spark) we are able to prove the absence of runtime errors and prevent the incorrect usage of the generated code (e.g., enforce that a field of a message is validated before accessed).

The code generator creates a number of packages for a specification. All basic types like integers, enumerations and arrays are collectively declared in one package. For each message a child package is generated which contains validation, accessor and setter functions for every field of the message.

A user of the generated code has to validate a message field or the whole message before accessing the data of a particular message field. The SPARK verification tools in combination with the generated contracts make it possible to ensure this property, and so prevent incorrect usage.

## Usage

The `rflx` tool is used to verify a specification and generate code based on it. It offers the two sub-commands `check` and `generate` for this purpose.

## Example

In the following, the complete process of specifying a message, generating code, and using the generated code is demonstrated using a small example.

### Specification

The following sample specification describes a protocol `TLV` with one message type `Message` consisting of three fields:

- A field `Tag` of 2 bit length,
- a field `Value_Length` of 14 bit length, and
- a field `Value`, whose length is specified by the value in `Value_Length`.

The `Tag` can have two valid values: `1` (`Msg_Data`) and `3` (`Msg_Error`). In case `Tag` has a value of `1` the fields `Value_Length` and `Value` follow. `Message` contains only the `Tag` field, if the value of `Tag` is `3`. All other values of `Tag` lead to an invalid message.

The structure of messages is often non-linear because of optional fields. For this reason the specification uses a graph-based representation. The order of fields is defined by then clauses. Then clauses are also used to state conditions and aspects of the following field. A more detailed description can be found in the [Language Reference](doc/Language-Reference.md#message-type).

```
package TLV is

   type Tag is (Msg_Data => 1, Msg_Error => 3) with Size => 2;
   type Length is mod 2**14;

   type Message is
      message
         Tag    : Tag
            then Length
               if Tag = Msg_Data,
            then null
               if Tag = Msg_Error;
         Length : Length
            then Value
               with Length => Length * 8;
         Value  : Payload;
       end message;

end TLV;
```

### Generating Code

With the sub-command `check` the correctness of the given specification file can be checked.

```
$ rflx check specs/tlv.rflx
Parsing specs/tlv.rflx... OK
```

The sub-command `generate` is used to generate the code based on the specification. The target directory and the specification files have to be given.

```
$ rflx generate -d generated specs/tlv.rflx
Parsing specs/tlv.rflx... OK
Generating... OK
Created generated/rflx-tlv.ads
Created generated/rflx-tlv-generic_message.ads
Created generated/rflx-tlv-generic_message.adb
Created generated/rflx-tlv-message.ads
Created generated/rflx.ads
Created generated/rflx-lemmas.ads
Created generated/rflx-lemmas.adb
Created generated/rflx-types.ads
Created generated/rflx-types.adb
Created generated/rflx-message_sequence.ads
Created generated/rflx-message_sequence.adb
Created generated/rflx-scalar_sequence.ads
Created generated/rflx-scalar_sequence.adb
```

### Use of Generated Code

All scalar types defined in the specification are represented by a similar Ada type in the generated code. For `TLV` the following types are defined in the package `RFLX.TLV`:

- `type Tag is (Msg_Data, Msg_Error) with Size => 2`
- `for Tag use (Msg_Data => 1, Msg_Error => 3);`
- `type Length is mod 2**14`

All types and subprograms related to `Message` can be found in the package `RFLX.TLV.Message`:

- `type Context`
    - Stores buffer and internal state
- `function Create return Context`
    - Return default initialized context
- `procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr)`
    - Initialize context with buffer
- `procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index_Type)`
    - Initialize context with buffer and explicit bounds
- `procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr)`
    - Get buffer and remove it from context (note: buffer cannot put back into context, thus further verification of message is not possible after this action)
- `function Has_Buffer (Ctx : Context) return Boolean`
    - Check if context contains buffer (i.e. non-null pointer)
- `procedure Verify (Ctx : in out Context; Fld : Field)`
    - Verify validity of field
- `procedure Verify_Message (Ctx : in out Context)`
    - Verify all fields of message
- `function Structural_Valid (Ctx : Context; Fld : Field) return Boolean`
    - Check if composite field is structural valid (i.e. location and length of field is correct, but content is not necessarily valid)
- `function Present (Ctx : Context; Fld : Field) return Boolean`
    - Check if composite field is structural valid and has non-zero length
- `function Valid (Ctx : Context; Fld : Field) return Boolean`
    - Check if field is valid (i.e. it has valid structure and valid content)
- `function Incomplete (Ctx : Context; Fld : Field) return Boolean`
    - Check if buffer was too short to verify field
- `function Structural_Valid_Message (Ctx : Context) return Boolean`
    - Check if all fields of message are at least structural valid
- `function Valid_Message (Ctx : Context) return Boolean`
    - Check if all fields of message are valid
- `function Incomplete_Message (Ctx : Context) return Boolean`
    - Check if buffer was too short to verify message
- `function Get_Tag (Ctx : Context) return Tag_Type`
    - Get value of `Tag` field
- `function Get_Length (Ctx : Context) return Length_Type`
    - Get value of `Length` field
- `generic with procedure Process_Value (Value : RFLX.Types.Bytes); procedure Get_Value (Ctx : Context)`
    - Access content of `Value` field
- `function Valid_Next (Ctx : Context; Fld : Field) return Boolean`
    - Check if field is potential next field
- `procedure Set_Tag (Ctx : in out Context; Value : Tag)`
    - Set value of `Tag` field
- `procedure Set_Length (Ctx : in out Context; Value : Length)`
    - Set value of `Length` field
- `generic with procedure Process_Payload (Payload : out RFLX.Types.Bytes); procedure Set_Value (Ctx : in out Context)`
    - Set content of `Value` field
- `procedure Initialize_Value (Ctx : in out Context)`
    - Initialize `Value` field (precondition to switch context for generating contained message)

A simple program to parse a `TLV.Message` could be as follows:

```
with Ada.Text_IO;
with RFLX.Types;
with RFLX.TLV.Message;

procedure Main is
   Buffer  : RFLX.Types.Bytes_Ptr := new RFLX.Types.Bytes'(64, 4, 0, 0, 0, 0);
   Context : RFLX.TLV.Message.Context := RFLX.TLV.Message.Create;
begin
   RFLX.TLV.Message.Initialize (Context, Buffer);
   RFLX.TLV.Message.Verify_Message (Context);
   if RFLX.TLV.Message.Structural_Valid_Message (Context) then
      case RFLX.TLV.Message.Get_Tag (Context) is
         when RFLX.TLV.Msg_Data =>
            if RFLX.TLV.Message.Present (Context, RFLX.TLV.Message.F_Value) then
               Ada.Text_IO.Put_Line ("Data message with value of"
                                     & RFLX.TLV.Message.Get_Length (Context)'Img
                                     & " byte length");
            else
               Ada.Text_IO.Put_Line ("Data message without value");
            end if;
         when RFLX.TLV.Msg_Error =>
            Ada.Text_IO.Put_Line ("Error message");
      end case;
   else
      Ada.Text_IO.Put_Line ("Invalid message");
   end if;
end Main;
```

In case that a valid message is contained in `Buffer` the value of `Tag` is read. If the value of `Tag` is `Msg_Data` and the `Value` field is present, the content of `Value` can be accessed.

A `TLV.Message` can be generated as follows:

```
with Ada.Text_IO;
with RFLX.Types; use type RFLX.Types.Length, RFLX.Types.Bytes;
with RFLX.TLV.Message;

procedure Main is
   Buffer  : RFLX.Types.Bytes_Ptr := new RFLX.Types.Bytes'(0, 0, 0, 0, 0, 0);
   Context : RFLX.TLV.Message.Context := RFLX.TLV.Message.Create;
   Data : RFLX.Types.Bytes (RFLX.Types.Index'First .. RFLX.Types.Index'First + 2**14);

   procedure Write_Data (Buffer : out RFLX.Types.Bytes) is
   begin
      Buffer := Data (Data'First .. Data'First + Buffer'Length - 1);
   end Write_Data;

   procedure Set_Value is new RFLX.TLV.Message.Set_Value (Write_Data);
begin
   --  Generating message
   RFLX.TLV.Message.Initialize (Context, Buffer);
   RFLX.TLV.Message.Set_Tag (Context, RFLX.TLV.Msg_Data);
   RFLX.TLV.Message.Set_Length (Context, 4);
   Data := (1, 2, 3, 4, others => 0);
   Set_Value (Context);

   --  Checking generated message
   RFLX.TLV.Message.Take_Buffer (Context, Buffer);
   if Buffer.all = (64, 4, 1, 2, 3, 4) then
      Ada.Text_IO.Put_Line ("Expected");
   else
      Ada.Text_IO.Put_Line ("Unexpected");
   end if;
end Main;
```

## Dependencies

- [Python >=3.6](https://www.python.org)
- [PyParsing](https://github.com/pyparsing/pyparsing/)
- [PyDotPlus](https://github.com/carlos-jenkins/pydotplus)
- [Z3](https://github.com/Z3Prover/z3)
- [GNAT Community 2019](https://www.adacore.com/download)
- [SPARK Pro 20.0](https://www.adacore.com/sparkpro) (only required for fully automatic verification of generated code)

## Known Issues

### GNAT Community 2019

- GNAT shows an incorrect warning for `Initialize (Context, Buffer)`. It can be suppressed by adding `pragma Assert (Buffer = null)` after calling `Initialize`.
- GNATprove is unable to prove some parts of the generated code.

These issues should be fixed in the GNAT Community 2020 release.

## Licence

This software is licensed under the `AGPL-3.0`. See the `LICENSE` file for the full license text.
