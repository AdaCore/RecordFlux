# [![RecordFlux](https://raw.githubusercontent.com/Componolit/RecordFlux/main/doc/img/logo.svg)](https://github.com/Componolit/RecordFlux/)

[![PyPI](https://img.shields.io/pypi/v/RecordFlux?color=blue)](https://pypi.org/project/RecordFlux/)
[![Python Versions](https://img.shields.io/badge/python-3.7%20%7C%203.8%20%7C%203.9-blue.svg)](https://python.org/)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![Tests](https://github.com/Componolit/RecordFlux/workflows/tests/badge.svg)](https://github.com/Componolit/RecordFlux/actions)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/5052/badge)](https://bestpractices.coreinfrastructure.org/projects/5052)

RecordFlux is a toolset for the formal specification of messages and the generation of verifiable binary parsers and message generators.

## Message Specification

The RecordFlux specification language is a domain-specific language to formally specify message formats of existing real-world binary protocols. Its syntax is inspired by [Ada](https://www.adacore.com/about-ada). A detailed description of the language elements can be found in the [Language Reference](/doc/Language-Reference.md).

Message specifications are automatically verified using the [Z3 theorem prover](https://github.com/Z3Prover/z3). The following invariants are proven at the specification level:

* Field conditions are mutually exclusive
* Field conditions do not contradict each other
* Field conditions are not statically false
* Each field is reachable on at least one path from the initial node
* Each field has at least one path to the final node
* Message fields are always located after the first message bit
* Field size is never negative
* Message fields cover all bits of a message on all paths
* Overlaid fields are congruent with exactly one other field

## SPARK Code Generation

Message parsers and generators are generated based on message specifications. The generated parser allows to validate and dissect messages and thereby respects all specified restrictions between message fields and related messages. The generated message generator enables the creation of messages according to the message specification. By using [SPARK](https://www.adacore.com/about-spark) we are able to prove the absence of runtime errors and prevent the incorrect usage of the generated code (e.g., enforce that a field of a message is validated before accessed).

Multiple packages are generated for a specification. All basic types like integers, enumerations and sequences are collectively declared in one package. For each message a child package is generated which contains validation, accessor and setter functions for every field of the message.

A user of the generated code has to validate a message field or the whole message before accessing the data of a particular message field. The SPARK verification tools in combination with the generated contracts make it possible to ensure this property, and so prevent incorrect usage.

The `rflx` tool is used to verify a specification and generate SPARK code based on it. It offers the sub-commands `check` and `generate` for this purpose. The sub-command `graph` allows to generate images of the graph representations of messages in a specification.

## Python Library

PyRFLX is a Python library for rapid-prototyping and validation. It uses RecordFlux specifications for parsing and generation of messages and validates the formal specification at runtime. It can be used by importing `rflx.pyrflx`.

By default assertions and contracts are executed to ensure correct functionality. For improved performance these additional checks can be disabled by running Python with the [`-O`](https://docs.python.org/3/using/cmdline.html#cmdoption-o) switch.

## Example

In the following, the complete process of specifying a message, generating code, and using the generated code is demonstrated using a small example.

### Specification

The following sample specification describes a protocol `TLV` with one message type `Message` consisting of three fields:

- A field `Tag` of 2 bit length,
- a field `Value_Length` of 14 bit length, and
- a field `Value`, whose length is specified by the value in `Value_Length`.

The `Tag` can have two valid values: `1` (`Msg_Data`) and `3` (`Msg_Error`). In case `Tag` has a value of `1` the fields `Value_Length` and `Value` follow. `Message` contains only the `Tag` field, if the value of `Tag` is `3`. All other values of `Tag` lead to an invalid message.

The structure of messages is often non-linear because of optional fields. For this reason the specification uses a graph-based representation. The order of fields is defined by then clauses. Then clauses are also used to state conditions and aspects of the following field. A more detailed description can be found in the [Language Reference](doc/Language-Reference.md#message-type).

```ada,rflx
package TLV is

   type Tag is (Msg_Data => 1, Msg_Error => 3) with Size => 8;
   type Length is mod 2**16;

   type Message is
      message
         Tag    : Tag
            then Length
               if Tag = Msg_Data
            then null
               if Tag = Msg_Error;
         Length : Length
            then Value
               with Size => Length * 8;
         Value  : Opaque;
       end message;

end TLV;
```

### Generating Code

With the sub-command `check` the correctness of the given specification file can be checked.

```console
$ rflx check tests/data/specs/tlv.rflx
Parsing tests/data/specs/tlv.rflx
Processing TLV
```

The sub-command `generate` is used to generate the code based on the specification. The target directory and the specification files have to be given.

```console
$ mkdir /tmp/generated
$ rflx generate -d /tmp/generated tests/data/specs/tlv.rflx
Parsing tests/data/specs/tlv.rflx
Processing TLV
Creating /tmp/generated/rflx-tlv.ads
Creating /tmp/generated/rflx-tlv-generic_message.ads
Creating /tmp/generated/rflx-tlv-generic_message.adb
Creating /tmp/generated/rflx-tlv-message.ads
Creating /tmp/generated/rflx-rflx_arithmetic.ads
Creating /tmp/generated/rflx-rflx_builtin_types-conversions.ads
Creating /tmp/generated/rflx-rflx_builtin_types.ads
Creating /tmp/generated/rflx-rflx_generic_types.ads
Creating /tmp/generated/rflx-rflx_message_sequence.ads
Creating /tmp/generated/rflx-rflx_scalar_sequence.ads
Creating /tmp/generated/rflx-rflx_types.ads
Creating /tmp/generated/rflx-rflx_arithmetic.adb
Creating /tmp/generated/rflx-rflx_generic_types.adb
Creating /tmp/generated/rflx-rflx_message_sequence.adb
Creating /tmp/generated/rflx-rflx_scalar_sequence.adb
Creating /tmp/generated/rflx.ads
```

### Using the Generated Code

All scalar types defined in the specification are represented by a similar Ada type in the generated code. For `TLV` the following types are defined in the package `RFLX.TLV`:

- `type Tag is (Msg_Data, Msg_Error) with Size => 2`
- `for Tag use (Msg_Data => 1, Msg_Error => 3);`
- `type Length is mod 2**14`

All types and subprograms related to `Message` can be found in the package `RFLX.TLV.Message`:

- `type Context`
    - Stores buffer and internal state
- `procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr)`
    - Initialize context with buffer
- `procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index)`
    - Initialize context with buffer and explicit bounds
- `procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr)`
    - Get buffer and remove it from context (note: buffer cannot put back into context, thus further verification of message is not possible after this action)
- `function Has_Buffer (Ctx : Context) return Boolean`
    - Check if context contains buffer (i.e. non-null pointer)
- `procedure Verify (Ctx : in out Context; Fld : Field)`
    - Verify validity of field
- `procedure Verify_Message (Ctx : in out Context)`
    - Verify all fields of message
- `function Structural_Valid (Ctx : Context; Fld : Field) return Boolean`
    - Check if composite field is structural valid (i.e. location and size of field is correct, but content is not necessarily valid)
- `function Present (Ctx : Context; Fld : Field) return Boolean`
    - Check if composite field is structural valid and has non-zero size
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
- `generic with procedure Process_Value (Value : Types.Bytes); procedure Get_Value (Ctx : Context)`
    - Access content of `Value` field
- `function Valid_Next (Ctx : Context; Fld : Field) return Boolean`
    - Check if field is potential next field
- `procedure Set_Tag (Ctx : in out Context; Value : Tag)`
    - Set value of `Tag` field
- `procedure Set_Length (Ctx : in out Context; Value : Length)`
    - Set value of `Length` field
- `procedure Set_Value_Empty (Ctx : in out Context; Value : Types.Bytes)`
    - Set empty `Value` field
- `procedure Set_Value (Ctx : in out Context; Value : Types.Bytes)`
    - Set content of `Value` field
- `generic with procedure Process_Value (Value : out Types.Bytes); procedure Generic_Set_Value (Ctx : in out Context)`
    - Set content of `Value` field
- `procedure Initialize_Value (Ctx : in out Context)`
    - Initialize `Value` field (precondition to switch context for generating contained message)

A simple program to parse a `TLV.Message` could be as follows:

```ada
with Ada.Text_IO;
with RFLX.RFLX_Builtin_Types;
with RFLX.TLV.Message;

procedure Main is
   Buffer  : RFLX.RFLX_Builtin_Types.Bytes_Ptr := new RFLX.RFLX_Builtin_Types.Bytes'(1, 0, 4, 0, 0, 0, 0);
   Context : RFLX.TLV.Message.Context;
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

```ada
with Ada.Text_IO;
with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Bytes;
with RFLX.TLV.Message;

procedure Main is
   Buffer  : RFLX.RFLX_Builtin_Types.Bytes_Ptr := new RFLX.RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0);
   Context : RFLX.TLV.Message.Context;
begin
   --  Generating message
   RFLX.TLV.Message.Initialize (Context, Buffer);
   RFLX.TLV.Message.Set_Tag (Context, RFLX.TLV.Msg_Data);
   RFLX.TLV.Message.Set_Length (Context, 4);
   RFLX.TLV.Message.Set_Value (Context, (1, 2, 3, 4));

   --  Checking generated message
   RFLX.TLV.Message.Take_Buffer (Context, Buffer);
   if Buffer.all = (1, 0, 4, 1, 2, 3, 4) then
      Ada.Text_IO.Put_Line ("Expected");
   else
      Ada.Text_IO.Put_Line ("Unexpected");
   end if;
end Main;
```

### Using the Python Library

The following code shows how PyRFLX can be used to parse and generate messages in Python:


```python
import sys

from rflx.pyrflx import MessageValue, PyRFLX

PYRFLX = PyRFLX.from_specs(["tests/data/specs/tlv.rflx"])
TLV = PYRFLX["TLV"]


def parse_message(input_bytes: bytes) -> MessageValue:
    msg = TLV["Message"]
    msg.parse(input_bytes)
    return msg


def create_message() -> MessageValue:
    msg = TLV["Message"]
    msg.set("Tag", "Msg_Data")
    msg.set("Length", 4)
    msg.set("Value", b"\x01\x02\x03\x04")
    return msg


if parse_message(b"\x01\x00\x04\x01\x02\x03\x04") != create_message():
    sys.exit("Error")
```

## Installation

As a prerequisite, the following dependencies need to be installed:

- [GNAT Community](https://www.adacore.com/download) >= 2021
- [GMP](https://gmplib.org/) is provided as a package for various distributions, e.g., `libgmp-dev` (Debian/Ubuntu), `gmp-devel` (Fedora) or `gmp` (Arch Linux).
- Optional: [Graphviz](https://graphviz.org/) is required for plotting graphs. It is provided as package `graphviz` for various distributions.

RecordFlux can be installed from PyPI:

```console
$ pip3 install RecordFlux
```

By default the following dependencies are installed:

- [attrs](https://github.com/python-attrs/attrs)
- [icontract](https://github.com/Parquery/icontract)
- [PyDotPlus](https://github.com/carlos-jenkins/pydotplus)
- [Z3](https://github.com/Z3Prover/z3)
- [RecordFlux parser](https://github.com/Componolit/RecordFlux-language)

Optionally, the GNAT Studio IDE integration for RecordFlux can be installed. It enables syntax highlighting for RecordFlux specifications and allows for running RecordFlux from within GNAT Studio. In the RecordFlux source directory do:

```console
$ make install_gnatstudio
```

## Contribution and Feedback

Contributions and feedback to RecordFlux are very welcome. To discuss a bug or an enhancement, [open a ticket on GitHub](https://github.com/Componolit/RecordFlux/issues/new/choose) and select the appropriate issue template. Please give sufficient information about your issue, the software version you are using and your environment such that the developers can understand and (if necessary) reproduce the problem. If none of the provided issue templates fit your needs, feel free to open [a blank issue](https://github.com/Componolit/RecordFlux/issues/new).

See [the development documentation](/doc/Development.md) on how to contribute to RecordFlux.

## Limitations

A list of known limitations for version 0.5.0 can be found [here](https://github.com/Componolit/RecordFlux/issues?q=is%3Aissue+label%3Alimitation+label%3Av0.5.0).

## Background

More information about the theoretical background can be found in our paper:

Reiher T., Senier A., Castrillon J., Strufe T. (2020) RecordFlux: Formal Message Specification and Generation of Verifiable Binary Parsers. In: Arbab F., Jongmans SS. (eds) Formal Aspects of Component Software. FACS 2019. Lecture Notes in Computer Science, vol 12018. Springer, Cham ([paper](https://doi.org/10.1007/978-3-030-40914-2_9), [preprint](https://arxiv.org/abs/1910.02146))

## Licence

This software is licensed under the `AGPL-3.0`. See the `LICENSE` file for the full license text.
