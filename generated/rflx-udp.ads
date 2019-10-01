with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.UDP with
  SPARK_Mode
is

   type Port is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Port return Port is
     (Port'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Port);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Port) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Port) return Port is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Length_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Length is Length_Base range 8 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length return Length is
     (Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Length_Base);

   function Valid (Value : Length_Base) return Boolean is
     (Value >= 8);

   function Convert (Value : Length_Base) return Length is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Checksum is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Checksum return Checksum is
     (Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Checksum);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Checksum) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Checksum) return Checksum is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.UDP;
