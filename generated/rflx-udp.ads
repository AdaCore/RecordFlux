with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index_Type, RFLX.Types.Length_Type, RFLX.Types.Bit_Index_Type, RFLX.Types.Bit_Length_Type;

package RFLX.UDP with
  SPARK_Mode
is

   type Port_Type is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Port_Type return Port_Type is
     (Port_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Port_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Port_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Port_Type) return Port_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Length_Type_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Length_Type is Length_Type_Base range 8 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length_Type return Length_Type is
     (Length_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (Length_Type_Base);

   function Valid (Value : Length_Type_Base) return Boolean is
     (Value >= 8);

   function Convert (Value : Length_Type_Base) return Length_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Checksum_Type is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Checksum_Type return Checksum_Type is
     (Checksum_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Checksum_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Checksum_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Checksum_Type) return Checksum_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.UDP;
