with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.UDP with
  SPARK_Mode
is

   type Port is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UDP_Port return UDP.Port is
     (UDP.Port'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Port);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Port);

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : UDP.Port) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : UDP.Port) return UDP.Port is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Length_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Length is Length_Base range 8 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UDP_Length return UDP.Length is
     (UDP.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Length_Base);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Length_Base);

   function Valid (Val : UDP.Length_Base) return Boolean is
     (Val >= 8);

   function Convert (Val : UDP.Length_Base) return UDP.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Checksum is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_UDP_Checksum return UDP.Checksum is
     (UDP.Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Checksum);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Checksum);

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : UDP.Checksum) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : UDP.Checksum) return UDP.Checksum is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.UDP;
