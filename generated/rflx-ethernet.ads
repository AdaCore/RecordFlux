with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.Ethernet with
  SPARK_Mode
is

   type Address is mod 2**48;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Address return Address is
     (Address'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Address);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Address);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Address) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Address) return Address is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Type_Length_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Type_Length is Type_Length_Base range 46 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Type_Length return Type_Length is
     (Type_Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Type_Length_Base);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, Type_Length_Base);

   function Valid (Value : Type_Length_Base) return Boolean is
     (Value >= 46);

   function Convert (Value : Type_Length_Base) return Type_Length is
     (Value)
    with
     Pre =>
       Valid (Value);

   type TPID_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype TPID is TPID_Base range 16#8100# .. 16#8100#;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TPID return TPID is
     (TPID'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, TPID_Base);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, TPID_Base);

   function Valid (Value : TPID_Base) return Boolean is
     (Value >= 16#8100#
      and Value <= 16#8100#);

   function Convert (Value : TPID_Base) return TPID is
     (Value)
    with
     Pre =>
       Valid (Value);

   type TCI is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TCI return TCI is
     (TCI'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Extract is new RFLX.Types.Extract (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, TCI);

   procedure Insert is new RFLX.Types.Insert (RFLX.Types.Index, RFLX.Types.Byte, RFLX.Types.Bytes, RFLX.Types.Offset, TCI);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : TCI) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : TCI) return TCI is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.Ethernet;
