with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index_Type, RFLX.Types.Length_Type, RFLX.Types.Bit_Index_Type, RFLX.Types.Bit_Length_Type;

package RFLX.Ethernet with
  SPARK_Mode
is

   type Address_Type is mod 2**48;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Address_Type return Address_Type is
     (Address_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Address_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Address_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Address_Type) return Address_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type Type_Length_Type_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype Type_Length_Type is Type_Length_Type_Base range 46 .. 2**16 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Type_Length_Type return Type_Length_Type is
     (Type_Length_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (Type_Length_Type_Base);

   function Valid (Value : Type_Length_Type_Base) return Boolean is
     (Value >= 46);

   function Convert (Value : Type_Length_Type_Base) return Type_Length_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type TPID_Type_Base is range 0 .. 2**16 - 1 with
     Size =>
       16;

   subtype TPID_Type is TPID_Type_Base range 33024 .. 33024;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TPID_Type return TPID_Type is
     (TPID_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Int (TPID_Type_Base);

   function Valid (Value : TPID_Type_Base) return Boolean is
     (Value >= 33024
      and then Value <= 33024);

   function Convert (Value : TPID_Type_Base) return TPID_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

   type TCI_Type is mod 2**16;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TCI_Type return TCI_Type is
     (TCI_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (TCI_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : TCI_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : TCI_Type) return TCI_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.Ethernet;
