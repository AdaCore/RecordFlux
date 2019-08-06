with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;

package Ethernet with
  SPARK_Mode
is

   type Address_Type is mod (2**48);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Address_Type return Address_Type is
     (Address_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Address_Type is new Types.Convert_To_Mod (Address_Type);

   function Valid_Address_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
     (True)
    with
     Pre => (Offset < 8 and then Buffer'Length = (((Address_Type'Size + Offset + (-1)) / 8) + 1));

   type Type_Length_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;

   subtype Type_Length_Type is Type_Length_Type_Base range 46 .. ((2**16) - 1);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Type_Length_Type return Type_Length_Type is
     (Type_Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Type_Length_Type_Base is new Types.Convert_To_Int (Type_Length_Type_Base);

   function Valid_Type_Length_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
     (Convert_To_Type_Length_Type_Base (Buffer, Offset) >= 46)
    with
     Pre => (Offset < 8 and then Buffer'Length = (((Type_Length_Type_Base'Size + Offset + (-1)) / 8) + 1));

   type TPID_Type_Base is range 0 .. ((2**16) - 1) with Size => 16;

   subtype TPID_Type is TPID_Type_Base range 33024 .. 33024;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TPID_Type return TPID_Type is
     (TPID_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_TPID_Type_Base is new Types.Convert_To_Int (TPID_Type_Base);

   function Valid_TPID_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
     ((Convert_To_TPID_Type_Base (Buffer, Offset) >= 33024 and then Convert_To_TPID_Type_Base (Buffer, Offset) <= 33024))
    with
     Pre => (Offset < 8 and then Buffer'Length = (((TPID_Type_Base'Size + Offset + (-1)) / 8) + 1));

   type TCI_Type is mod (2**16);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TCI_Type return TCI_Type is
     (TCI_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_TCI_Type is new Types.Convert_To_Mod (TCI_Type);

   function Valid_TCI_Type (Buffer : Types.Bytes; Offset : Natural) return Boolean is
     (True)
    with
     Pre => (Offset < 8 and then Buffer'Length = (((TCI_Type'Size + Offset + (-1)) / 8) + 1));

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Index_Type return Types.Index_Type is
     (Types.Index_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Types_Length_Type return Types.Length_Type is
     (Types.Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

end Ethernet;
