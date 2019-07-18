with System.Storage_Elements;

package RFLX.Types with
  SPARK_Mode
is

   type Byte is mod 2**8;

   type Length_Type is new Natural;
   subtype Index_Type is Length_Type range 1 .. Length_Type'Last;

   type Bit_Length_Type is range 0 .. 2**34 - 8;
   subtype Bit_Index_Type is Bit_Length_Type range 1 .. Bit_Length_Type'Last;

   function Byte_Index (Bit_Index : Bit_Index_Type) return Index_Type is
     (Length_Type ((Bit_Index - 1) / 8) + 1);

   function First_Bit_Index (Index : Index_Type) return Bit_Index_Type is
     ((Bit_Length_Type (Index) - 1) * 8 + 1);

   function Last_Bit_Index (Index : Index_Type) return Bit_Index_Type is
     ((Bit_Length_Type (Index) - 1) * 8 + 8);

   type Bytes is array (Index_Type range <>) of Byte with
     Predicate => Bytes'Length > 0;

   type Bytes_Ptr is access Bytes;

   type Integer_Address is new System.Storage_Elements.Integer_Address;

   function Bytes_Address (Buffer : Bytes_Ptr) return Integer_Address with
     Global => null;

   function Bytes_First (Buffer : Types.Bytes_Ptr) return Types.Index_Type is
     (Buffer'First)
     with
       Pre => Buffer /= null;

   function Bytes_Last (Buffer : Types.Bytes_Ptr) return Types.Index_Type is
     (Buffer'Last)
     with
       Pre => Buffer /= null;

   type Offset_Type is mod 8;

   generic
      type Int is mod <>;
   function Convert_To_Mod (Buffer : Bytes; Offset : Offset_Type := 0) return Int with
     Pre => Buffer'Length = Byte_Index (Int'Size + Bit_Length_Type (Offset));

   generic
      type Int is range <>;
   function Convert_To_Int (Buffer : Bytes; Offset : Offset_Type := 0) return Int with
     Pre => Buffer'Length = Byte_Index (Int'Size + Bit_Length_Type (Offset));

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Bit_Length_Type return Bit_Length_Type is
     (Bit_Length_Type'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

end RFLX.Types;
