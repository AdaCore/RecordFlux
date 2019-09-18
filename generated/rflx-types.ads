with System.Storage_Elements;

package RFLX.Types with
  SPARK_Mode
is

   type Byte is mod 2**8;

   type Length is new Natural;
   subtype Index is Length range 1 .. Length'Last;

   type Bit_Length is range 0 .. 2**34 - 8;
   subtype Bit_Index is Bit_Length range 1 .. Bit_Length'Last;

   function Byte_Index (Bit_Idx : Bit_Index) return Index is
     (Length ((Bit_Idx - 1) / 8) + 1);

   function First_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 1);

   function Last_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 8);

   type Bytes is array (Index range <>) of Byte with
     Predicate => Bytes'Length > 0;

   type Bytes_Ptr is access Bytes;

   type Integer_Address is new System.Storage_Elements.Integer_Address;

   function Bytes_Address (Buffer : Bytes_Ptr) return Integer_Address with
     Global => null;

   function Bytes_First (Buffer : Bytes_Ptr) return Index is
     (Buffer'First)
     with
       Pre => Buffer /= null;

   function Bytes_Last (Buffer : Bytes_Ptr) return Index is
     (Buffer'Last)
     with
       Pre => Buffer /= null;

   type Offset is mod 8;

   generic
      type Int is mod <>;
   function Convert_To_Mod (Buffer : Bytes; Offset : Types.Offset := 0) return Int with
     Pre => Buffer'Length = Byte_Index (Int'Size + Bit_Length (Offset));

   generic
      type Int is range <>;
   function Convert_To_Int (Buffer : Bytes; Offset : Types.Offset := 0) return Int with
     Pre => Buffer'Length = Byte_Index (Int'Size + Bit_Length (Offset));

   generic
      type Index_Type   is (<>);
      type Element_Type is (<>);
      type Array_Type   is array (Index_Type range <>) of Element_Type;
      type Offset_Type  is (<>);
      type Value_Type   is (<>);
   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type with
     Pre => (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size < Data'Length
             and then (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size <= Natural'Size
             and then Natural (((Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size) * Element_Type'Size) < Long_Integer'Size - 1
             and then 2**(Element_Type'Size - Natural (Offset_Type'Pos (Offset) mod Element_Type'Size)) <= Long_Integer'Last;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Bit_Length return Bit_Length is
     (Bit_Length'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

end RFLX.Types;
