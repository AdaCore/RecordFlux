pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with {prefix}RFLX_Arithmetic;

generic
   type Custom_Index is range <>;
   type Custom_Byte is (<>);
   type Custom_Bytes is array (Custom_Index range <>) of Custom_Byte;
   type Custom_Bytes_Ptr is access Custom_Bytes;
   type Custom_Length is range <>;
   type Custom_Bit_Length is range <>;
package RFLX.RFLX_Generic_Types with
  SPARK_Mode
is

   subtype Index is Custom_Index;

   subtype Byte is Custom_Byte;

   subtype Bytes is Custom_Bytes;

   subtype Bytes_Ptr is Custom_Bytes_Ptr;

   subtype Length is Custom_Length;

   subtype Bit_Length is Custom_Bit_Length;

   pragma Compile_Time_Error (Index'First /= 1, "Index'First must be 1");

   pragma Compile_Time_Error (Byte'Size /= 8, "Byte must be of size 8");

   pragma Compile_Time_Error (Byte'Pos (Byte'Last) - Byte'Pos (Byte'First) + 1 /= 2**Byte'Size,
                              "Byte must cover entire value range");

   pragma Compile_Time_Error (Length'First /= 0, "Length'First must be 0");

   pragma Compile_Time_Error (Length'Pos (Length'Last) /= Index'Pos (Index'Last),
                              "Length'Last must be equal to Index'Last");

   pragma Compile_Time_Error (Bit_Length'First /= 0, "Bit_Length'First must be 0");

   pragma Compile_Time_Error (Bit_Length'Pos (Bit_Length'Last) /= Length'Pos (Length'Last) * 8,
                              "Bit_Length'Last must be equal to Length'Last * 8");

   subtype Bit_Index is Bit_Length range 1 .. Bit_Length'Last;

   subtype U64 is {prefix}RFLX_Arithmetic.U64;

   function Byte_Index (Bit_Idx : Bit_Index) return Index is
     (Index (Length ((Bit_Idx - 1) / 8) + 1));

   function First_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 1);

   function Last_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 8);

   type Offset is mod 8;

   generic
      type Value is mod <>;
   function Extract (Data : Bytes;
                     Off  : Offset) return Value with
     Pre =>
       ((Offset'Pos (Off) + Value'Size - 1) / Byte'Size < Data'Length
        and then (Offset'Pos (Off) + Value'Size - 1) / Byte'Size <= Natural'Size
        and then (Byte'Size - Natural (Offset'Pos (Off) mod Byte'Size)) < Long_Integer'Size - 1);

   generic
      type Value is mod <>;
   procedure Insert (Val  :        Value;
                     Data : in out Bytes;
                     Off  :        Offset) with
     Pre =>
       (Offset'Pos (Off) + Value'Size - 1) / Byte'Size < Data'Length;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_Bit_Length return Bit_Length is
     (Bit_Length'First)
     with
       Pre =>
         False;

   pragma Warnings (On, "precondition is * false");

end {prefix}RFLX_Generic_Types;
