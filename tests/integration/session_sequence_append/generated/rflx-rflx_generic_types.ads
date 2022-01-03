pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with Ada.Unchecked_Deallocation;
with RFLX.RFLX_Arithmetic;

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

   subtype U64 is RFLX.RFLX_Arithmetic.U64;

   use type U64;

   subtype Bit_Index is Bit_Length range 1 .. Bit_Length'Last;

   function To_Index (Bit_Idx : Bit_Length) return Index is
     (Index (Length ((Bit_Idx - 1) / 8) + 1));

   function To_Length (Bit_Len : Bit_Length) return Length is
     (Length ((Bit_Len + 7) / 8));

   function To_Bit_Length (Len : Length) return Bit_Length is
     (Bit_Length (Len) * 8);

   function To_First_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 1);

   function To_Last_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 8);

   function To_Last_Bit_Index (Idx : Length) return Bit_Length is
     ((Bit_Length (Idx) - 1) * 8 + 8);

   type Offset is mod 8;

   type Byte_Order is (High_Order_First, Low_Order_First);

   function Extract
      (Buffer : Bytes_Ptr;
       First  : Index;
       Last   : Index;
       Off    : Offset;
       Size   : Positive;
       BO     : Byte_Order) return U64
   with
     Pre =>
       (Buffer /= null
        and then First >= Buffer'First
        and then Last <= Buffer'Last
        and then Size in 1 .. U64'Size
        and then First <= Last
        and then Last - First <= Index'Last - 1
        and then Length ((Offset'Pos (Off) + Size - 1) / Byte'Size) < Length (Last - First + 1)
        and then (Offset'Pos (Off) + Size - 1) / Byte'Size <= Natural'Size
        and then (Byte'Size - Natural (Offset'Pos (Off) mod Byte'Size)) < Long_Integer'Size - 1);

   procedure Insert
      (Val    : U64;
       Buffer : Bytes_Ptr;
       First  : Index;
       Last   : Index;
       Off    : Offset;
       Size   : Positive;
       BO     : Byte_Order)
   with
     Pre =>
       (Buffer /= null
        and then First >= Buffer'First
        and then Last <= Buffer'Last
        and then Size in 1 .. U64'Size
        and then (if Size < U64'Size then Val < 2**Size)
        and then First <= Last
        and then Last - First <= Index'Last - 1
        and then Length ((Offset'Pos (Off) + Size - 1) / Byte'Size) < Length (Last - First + 1)),
     Post =>
       (Buffer'First = Buffer.all'Old'First and Buffer'Last = Buffer.all'Old'Last);

   procedure Free is new Ada.Unchecked_Deallocation (Object => Bytes, Name => Bytes_Ptr);

   function Unreachable return Boolean is (False) with Pre => False;

   function Unreachable return Bit_Length is (0) with Pre => False;

   function Unreachable return Length is (0) with Pre => False;

end RFLX.RFLX_Generic_Types;