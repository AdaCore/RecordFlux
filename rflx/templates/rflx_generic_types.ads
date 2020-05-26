pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

pragma Warnings (Off, "type ""Bytes_Ptr"" is not referenced");

generic
   type Index is range <>;
   type Byte is (<>);
   type Bytes is array (Index range <>) of Byte;
   type Bytes_Ptr is access Bytes;
   type Length is range <>;
   type Bit_Length is range <>;
package {prefix}RFLX_Generic_Types is

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

   function Byte_Index (Bit_Idx : Bit_Index) return Index is
     (Index (Length ((Bit_Idx - 1) / 8) + 1));

   function First_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 1);

   function Last_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 8);

   type Offset is mod 8;

   generic
      type Value is (<>);
   function Extract (Data : Bytes;
                     Ofst : Offset) return Value with
     Pre =>
       ((Offset'Pos (Ofst) + Value'Size - 1) / Byte'Size < Data'Length
        and then (Offset'Pos (Ofst) + Value'Size - 1) / Byte'Size <= Natural'Size
        and then Natural (((Offset'Pos (Ofst) + Value'Size - 1) / Byte'Size) * Byte'Size) < Long_Integer'Size - 1
        and then (Byte'Size - Natural (Offset'Pos (Ofst) mod Byte'Size)) < Long_Integer'Size - 1);

   generic
      type Value is (<>);
   procedure Insert (Val  :        Value;
                     Data : in out Bytes;
                     Ofst :        Offset) with
     Pre =>
       (Offset'Pos (Ofst) + Value'Size - 1) / Byte'Size < Data'Length;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Bit_Length return Bit_Length is
     (Bit_Length'First)
     with
       Pre =>
         False;

   pragma Warnings (On, "precondition is statically false");

end {prefix}RFLX_Generic_Types;
