pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, """Always_Terminates"" is not a valid aspect identifier");

with Ada.Unchecked_Deallocation;
with {prefix}RFLX_Arithmetic;

generic
   type Custom_Index is range <>;
   type Custom_Byte is (<>);
   type Custom_Bytes is array (Custom_Index range <>) of Custom_Byte;
   type Custom_Bytes_Ptr is access Custom_Bytes;
   type Custom_Length is range <>;
   type Custom_Bit_Length is range <>;
package {prefix}RFLX_Generic_Types with
  SPARK_Mode,
  Always_Terminates
is

   subtype Index is Custom_Index;

   subtype Byte is Custom_Byte;

   subtype Bytes is Custom_Bytes;

   subtype Bytes_Ptr is Custom_Bytes_Ptr;

   subtype Length is Custom_Length;

   subtype Bit_Length is Custom_Bit_Length;

   function "+" (Left : Index; Right : Index) return Index is abstract;

   function "-" (Left : Index; Right : Index) return Index is abstract;

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

   subtype U64 is {prefix}RFLX_Arithmetic.U64;

   subtype Base_Integer is {prefix}RFLX_Arithmetic.Base_Integer;

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

   function Fits_Into (V : U64; Bits : Natural) return Boolean renames RFLX_Arithmetic.Fits_Into;
   function Fits_Into (V : Base_Integer; Bits : Natural) return Boolean renames RFLX_Arithmetic.Fits_Into;

   type Offset is mod 8;

   type Byte_Order is (High_Order_First, Low_Order_First);

   pragma Warnings (Off, "precondition is always False");

   function Unreachable return Boolean is (False) with Pre => False;

   function Unreachable return Bit_Length is (0) with Pre => False;

   function Unreachable return Length is (0) with Pre => False;

   pragma Warnings (On, "precondition is always False");

   procedure Lemma_Size (Val : Base_Integer; Size : Positive) renames {prefix}RFLX_Arithmetic.Lemma_Size;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Bytes, Name => Bytes_Ptr);

end {prefix}RFLX_Generic_Types;
