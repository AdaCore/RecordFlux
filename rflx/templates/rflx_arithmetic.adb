pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

package body {prefix}RFLX_Arithmetic with
  SPARK_Mode
is

   function Shift_Left (Value : U64; Amount : Natural) return U64 with
      Import,
      Convention => Intrinsic,
      Global => null;

   function Shift_Right (Value : U64; Amount : Natural) return U64 with
      Import,
      Convention => Intrinsic,
      Global => null;

   function Shift_Add (V : U64;
                       Data : U64;
                       Amount : Natural;
                       Bits : Natural) return U64
   is
      pragma Unreferenced (Bits);
   begin
      return Shift_Left (V, Amount) + Data;
   end Shift_Add;

   function Right_Shift (V : U64; Amount : Natural; Size : Natural) return U64
   is
      pragma Unreferenced (Size);
   begin
      return Shift_Right (V, Amount);
   end Right_Shift;

   function Left_Shift (V : U64; Amount : Natural; Size : Natural) return U64
   is
      pragma Unreferenced (Size);
      Result : constant U64 := Shift_Left (V, Amount);
   begin
      return Result;
   end Left_Shift;

   function Mask_Lower (V : U64; Mask, Bits : Natural) return U64
   is
      Result : constant U64 := Shift_Left (Shift_Right (V, Mask), Mask);
   begin
      pragma Assert
        (if Bits < U64'Size then Result <= 2 ** Bits - 2 ** Mask
         elsif Mask < U64'Size then Result <= U64'Last - 2 ** Mask + 1);
      return Result;
   end Mask_Lower;

   function Mask_Upper (V : U64; Mask : Natural) return U64
   is
   begin
      return V and (2 ** Mask - 1);
   end Mask_Upper;

   function Add (A : U64; B : U64; Total_Bits, Lower_Bits : Natural) return U64
   is
      pragma Unreferenced (Total_Bits, Lower_Bits);
   begin
      return A + B;
   end Add;

   procedure Lemma_Size (Val : Base_Integer; Size : Positive) is
   begin
      if Size < Base_Integer'Size then
         pragma Assert (Val < 2 ** Size);
         pragma Assert (U64 (Val) < 2 ** Size);
         pragma Assert (Fits_Into (U64 (Val), Size));
      else
         pragma Assert (Size = 63);
         pragma Assert (Fits_Into (U64 (Val), Size));
      end if;
   end Lemma_Size;

end {prefix}RFLX_Arithmetic;
