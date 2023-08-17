pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, """Always_Terminates"" is not a valid aspect identifier");

package {prefix}RFLX_Arithmetic with
  SPARK_Mode,
  Always_Terminates
is

   type U64 is mod 2**64 with
     Annotate => (GNATprove, No_Wrap_Around);

   type Base_Integer is range 0 .. 2 ** 63 - 1;

   -- Express that V contains at most Bits non-zero bits, in the least
   -- significant part (the rest is zero).
   pragma Warnings (Off, "postcondition does not mention function result");
   function Fits_Into (V : U64; Bits : Natural) return Boolean
   is (if Bits < U64'Size then V < 2 ** Bits)
     with Post => True;

   function Fits_Into (V : Base_Integer; Bits : Natural) return Boolean
   is (if Bits < Base_Integer'Size then V < 2 ** Bits)
     with Post => True;

   -- Express that V contains (U64'Size - Bits) leading zero bits, then (Bits -
   -- Lower) bits of data, then Lower bits of zeros.
   -- |- (U64'Size - bits) -|- (Bits-Lower) -|- Lower -|
   -- |000000000000000000000|xxxxxxxxxxxxxxxx|000000000|
   function Fits_Into_Upper (V : U64; Bits, Lower : Natural) return Boolean
   is (if Bits < U64'Size then V <= 2 ** Bits - 2 ** Lower
       elsif Lower > 0 and then Lower < U64'Size then V <= U64'Last - 2 ** Lower + 1)
     with Pre => Bits <= U64'Size and then Lower <= Bits,
          Post => True;
   pragma Warnings (On, "postcondition does not mention function result");

   -- V is assumed to contain Bits bits of data. Add the Amount bits contained
   -- in Data by shifting V to the left and adding Data. The result contains
   -- (Bits + Amount) bits of data.
   function Shift_Add (V : U64;
                       Data : U64;
                       Amount : Natural;
                       Bits : Natural) return U64
     with Pre  =>
       Bits < U64'Size
       and then Amount < U64'Size
       and then Fits_Into (V, Bits)
       and then U64'Size - Amount >= Bits
       and then Fits_Into (Data, Amount),
     Post => Fits_Into (Shift_Add'Result, Bits + Amount);

   -- Wrapper of Shift_Right that expresses the operation in terms of
   -- Fits_Into.
   function Right_Shift (V : U64; Amount : Natural; Size : Natural) return U64 with
     Pre =>
       Size <= U64'Size
       and then Fits_Into (V, Size)
       and then Amount <= Size
       and then Size - Amount < U64'Size,
     Post => Fits_Into (Right_Shift'Result, Size - Amount);

   -- Wrapper of Shift_Left that expresses the operation in terms of
   -- Fits_Into/Fits_Into_Upper.
   function Left_Shift (V : U64; Amount : Natural; Size : Natural) return U64 with
     Pre =>
       Size < U64'Size
       and then Amount < U64'Size
       and then Fits_Into (V, Size)
       and then Size + Amount < U64'Size,
       Post => Fits_Into_Upper (Left_Shift'Result, Size + Amount, Amount);

   -- V is assumed to have Bits bits of data. Set the lower bits of V to zero.
   function Mask_Lower (V : U64; Mask, Bits : Natural) return U64
     with Pre => Bits <= U64'Size and then Fits_Into (V, Bits) and then Mask <= Bits and then Mask >= 1,
          Post => Fits_Into_Upper (Mask_Lower'Result, Bits, Mask);

   -- Set the upper bits of V to zero.
   function Mask_Upper (V : U64; Mask : Natural) return U64
     with Pre => Mask < U64'Size,
          Post => Fits_Into (Mask_Upper'Result, Mask);

   -- Eng/RecordFlux/Workarounds#50 (required for GNAT Pro 23.2)
   pragma Warnings (Off, "aspect Unreferenced specified for ""Total_Bits""");

   -- Add A and B in the special case where A only uses the upper bits and B
   -- only the lower bits.
   function Add (A : U64; B : U64; Total_Bits, Lower_Bits : Natural) return U64
     with Pre =>
       Total_Bits <= U64'Size
       and then Lower_Bits <= Total_Bits
       and then (if Total_Bits = U64'Size then Lower_Bits /= U64'Size)
       and then Fits_Into_Upper (A, Total_Bits, Lower_Bits)
       and then Fits_Into (B, Lower_Bits),
     Post => Add'Result = A + B and Fits_Into (Add'Result, Total_Bits),
     Global => null;

   pragma Warnings (On, "aspect Unreferenced specified for ""Total_Bits""");

   procedure Lemma_Size (Val : Base_Integer; Size : Positive)
   with Ghost,
      Pre => Size in 1 .. 63 and then Fits_Into (Val, Size),
      Post => Fits_Into (U64 (Val), Size);

end {prefix}RFLX_Arithmetic;
