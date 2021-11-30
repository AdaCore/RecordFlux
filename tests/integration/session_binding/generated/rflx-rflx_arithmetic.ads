pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package RFLX.RFLX_Arithmetic with
  SPARK_Mode
is

   type U64 is mod 2**64 with
     Annotate => (GNATprove, No_Wrap_Around);

   function Shift_Left (Value : U64; Amount : Natural) return U64 with
      Import,
      Convention => Intrinsic,
      Global => null;

   function Shift_Right (Value : U64; Amount : Natural) return U64 with
      Import,
      Convention => Intrinsic,
      Global => null;

   --  Express that V contains no more than Bits bits of data (the rest is
   --  zero).
   function Has_Bits (V : U64; Bits : Natural) return Boolean
     is (if Bits < U64'Size then V < 2 ** Bits);

   --  Express that V contains no more than (Bits - Lower) bits of data, that
   --  are contained in the more significant part of V (the rest is zero):
   --  |--|- (bits-lower) -|- lower -|
   --  |00|xxxxxxxxxxxxxxxx|000000000|
   function Has_Bits_Upper (V : U64; Bits, Lower : Natural) return Boolean
   is (if Bits < U64'Size then V <= 2 ** Bits - 2 ** Lower
       elsif Lower > 0 and then Lower < U64'Size then V <= U64'Last - 2 ** Lower + 1)
     with Pre => Bits <= U64'Size and then Lower <= Bits;

   --  V is assumed to contain Bits bits of data. Add the Amount bits
   --  contained in Data by shifting Result to the left and adding Data.
   --  The result contains (Bits + Amount) bits of data.
   function Shift_Add (V : U64;
                       Data : U64;
                       Amount : Natural;
                       Bits : Natural) return U64
     with Pre  =>
       Bits < U64'Size
       and then Amount < U64'Size
       and then Has_Bits (V, Bits)
       and then U64'Size - Amount >= Bits
       and then Has_Bits (Data, Amount),
     Post => Has_Bits (Shift_Add'Result, Bits + Amount);

   --  Wrapper of Shift_Right that expresses the operation in terms of
   --  Has_Bits.
   function Right_Shift (V : U64; Amount : Natural; Size : Natural) return U64 with
     Pre =>
       Size <= U64'Size
       and then Has_Bits (V, Size)
       and then Amount <= Size
       and then Size - Amount < U64'Size,
     Post => Has_Bits (Right_Shift'Result, Size - Amount);

   --  Wrapper of Shift_Left that expresses the operation in terms of
   --  Has_Bits/Has_Bits_Upper.
   function Left_Shift (V : U64; Amount : Natural; Size : Natural) return U64 with
     Pre =>
       Size < U64'Size
       and then Amount < U64'Size
       and then Has_Bits (V, Size)
       and then Size + Amount < U64'Size,
       Post => Has_Bits_Upper (Left_Shift'Result, Size + Amount, Amount);

   --  V is assumed to have Bits bits of data. Set the lower bits of V to zero.
   function Mask_Lower (V : U64; Mask, Bits : Natural) return U64
     with Pre => Bits <= U64'Size and then Has_Bits (V, Bits) and then Mask <= Bits and then Mask >= 1,
          Post => Has_Bits_Upper (Mask_Lower'Result, Bits, Mask);

   --  Set the upper bits of V to zero.
   function Mask_Upper (V : U64; Mask : Natural) return U64
     with Pre => Mask < U64'Size,
          Post => Has_Bits (Mask_Upper'Result, Mask);

   --  Add A and B in the special case where A only uses the upper bits and B
   --  only the lower bits.
   function Add (A : U64; B : U64; Total_Bits, Lower_Bits : Natural) return U64
     with Pre =>
       Total_Bits <= U64'Size
       and then Lower_Bits <= Total_Bits
       and then (if Total_Bits = U64'Size then Lower_Bits /= U64'Size)
       and then Has_Bits_Upper (A, Total_Bits, Lower_Bits)
       and then Has_Bits (B, Lower_Bits),
     Post => Add'Result = A + B and Has_Bits (Add'Result, Total_Bits),
     Global => null;

end RFLX.RFLX_Arithmetic;
