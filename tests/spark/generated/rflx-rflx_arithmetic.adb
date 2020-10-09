pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.RFLX_Arithmetic with
  SPARK_Mode
is

   function Pow2 (Exp : Natural) return U64 is
     (2**Exp);

   function Mod_Pow2 (Value : U64; Exp : Natural) return U64 is
     (Value mod Pow2 (Exp));

   procedure Lemma_Right_Shift_Limit (X : U64; J : Natural; K : Natural) with
     Pre  =>
       J <= U64'Size
       and then K < U64'Size
       and then J >= K
       and then J - K in 0 .. U64'Size - 1
       and then (if J < U64'Size then X < 2**J),
     Post =>
       X / Pow2 (K) < Pow2 (J - K),
     Ghost
   is
   begin
      null;
   end Lemma_Right_Shift_Limit;

   procedure Lemma_Left_Shift_Limit (X : U64; J : Natural; K : Natural) with
     Pre  =>
       J <= U64'Size
       and then K < U64'Size
       and then J + K <= U64'Size
       and then (if J < U64'Size then X < Pow2 (J)),
     Post =>
       (if J + K < U64'Size
        then X * Pow2 (K) <= Pow2 (J + K) - Pow2 (K) and Pow2 (J + K) >= Pow2 (K)
        else X * Pow2 (K) <= U64'Last - Pow2 (K) + 1),
     Ghost
   is
   begin
      null;
   end Lemma_Left_Shift_Limit;

   function Right_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64
   is
      Result : constant U64 := Value / Pow2 (Length);
   begin
      Lemma_Right_Shift_Limit (Value, Value_Size, Length);
      return Result;
   end Right_Shift;

   function Left_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64
   is
      Result : constant U64 := Value * 2**Length;
   begin
      Lemma_Left_Shift_Limit (Value, Value_Size, Length);
      return Result;
   end Left_Shift;

end RFLX.RFLX_Arithmetic;
