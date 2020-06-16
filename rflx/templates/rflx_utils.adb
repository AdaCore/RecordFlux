pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body {prefix}RFLX_Utils with
  SPARK_Mode
is

   procedure Lemma_Right_Shift_Limit (X : U64; J : Natural; K : Natural) with
     Pre  =>
       J <= U64'Size
       and then K < U64'Size
       and then J >= K
       and then J - K in 0 .. U64'Size - 1
       and then (if J < U64'Size then X < 2**J),
     Post =>
       X / U64 (2)**K < U64 (2)**(J - K),
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
       and then (if J < U64'Size then X < U64 (2)**J),
     Post =>
       (if J + K < U64'Size
        then X * U64 (2)**K <= U64 (2)**(J + K) - U64 (2)**K and U64 (2)**(J + K) >= U64 (2)**K
        else X * U64 (2)**K <= U64'Last - U64 (2)**K + 1),
     Ghost
   is
   begin
      null;
   end Lemma_Left_Shift_Limit;

   function Right_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64
   is
      Result : constant U64 := Value / U64 (2)**Length;
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

   function Mod_Pow2 (Value : U64; Exp : Natural) return U64
   is
      pragma Assert (U64 (2)**Exp > 0);
      Result : constant U64 := Value mod U64 (2)**Exp;
   begin
      pragma Assert (Result < 2**Exp);
      return Result;
   end Mod_Pow2;

end {prefix}RFLX_Utils;
