package body RFLX.Lemmas with
  SPARK_Mode
is

   procedure Mult_Limit (Value_1 : Long_Integer;
                         Exp_1   : Natural;
                         Value_2 : Long_Integer;
                         Exp_2   : Natural)
   is
   begin
      Mult_Mono (Value_1, 2**Exp_1, Value_2);
      pragma Assert (Value_1 * Value_2 <= 2**Exp_1 * Value_2);
      Mult_Mono (Value_2, 2**Exp_2, 2**Exp_1);
      pragma Assert (2**Exp_1 * Value_2 <= 2**Exp_1 * 2**Exp_2);
      Exp_Mult (2, Exp_1, Exp_2);
      pragma Assert (2**Exp_1 * 2**Exp_2 = 2**(Exp_1 + Exp_2));
      pragma Assert (Value_1 * Value_2 <= 2**(Exp_1 + Exp_2));
   end Mult_Limit;

   procedure Mult_Ge_0 (Factor_1 : Long_Integer;
                        Factor_2 : Long_Integer)
   is
   begin
      null;
   end Mult_Ge_0;

   procedure Mult_Mono (X : Long_Integer;
                        Y : Long_Integer;
                        Z : Long_Integer)
   is
   begin
      null;
   end Mult_Mono;

   procedure Exp_Mult (Base  : Long_Integer;
                       Exp_1 : Natural;
                       Exp_2 : Natural)
   is
   begin
      null;
   end Exp_Mult;

end RFLX.Lemmas;
