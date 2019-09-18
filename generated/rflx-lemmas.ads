package RFLX.Lemmas with
  SPARK_Mode, Ghost
is

   procedure Mult_Limit (Value_1 : Long_Integer;
                         Exp_1   : Natural;
                         Value_2 : Long_Integer;
                         Exp_2   : Natural) with
     Pre  => 2**Exp_1 <= Long_Integer'Last
               and 2**Exp_2 <= Long_Integer'Last
               and 0 <= Value_2
               and Value_1 <= 2**Exp_1
               and Value_2 <= 2**Exp_2,
     Post => Value_1 * Value_2 <= 2**(Exp_1 + Exp_2),
     Ghost;

   procedure Mult_Ge_0 (Factor_1 : Long_Integer;
                        Factor_2 : Long_Integer) with
     Pre  => Factor_1 >= 0 and Factor_2 >= 0,
     Post => Factor_1 * Factor_2 >= 0,
     Ghost;

   procedure Mult_Mono (X : Long_Integer;
                        Y : Long_Integer;
                        Z : Long_Integer) with
     Pre  => 0 <= Z and X <= Y,
     Post => Z * X <= Z * Y,
     Ghost;

   procedure Exp_Mult (Base  : Long_Integer;
                       Exp_1 : Natural;
                       Exp_2 : Natural) with
     Post => Base**Exp_1 * Base**Exp_2 = Base**(Exp_1 + Exp_2),
     Ghost;

end RFLX.Lemmas;
