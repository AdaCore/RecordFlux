package body {prefix}Lemmas with
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

   procedure Mult_Div_Id (X : Long_Integer;
                          Y : Long_Integer)
   is
   begin
      null;
   end Mult_Div_Id;

   procedure Div_Pow2_Mono_Strict (X : Long_Integer;
                                   J : Natural;
                                   K : Natural)
   is
   begin
      pragma Assert (J < 63);
      pragma Assert (2**J <= 2**63 - 1);
      --  WORKAROUND: Componolit/Workarounds#15
      pragma Annotate (GNATprove, False_Positive, "assertion",
                       "B > 1 and E1 < E2 => B**E1 < B**E2 => B*E1 <= B**E2 - 1");

      Exp_Div (2, J, K);

      if X / 2**K >= 2**J / 2**K then
         Mult_Mono (2**J / 2**K, X / 2**K, 2**K);
      end if;

      pragma Assert (2**K * (2**J / 2**K) = 2**K * 2**(J - K));
   end Div_Pow2_Mono_Strict;

   procedure Exp_Mult (Base  : Long_Integer;
                       Exp_1 : Natural;
                       Exp_2 : Natural)
   is
   begin
      null;
   end Exp_Mult;

   procedure Exp_Div (Base  : Long_Integer;
                      Exp_1 : Natural;
                      Exp_2 : Natural)
   is
   begin
      Exp_Mult (Base, Exp_1 - Exp_2, Exp_2);
      Mult_Div_Id (Base**(Exp_1 - Exp_2), Base**Exp_2);
   end Exp_Div;

   procedure Right_Shift_Limit (X : Long_Integer;
                                J : Natural;
                                K : Natural)
   is
   begin
      pragma Assert (J < 63);
      pragma Assert (2**J <= 2**63 - 1);
      --  WORKAROUND: Componolit/Workarounds#15
      pragma Annotate (GNATprove, False_Positive, "assertion",
                       "B > 1 and E1 < E2 => B**E1 < B**E2 => B*E1 <= B**E2 - 1");

      pragma Assert (K < 63);
      pragma Assert (2**K <= 2**63 - 1);
      --  WORKAROUND: Componolit/Workarounds#15
      pragma Annotate (GNATprove, False_Positive, "assertion",
                       "B > 1 and E1 < E2 => B**E1 < B**E2 => B*E1 <= B**E2 - 1");

      Exp_Mult (2, J, K);
      Div_Pow2_Mono_Strict (X, J + K, K);
      Exp_Div (2, J + K, K);
   end Right_Shift_Limit;

   procedure Left_Shift_Limit (X : Long_Integer;
                               J : Natural;
                               K : Natural)
   is
   begin
      Mult_Mono (X + 1, 2**J, 2**K);
      Exp_Mult (2, J, K);
   end Left_Shift_Limit;

end {prefix}Lemmas;
