pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

pragma Warnings (Off, """Ada.Numerics.Big_Numbers.Big_Integers"" is an Ada 202x unit");

with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

--  WORKAROUND: Componolit/Workarounds#26
pragma Warnings (Off, "no Global contract available for ""*""");

package {prefix}RFLX_Lemmas with
  SPARK_Mode,
  Ghost
is

   procedure Mult_Limit (Value_1 : Big_Integer;
                         Exp_1   : Natural;
                         Value_2 : Big_Integer;
                         Exp_2   : Natural) with
     Pre  => To_Big_Integer (0) <= Value_2
             and Value_1 <= To_Big_Integer (2)**Exp_1
             and Value_2 <= To_Big_Integer (2)**Exp_2
             and Exp_1 <= Natural'Last - Exp_2,
     Post => Value_1 * Value_2 <= To_Big_Integer (2)**(Exp_1 + Exp_2),
     Ghost;

   procedure Mult_Ge_0 (Factor_1 : Big_Integer;
                        Factor_2 : Big_Integer) with
     Pre  => Factor_1 >= To_Big_Integer (0) and Factor_2 >= To_Big_Integer (0),
     Post => Factor_1 * Factor_2 >= To_Big_Integer (0),
     Ghost;

   procedure Mult_Mono (X : Big_Integer;
                        Y : Big_Integer;
                        Z : Big_Integer) with
     Pre  => To_Big_Integer (0) <= Z and X <= Y,
     Post => Z * X <= Z * Y,
     Ghost;

   procedure Mult_Div_Id (X : Big_Integer;
                          Y : Big_Integer) with
     Pre  => To_Big_Integer (0) <= X and To_Big_Integer (0) < Y,
     Post => X * Y / Y = X,
     Ghost;

   procedure Div_Pow2_Mono_Strict (X : Big_Integer;
                                   J : Natural;
                                   K : Natural) with
     Pre  => X >= To_Big_Integer (0) and X < To_Big_Integer (2)**J and J >= K and J > 0,
     Post => X / To_Big_Integer (2)**K < To_Big_Integer (2)**J / To_Big_Integer (2)**K,
     Ghost;

   procedure Exp_Mult (Base  : Big_Integer;
                       Exp_1 : Natural;
                       Exp_2 : Natural) with
     Pre => Exp_1 <= Natural'Last - Exp_2,
     Post => Base**Exp_1 * Base**Exp_2 = Base**(Exp_1 + Exp_2),
     Ghost;

   procedure Exp_Div (Base  : Big_Integer;
                      Exp_1 : Natural;
                      Exp_2 : Natural) with
     Pre  => To_Big_Integer (0) < Base
             and then Exp_2 <= Exp_1,
     Post => Base**Exp_1 / Base**Exp_2 = Base**(Exp_1 - Exp_2),
     Ghost;

   procedure Right_Shift_Limit (X : Big_Integer;
                                J : Natural;
                                K : Natural) with
     Pre  => J <= Natural'Last - K
             and then J + K > 0
             and then X >= To_Big_Integer (0)
             and then X < To_Big_Integer (2)**(J + K),
     Post => X / To_Big_Integer (2)**K < To_Big_Integer (2)**J,
     Ghost;

   procedure Left_Shift_Limit (X : Big_Integer;
                               J : Natural;
                               K : Natural) with
     Pre  => X < To_Big_Integer (2)**J and J <= Natural'Last - K,
     Post => X * To_Big_Integer (2)**K <= To_Big_Integer (2)**(J + K) - To_Big_Integer (2)**K,
     Ghost;

end {prefix}RFLX_Lemmas;
