pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.Messages with
  SPARK_Mode
is

   type Integer is mod 2**32 with
     Size =>
       32;

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.Messages.Integer) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.Messages.Integer) return RFLX.Messages.Integer is
     (Val);

   function To_Actual (Val : RFLX.Messages.Integer) return RFLX.Messages.Integer is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Enum_T_Base is mod 2**32;

   type Enum_T is (Enum_A, Enum_B, Enum_C, Enum_D, Enum_E, Enum_F, Enum_G) with
     Size =>
       32;
   for Enum_T use (Enum_A => 0, Enum_B => 1, Enum_C => 2, Enum_D => 4, Enum_E => 8, Enum_F => 16, Enum_G => 32);

   function Valid (Val : RFLX.Messages.Enum_T_Base) return Boolean is
     ((case Val is
          when 0 | 1 | 2 | 4 | 8 | 16 | 32 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.Messages.Enum_T) return RFLX.Messages.Enum_T_Base is
     ((case Enum is
          when Enum_A =>
             0,
          when Enum_B =>
             1,
          when Enum_C =>
             2,
          when Enum_D =>
             4,
          when Enum_E =>
             8,
          when Enum_F =>
             16,
          when Enum_G =>
             32));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.Messages.Enum_T_Base) return RFLX.Messages.Enum_T is
     ((case Val is
          when 0 =>
             Enum_A,
          when 1 =>
             Enum_B,
          when 2 =>
             Enum_C,
          when 4 =>
             Enum_D,
          when 8 =>
             Enum_E,
          when 16 =>
             Enum_F,
          when 32 =>
             Enum_G,
          when others =>
             raise Program_Error))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

end RFLX.Messages;
