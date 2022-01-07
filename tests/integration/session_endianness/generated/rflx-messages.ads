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

end RFLX.Messages;
