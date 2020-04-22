pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package {prefix}RFLX_Builtin_Types.Conversions with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Conversions);

   function Valid (Val : Boolean_Base) return Boolean is
     (case Val is
         when 0 | 1 =>
            True);

   function To_Base (Enum : Boolean) return Boolean_Base is
     (case Enum is
         when False =>
            0,
         when True =>
            1);

   function To_Actual (Val : Boolean_Base) return Boolean is
     (case Val is
         when 0 =>
            False,
         when 1 =>
            True)
    with
     Pre =>
       Valid (Val);

end {prefix}RFLX_Builtin_Types.Conversions;
