package RFLX.Builtin_Types.Conversions is

   pragma Annotate (GNATprove, Terminating, Conversions);

   function Valid (Val : Boolean_Base) return Boolean is
     (case Val is
         when 0 | 1 =>
            True,
         when others =>
            False);

   function Convert (Enum : Boolean) return Boolean_Base is
     (case Enum is
         when False =>
            0,
         when True =>
            1);

   function Convert (Val : Boolean_Base) return Boolean is
     (case Val is
         when 0 =>
            False,
         when 1 =>
            True)
    with
     Pre =>
       Valid (Val);

end RFLX.Builtin_Types.Conversions;
