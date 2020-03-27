package RFLX.TLV with
  SPARK_Mode
is

   type Tag_Base is mod 2**2;

   type Tag is (Msg_Data, Msg_Error) with
     Size =>
       2;
   for Tag use (Msg_Data => 1, Msg_Error => 3);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TLV_Tag return RFLX.TLV.Tag is
     (RFLX.TLV.Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : RFLX.TLV.Tag_Base) return Boolean is
     ((case Val is
         when 1 | 3 =>
            True,
         when others =>
            False));

   function Convert (Enum : RFLX.TLV.Tag) return RFLX.TLV.Tag_Base is
     ((case Enum is
         when Msg_Data =>
            1,
         when Msg_Error =>
            3));

   function Convert (Val : RFLX.TLV.Tag_Base) return RFLX.TLV.Tag is
     ((case Val is
         when 1 =>
            Msg_Data,
         when 3 =>
            Msg_Error,
         when others =>
            Unreachable_TLV_Tag))
    with
     Pre =>
       Valid (Val);

   type Length is mod 2**14;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_TLV_Length return RFLX.TLV.Length is
     (RFLX.TLV.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : RFLX.TLV.Length) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : RFLX.TLV.Length) return RFLX.TLV.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

end RFLX.TLV;
