with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index, RFLX.Types.Length, RFLX.Types.Bit_Index, RFLX.Types.Bit_Length;

package RFLX.TLV with
  SPARK_Mode
is

   type Tag_Base is mod 2**2;

   type Tag is (Msg_Data, Msg_Error) with
     Size =>
       2;
   for Tag use (Msg_Data => 1, Msg_Error => 3);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Tag return Tag is
     (Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Tag_Base);

   function Valid (Value : Tag_Base) return Boolean is
     ((case Value is
         when 1 | 3 =>
            True,
         when others =>
            False));

   function Convert (Value : Tag_Base) return Tag is
     ((case Value is
         when 1 =>
            Msg_Data,
         when 3 =>
            Msg_Error,
         when others =>
            Unreachable_Tag))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Tag) return Tag_Base is
     ((case Enum is
         when Msg_Data =>
            1,
         when Msg_Error =>
            3));

   type Length is mod 2**14;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length return Length is
     (Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Length);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Length) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Length) return Length is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.TLV;
