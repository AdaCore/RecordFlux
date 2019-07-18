with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index_Type, RFLX.Types.Length_Type, RFLX.Types.Bit_Index_Type, RFLX.Types.Bit_Length_Type;

package RFLX.TLV with
  SPARK_Mode
is

   type Tag_Type_Base is mod 2**2;

   type Tag_Type is (Msg_Data, Msg_Error) with
     Size =>
       2;
   for Tag_Type use (Msg_Data => 1, Msg_Error => 3);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Tag_Type return Tag_Type is
     (Tag_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Tag_Type_Base);

   function Valid (Value : Tag_Type_Base) return Boolean is
     ((case Value is
         when 1 | 3 =>
            True,
         when others =>
            False));

   function Convert (Value : Tag_Type_Base) return Tag_Type is
     ((case Value is
         when 1 =>
            Msg_Data,
         when 3 =>
            Msg_Error,
         when others =>
            Unreachable_Tag_Type))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Tag_Type) return Tag_Type_Base is
     ((case Enum is
         when Msg_Data =>
            1,
         when Msg_Error =>
            3));

   type Length_Type is mod 2**14;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Length_Type return Length_Type is
     (Length_Type'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Length_Type);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Length_Type) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Length_Type) return Length_Type is
     (Value)
    with
     Pre =>
       Valid (Value);

end RFLX.TLV;
