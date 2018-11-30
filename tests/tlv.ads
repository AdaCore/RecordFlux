with Types; use Types;

package TLV
  with SPARK_Mode
is

   type Tag_Type_Base is mod (2**2);
   function Convert_To_Tag_Type_Base is new Convert_To_Mod (Tag_Type_Base);

   type Tag_Type is (Msg_Data, Msg_Error) with Size => 2;
   for Tag_Type use (Msg_Data => 1, Msg_Error => 3);

   type Length_Type is mod (2**14);
   function Convert_To_Length_Type is new Convert_To_Mod (Length_Type);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Tag_Type return Tag_Type is
      (Tag_Type'First)
     with
       Pre => False;

   function Unreachable_Length_Type return Length_Type is
      (Length_Type'First)
     with
       Pre => False;

   function Unreachable_Natural return Natural is
      (Natural'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Valid_Tag_Type (Buffer : Bytes; Offset : Natural) return Boolean is
      (case Convert_To_Tag_Type_Base (Buffer, Offset) is when 1 | 3 => True, when others => False)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Tag_Type_Base'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_Tag_Type (Buffer : Bytes; Offset : Natural) return Tag_Type is
      (case Convert_To_Tag_Type_Base (Buffer, Offset) is when 1 => Msg_Data, when 3 => Msg_Error, when others => Unreachable_Tag_Type)
     with
       Pre => ((Offset < 8 and then Buffer'Length = (((Tag_Type_Base'Size + Offset + (-1)) / 8) + 1)) and then Valid_Tag_Type (Buffer, Offset));

end TLV;
