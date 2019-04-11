with Types;
use type Types.Bytes, Types.Index_Type, Types.Length_Type;

package Enumeration
  with SPARK_Mode
is

   type Priority_Base is mod (2**3);

   type Priority_Enum is (LOW, MEDIUM, HIGH) with Size => 3;
   for Priority_Enum use (LOW => 1, MEDIUM => 4, HIGH => 7);

   type Priority (Known : Boolean := False) is
      record
         case Known is
            when True =>
               Enum : Priority_Enum;
            when False =>
               Raw : Priority_Base;
         end case;
      end record;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Priority return Priority is
      ((False, Priority_Base'First))
     with
       Pre => False;

   function Unreachable_Types_Length_Type return Types.Length_Type is
      (Types.Length_Type'First)
     with
       Pre => False;

   pragma Warnings (On, "precondition is statically false");

   function Convert_To_Priority_Base is new Types.Convert_To_Mod (Priority_Base);

   function Valid_Priority (Buffer : Types.Bytes; Offset : Natural) return Boolean is
      (True)
     with
       Pre => (Offset < 8 and then Buffer'Length = (((Priority_Base'Size + Offset + (-1)) / 8) + 1));

   function Convert_To_Priority (Buffer : Types.Bytes; Offset : Natural) return Priority
     with
       Pre => ((Offset < 8 and then Buffer'Length = (((Priority_Base'Size + Offset + (-1)) / 8) + 1)) and then Valid_Priority (Buffer, Offset));

   function Convert_To_Priority_Base (Enum : Priority_Enum) return Priority_Base is
      (case Enum is when LOW => 1, when MEDIUM => 4, when HIGH => 7);

end Enumeration;
