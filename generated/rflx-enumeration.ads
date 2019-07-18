with RFLX.Types;
use type RFLX.Types.Bytes, RFLX.Types.Bytes_Ptr, RFLX.Types.Index_Type, RFLX.Types.Length_Type, RFLX.Types.Bit_Index_Type, RFLX.Types.Bit_Length_Type;

package RFLX.Enumeration with
  SPARK_Mode
is

   type Priority_Base is mod 2**3;

   type Priority_Enum is (LOW, MEDIUM, HIGH) with
     Size =>
       3;
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
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Convert is new RFLX.Types.Convert_To_Mod (Priority_Base);

   pragma Warnings (Off, "unused variable ""Value""");

   function Valid (Value : Priority_Base) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Value""");

   function Convert (Value : Priority_Base) return Priority is
     ((case Value is
         when 1 =>
            (True, LOW),
         when 4 =>
            (True, MEDIUM),
         when 7 =>
            (True, HIGH),
         when others =>
            (False, Value)))
    with
     Pre =>
       Valid (Value);

   function Convert (Enum : Priority_Enum) return Priority_Base is
     ((case Enum is
         when LOW =>
            1,
         when MEDIUM =>
            4,
         when HIGH =>
            7));

end RFLX.Enumeration;
