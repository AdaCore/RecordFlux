
with System.Unsigned_Types;

package body Generic_Checksum with
   SPARK_Mode
is

   package SU renames System.Unsigned_Types;

   function Shl (C : ICMP.Checksum) return ICMP.Checksum is
      (ICMP.Checksum (SU.Shift_Left (SU.Short_Unsigned (C), 8)));

   function To_Checksum (B : Types.Byte) return ICMP.Checksum is
      (ICMP.Checksum'Val (Types.Byte'Pos (B)));

   function Echo_Request_Reply_Checksum (Tag             : ICMP.Tag;
                                         Code            : ICMP.Code_Zero;
                                         Identifier      : ICMP.Identifier;
                                         Sequence_Number : ICMP.Sequence_Number;
                                         Data            : Types.Bytes) return ICMP.Checksum
   is
      use type ICMP.Checksum;
      use type Types.Index;
      Checksum : ICMP.Checksum := Shl (ICMP.Checksum (ICMP.To_Base (Tag)))
                                      + ICMP.Checksum (ICMP.To_Base (Code));
      Index    : Types.Index := Data'First;
   begin
      Checksum := Add (Checksum, Add (ICMP.Checksum (Identifier), ICMP.Checksum (Sequence_Number)));
      while Index < Data'Last loop
         Checksum := Add (Checksum, Shl (To_Checksum (Data (Index))) + To_Checksum (Data (Index + 1)));
         Index    := Index + 2;
      end loop;
      if Index = Data'Last then
         Checksum := Add (Checksum, Shl (To_Checksum (Data (Index))));
      end if;
      return not Checksum;
   end Echo_Request_Reply_Checksum;

   function Add (C1 : ICMP.Checksum;
                 C2 : ICMP.Checksum) return ICMP.Checksum
   is
      use type SU.Unsigned;
      Ch32 : SU.Unsigned;
   begin
      Ch32 := SU.Unsigned (C1) + SU.Unsigned (C2);
      if Ch32 > SU.Unsigned (ICMP.Checksum'Last) then
         Ch32 := Ch32 + 1;
      end if;
      return ICMP.Checksum (Ch32 and SU.Unsigned (ICMP.Checksum'Last));
   end Add;

end Generic_Checksum;
