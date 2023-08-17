with Interfaces;

package body Generic_Checksum with
   SPARK_Mode
is

   package Int renames Interfaces;

   function Shift_Left (C : ICMP.Checksum) return ICMP.Checksum is
      (ICMP.Checksum (Int.Shift_Left (Int.Unsigned_16 (C), 8)));

   function To_Checksum (B : Types.Byte) return ICMP.Checksum is
      (ICMP.Checksum'Val (Types.Byte'Pos (B)));

   function Echo_Request_Reply_Checksum (Tag             : ICMP.Tag;
                                         Code            : ICMP.Code_Zero;
                                         Identifier      : ICMP.Identifier;
                                         Sequence_Number : ICMP.Sequence_Number;
                                         Data            : Types.Bytes) return ICMP.Checksum
   is
      use type Interfaces.Unsigned_16;
      use type ICMP.Checksum;
      use type Types.Index;
      Checksum : ICMP.Checksum := Shift_Left (ICMP.Checksum (ICMP.To_Base_Integer (Tag)))
                                  + ICMP.Checksum (ICMP.To_Base_Integer (Code));
      Index    : Types.Index;
   begin
      Checksum := Add (Checksum, Add (ICMP.Checksum (Identifier), ICMP.Checksum (Sequence_Number)));
      if Data'Length > 0 then
         Index := Data'First;
         while Index < Data'Last loop
            pragma Loop_Invariant (Index in Data'Range);
            pragma Loop_Variant (Increases => Index);
            Checksum := Add (Checksum, Shift_Left (To_Checksum (Data (Index))) + To_Checksum (Data (Index + 1)));
            exit when Index >= Data'Last - 1;
            Index := Index + 2;
         end loop;
         if Index = Data'Last then
            Checksum := Add (Checksum, Shift_Left (To_Checksum (Data (Index))));
         end if;
      end if;
      return ICMP.Checksum (not Interfaces.Unsigned_16 (Checksum));
   end Echo_Request_Reply_Checksum;

   function Add (C1 : ICMP.Checksum;
                 C2 : ICMP.Checksum) return ICMP.Checksum
   is
      use type Int.Unsigned_32;
      Ch32 : Int.Unsigned_32;
   begin
      Ch32 := Int.Unsigned_32 (C1) + Int.Unsigned_32 (C2);
      if Ch32 > Int.Unsigned_32 (ICMP.Checksum'Last) then
         Ch32 := Ch32 + 1;
      end if;
      return ICMP.Checksum (Ch32 and Int.Unsigned_32 (ICMP.Checksum'Last));
   end Add;

end Generic_Checksum;
