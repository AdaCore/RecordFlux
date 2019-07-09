with Ada.Text_IO;

package body Types
  with SPARK_Mode => Off
is

   procedure Bytes_Put (Buffer : Bytes)
     with SPARK_Mode => Off
   is
      package Modular_Text_IO is new Ada.Text_IO.Modular_IO (Byte);
      S : String (1 .. 6);
   begin
      for I in Length_Type range Buffer'First .. Buffer'Last loop
         Modular_Text_IO.Put(To => S, Item => Buffer (I), Base => 16);
         if S (4) = '#' then
            Ada.Text_IO.Put ("0" & S (5) & " ");
         else
            Ada.Text_IO.Put (S (4 .. 5) & " ");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Bytes_Put;

   function Convert_To_Mod (Buffer : Bytes; Offset : Natural := 0) return Int
   is
      Current : Byte;
      Carry : Byte := 0;
      Next_Carry : Byte;
      Value : Int := 0;
      Fraction : Boolean := (Int'Size + Offset) mod 8 /= 0;
   begin
      for I in reverse Length_Type range 0 .. Buffer'Length - 1 loop
         Current := Buffer (Buffer'First + (Buffer'Length - I - 1));
         if Fraction and I = Buffer'Length - 1 then
            Current := Current and (2**((Int'Size + Offset) mod 8) - 1);
         end if;
         if Offset > 0 then
            Next_Carry := Current and (2**Offset - 1);
            Current := Current / 2**Offset + Carry * 2**(8 - Offset);
            Carry := Next_Carry;
         end if;
         if I < (Int'Size - 1) / 8 + 1 then
            Value := Value + Int (Current) * Int (256**Natural (I));
         end if;
      end loop;
      return Value;
   end Convert_To_Mod;

   function Convert_To_Int (Buffer : Bytes; Offset : Natural := 0) return Int
   is
      Current : Byte;
      Carry : Byte := 0;
      Next_Carry : Byte;
      Value : Int := 0;
      Fraction : Boolean := (Int'Size + Offset) mod 8 /= 0;
   begin
      for I in reverse Length_Type range 0 .. Buffer'Length - 1 loop
         Current := Buffer (Buffer'First + (Buffer'Length - I - 1));
         if Fraction and I = Buffer'Length - 1 then
            Current := Current and (2**((Int'Size + Offset) mod 8) - 1);
         end if;
         if Offset > 0 then
            Next_Carry := Current and (2**Offset - 1);
            Current := Current / 2**Offset + Carry * 2**(8 - Offset);
            Carry := Next_Carry;
         end if;
         if I < (Int'Size - 1) / 8 + 1 then
            Value := Value + Int (Current) * Int (256**Natural (I));
         end if;
      end loop;
      return Value;
   end Convert_To_Int;

end Types;
