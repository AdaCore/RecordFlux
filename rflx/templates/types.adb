package body {prefix}Types with
  SPARK_Mode
is

   function Bytes_Address (Buffer : Bytes_Ptr) return Integer_Address with
     SPARK_Mode => Off
   is
   begin
      return Integer_Address (System.Storage_Elements.To_Integer (Buffer.all'Address));
   end Bytes_Address;

   function Convert_To_Mod (Buffer : Bytes; Offset : Types.Offset := 0) return Int with
     SPARK_Mode => Off
   is
      Current : Byte;
      Carry : Byte := 0;
      Next_Carry : Byte;
      Value : Int := 0;
      Fraction : Boolean := (Int'Size + Natural (Offset)) mod 8 /= 0;
   begin
      for I in reverse Length range 0 .. Buffer'Length - 1 loop
         Current := Buffer (Buffer'First + (Buffer'Length - I - 1));
         if Fraction and I = Buffer'Length - 1 then
            Current := Current and (2**((Int'Size + Natural (Offset)) mod 8) - 1);
         end if;
         if Offset > 0 then
            Next_Carry := Current and (2**Natural (Offset) - 1);
            Current := Current / 2**Natural (Offset) + Carry * 2**(8 - Natural (Offset));
            Carry := Next_Carry;
         end if;
         if I < (Int'Size - 1) / 8 + 1 then
            Value := Value + Int (Current) * Int (256**Natural (I));
         end if;
      end loop;
      return Value;
   end Convert_To_Mod;

   function Convert_To_Int (Buffer : Bytes; Offset : Types.Offset := 0) return Int with
     SPARK_Mode => Off
   is
      Current : Byte;
      Carry : Byte := 0;
      Next_Carry : Byte;
      Value : Int := 0;
      Fraction : Boolean := (Int'Size + Natural (Offset)) mod 8 /= 0;
   begin
      for I in reverse Length range 0 .. Buffer'Length - 1 loop
         Current := Buffer (Buffer'First + (Buffer'Length - I - 1));
         if Fraction and I = Buffer'Length - 1 then
            Current := Current and (2**((Int'Size + Natural (Offset)) mod 8) - 1);
         end if;
         if Offset > 0 then
            Next_Carry := Current and (2**Natural (Offset) - 1);
            Current := Current / 2**Natural (Offset) + Carry * 2**(8 - Natural (Offset));
            Carry := Next_Carry;
         end if;
         if I < (Int'Size - 1) / 8 + 1 then
            Value := Value + Int (Current) * Int (256**Natural (I));
         end if;
      end loop;
      return Value;
   end Convert_To_Int;

end {prefix}Types;
