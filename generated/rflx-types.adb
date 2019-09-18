with RFLX.Lemmas;

package body RFLX.Types with
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

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      pragma Assert (Element_Type'Pos (Element_Type'First) = 0);
      pragma Assert (Element_Type'Pos (Element_Type'Last) = 2**Element_Type'Size - 1);

      LSB_Offset : constant Long_Integer := Offset_Type'Pos (Offset);

      --  Index pointing to least significant element
      Least_Significant_Index : constant Long_Integer := LSB_Offset / Element_Type'Size;

      --  Bits the least significant element (LSE) is shifted left relative to a single element
      LSE_Offset : constant Natural := Natural (LSB_Offset mod Element_Type'Size);

      --  Index pointing to most significant index
      Most_Significant_Index : constant Long_Integer :=
        (LSB_Offset + Long_Integer (Value_Type'Size) - 1) / Element_Type'Size;

      --  Bits the most significant element (MSE) is shifted right relative to a single element
      MSE_Offset : constant Natural := Element_Type'Size - LSE_Offset;

      function D (Index : Long_Integer) return Element_Type with
        Pre => Index >= 0 and then Index < Data'Length;

      function D (Index : Long_Integer) return Element_Type
      is
         function ES return Natural is (Element_Type'Size) with Post => ES'Result = Element_Type'Size;
         E : constant Natural := (LSE_Offset + Value_Type'Size + Element_Type'Size - 1) mod ES + 1;
         pragma Assert (2**Element_Type'Size = 2**ES);
      begin
         declare
            Mask : constant Long_Integer := (if Index < Most_Significant_Index then 2**ES else 2**E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Index)));
         begin
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

      type Result_Type is mod 2**Long_Integer'Size;
      Result : Result_Type := 0;

      Pow2_LSE_Offset : constant Long_Integer := 2**LSE_Offset;

   begin
      pragma Assert (Value_Type'Size - Value_Type'Size mod Element_Type'Size
                     = Element_Type'Size * (Value_Type'Size / Element_Type'Size));

      for I in Least_Significant_Index .. Most_Significant_Index - 1
      loop
         declare
            D_Current : constant Element_Type := D (I);
            D_Next    : constant Element_Type := D (I + 1);
         begin
            Lemmas.Mult_Limit (Element_Type'Pos (D_Next) mod Pow2_LSE_Offset, LSE_Offset, 2**MSE_Offset, MSE_Offset);
            Lemmas.Mult_Ge_0 (Element_Type'Pos (D_Next) mod Pow2_LSE_Offset, 2**MSE_Offset);
            declare
               Current : constant Long_Integer := Element_Type'Pos (D_Current) / Pow2_LSE_Offset;
               Next    : constant Long_Integer := Element_Type'Pos (D_Next) mod Pow2_LSE_Offset * 2**MSE_Offset;
            begin
               Result := Result
                 + (Result_Type (Current) + Result_Type (Next))
                 * 2**(Element_Type'Size * Natural (I - Least_Significant_Index));
            end;
         end;
      end loop;

      Result := Result
        + 2**(Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index))
        * Result_Type (Element_Type'Pos (D (Most_Significant_Index)) / Pow2_LSE_Offset);
      return Value_Type'Val (Result mod 2**Value_Type'Size);
   end Extract;

end RFLX.Types;
