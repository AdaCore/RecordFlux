pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with {prefix}RFLX_Lemmas;

pragma Warnings (Off, """Ada.Numerics.Big_Numbers.Big_Integers"" is an Ada 202x unit");

with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

package body {prefix}RFLX_Generic_Types is

   package Conversions is new Signed_Conversions (Long_Integer);

   function Extract (Data : Bytes;
                     Ofst : Offset) return Value
   is
      pragma Compile_Time_Error (Value'Pos (Value'Last) - Value'Pos (Value'First) + 1 /= 2**Value'Size,
                                 "Value must cover entire value range");

      pragma Assert (Byte'Pos (Byte'First) = 0);
      pragma Assert (Byte'Pos (Byte'Last) = 2**Byte'Size - 1);

      LSB_Offset : constant Long_Integer := Offset'Pos (Ofst);

      --  Index pointing to least significant element
      Least_Significant_Index : constant Long_Integer := LSB_Offset / Byte'Size;

      --  Bits the least significant element (LSE) is shifted left relative to a single element
      LSE_Offset : constant Natural := Natural (LSB_Offset mod Byte'Size);

      --  Index pointing to most significant index
      Most_Significant_Index : constant Long_Integer :=
        (LSB_Offset + Long_Integer (Value'Size) - 1) / Byte'Size;

      --  Bits the most significant element (MSE) is shifted right relative to a single element
      MSE_Offset : constant Natural := Byte'Size - LSE_Offset;

      function D (Idx : Long_Integer) return Byte with
        Pre =>
          Idx >= 0 and then Idx < Data'Length;

      function D (Idx : Long_Integer) return Byte
      is
         function ES return Natural is (Byte'Size) with Post => ES'Result = Byte'Size;
         E : constant Natural := (LSE_Offset + Value'Size + Byte'Size - 1) mod ES + 1;
         pragma Assert (2**Byte'Size = 2**ES);
      begin
         declare
            Mask : constant Long_Integer := (if Idx < Most_Significant_Index then 2**ES else 2**E);
            Val  : constant Byte := Data (Index'Val ((Index'Pos (Data'Last) - Idx)));
         begin
            return Byte'Val (Byte'Pos (Val) mod Mask);
         end;
      end D;

      type Result_Type is mod 2**Long_Integer'Size;
      Result : Result_Type := 0;

      function LIS return Natural is (Long_Integer'Size) with Post => LIS'Result = Long_Integer'Size;
      pragma Assert (2**(LIS - 2) - 1 = 2**(Long_Integer'Size - 2) - 1);
      --  WORKAROUND: Componolit/Workarounds#15
      pragma Annotate (GNATprove, False_Positive, "assertion",
                       "solvers cannot show correspondence between integers and exponentiation abstraction");
      pragma Annotate (GNATprove, False_Positive, "overflow",
                       "solvers cannot show that overflow does not occur for 2**62");
      Pow2_LSE_Offset : constant Long_Integer := 2**LSE_Offset;

   begin
      pragma Assert (Value'Size - Value'Size mod Byte'Size
                     = Byte'Size * (Value'Size / Byte'Size));

      for I in Least_Significant_Index .. Most_Significant_Index - 1
      loop
         declare
            D_Current : constant Byte := D (I);
            D_Next    : constant Byte := D (I + 1);
         begin
            --  WORKAROUND: Componolit/Workarounds#26
            pragma Warnings (Off, "no Global contract available for ""*""");
            RFLX_Lemmas.Mult_Limit (Conversions.To_Big_Integer (Byte'Pos (D_Next) mod Pow2_LSE_Offset),
                                    LSE_Offset,
                                    Conversions.To_Big_Integer (2)**MSE_Offset,
                                    MSE_Offset);
            RFLX_Lemmas.Mult_Ge_0 (Conversions.To_Big_Integer (Byte'Pos (D_Next) mod Pow2_LSE_Offset), To_Big_Integer (2)**MSE_Offset);
            pragma Warnings (On, "no Global contract available for ""*""");
            declare
               Current : constant Long_Integer := Byte'Pos (D_Current) / Pow2_LSE_Offset;
               Next    : constant Long_Integer := Byte'Pos (D_Next) mod Pow2_LSE_Offset * 2**MSE_Offset;
            begin
               Result := Result
                 + (Result_Type (Current) + Result_Type (Next))
                 * 2**(Byte'Size * Natural (I - Least_Significant_Index));
            end;
         end;
      end loop;

      Result := Result
        + 2**(Byte'Size * Natural (Most_Significant_Index - Least_Significant_Index))
        * Result_Type (Byte'Pos (D (Most_Significant_Index)) / Pow2_LSE_Offset);
      return Value'Val (Result mod 2**Value'Size);
   end Extract;

   procedure Insert (Val  :        Value;
                     Data : in out Bytes;
                     Ofst :        Offset)
   is
      pragma Compile_Time_Error (Value'Pos (Value'Last) - Value'Pos (Value'First) + 1 /= 2**Value'Size,
                                 "Value must cover entire value range");

      pragma Assert (Byte'Pos (Byte'First) = 0);
      pragma Assert (Byte'Pos (Byte'Last) = 2**Byte'Size - 1);

      LSB_Offset : constant Long_Integer := Offset'Pos (Ofst);

      --  Index pointing to least significant element
      Least_Significant_Index : constant Long_Integer := LSB_Offset / Byte'Size;

      pragma Assert (Least_Significant_Index >= 0 and then Least_Significant_Index < Data'Length);

      --  Bits the least significant element (LSE) is shifted left relative to a single element
      LSE_Offset : constant Natural := Natural (LSB_Offset mod Byte'Size);

      --  Index pointing to most significant index
      Most_Significant_Index : constant Long_Integer :=
        (LSB_Offset + Long_Integer (Value'Size) - 1) / Byte'Size;

      function ES return Natural is (Byte'Size) with Post => ES'Result = Byte'Size;

      --  Bits of least significant element (LSE)
      LSE_Bits   : constant Natural := Byte'Size - LSE_Offset;

      --  Bits of most significant element (MSE)
      MSE_Bits   : constant Natural := (LSE_Offset + Value'Size + Byte'Size - 1) mod ES + 1;

      --  Bits the most significant element (MSE) is shifted right relative to a single element
      MSE_Offset : constant Natural := Byte'Size - MSE_Bits;

      function Read (Idx : Long_Integer) return Byte with
        Pre =>
          Idx >= 0 and then Idx < Data'Length
      is
      begin
         return Data (Index'Val ((Index'Pos (Data'Last) - Idx)));
      end Read;

      procedure Write (Idx : Long_Integer; Element : Byte) with
        Pre =>
          Idx >= 0 and then Idx < Data'Length
      is
      begin
         Data (Index'Val ((Index'Pos (Data'Last) - Idx))) := Element;
      end Write;

      function LIS return Natural is (Long_Integer'Size) with Post => LIS'Result = Long_Integer'Size;
      pragma Assert (2**(LIS - 2) - 1 = 2**(Long_Integer'Size - 2) - 1);
      --  WORKAROUND: Componolit/Workarounds#15
      pragma Annotate (GNATprove, False_Positive, "assertion",
                       "solvers cannot show correspondence between integers and exponentiation abstraction");
      pragma Annotate (GNATprove, False_Positive, "overflow",
                       "solvers cannot show that overflow does not occur for 2**62");

      Pow2_LSE_Offset : constant Long_Integer := 2**LSE_Offset;
      Pow2_LSE_Bits   : constant Long_Integer := 2**LSE_Bits;

      pragma Assert (ES < Long_Integer'Size - 2);
      pragma Assert (2**ES = 2**Byte'Size);
      --  WORKAROUND: Componolit/Workarounds#15
      pragma Annotate (GNATprove, False_Positive, "assertion",
                       "solvers cannot show correspondence between integers and exponentiation abstraction");
      pragma Annotate (GNATprove, False_Positive, "overflow",
                       "solvers cannot show that overflow does not occur when exponent is lower than (Long_Integer'Size - 2)");

      V : Long_Integer;
   begin
      if Least_Significant_Index = Most_Significant_Index then
         declare
            LR_Value      : constant Long_Integer := Byte'Pos (Read (Least_Significant_Index)) mod 2**LSE_Offset;
            Element_Value : constant Long_Integer := (Value'Pos (Val) mod 2**Value'Size);
            UR_Offset     : constant Natural := LSE_Offset + Value'Size;
            UR_Value      : constant Long_Integer := Byte'Pos (Read (Most_Significant_Index)) / 2**UR_Offset;
         begin
            --  WORKAROUND: Componolit/Workarounds#26
            pragma Warnings (Off, "no Global contract available for ""*""");
            RFLX_Lemmas.Mult_Ge_0 (Conversions.To_Big_Integer (Element_Value), Conversions.To_Big_Integer (Pow2_LSE_Offset));
            RFLX_Lemmas.Mult_Limit (Conversions.To_Big_Integer (Element_Value), LSE_Bits, Conversions.To_Big_Integer (Pow2_LSE_Offset), LSE_Offset);
            pragma Assert (Element_Value * Pow2_LSE_Offset <= 2**(LSE_Bits + LSE_Offset));
            --  WORKAROUND: Componolit/Workarounds#15
            pragma Annotate (GNATprove, False_Positive, "assertion",
                             "direct re-expression of lemma postcondition");
            pragma Annotate (GNATprove, False_Positive, "overflow check",
                             "consequence of lemma postcondition");
            RFLX_Lemmas.Left_Shift_Limit (Conversions.To_Big_Integer (Element_Value), Value'Size, LSE_Offset);
            RFLX_Lemmas.Right_Shift_Limit (Conversions.To_Big_Integer (Byte'Pos (Read (Most_Significant_Index))), ES
                                           - UR_Offset, UR_Offset);
            RFLX_Lemmas.Left_Shift_Limit (Conversions.To_Big_Integer (UR_Value), ES - UR_Offset, UR_Offset);
            pragma Warnings (On, "no Global contract available for ""*""");
            pragma Assert (LR_Value in 0 .. 2**LSE_Offset - 1);
            pragma Assert (Element_Value * Pow2_LSE_Offset in 0 .. 2**UR_Offset);
            pragma Assert (UR_Value in 0 .. 2**(ES - UR_Offset) - 1);
            pragma Assert (UR_Value * 2**UR_Offset in 0 .. 2**ES - 1);
            pragma Assert (LR_Value + Element_Value * Pow2_LSE_Offset + UR_Value * 2**UR_Offset in 0 .. 2**ES - 1);
            Write (Least_Significant_Index, Byte'Val (LR_Value + Element_Value * Pow2_LSE_Offset + UR_Value * 2**UR_Offset));
         end;

      else
         --  LSE
         declare
            LSE_Value   : constant Long_Integer := (Value'Pos (Val) mod Pow2_LSE_Bits);
            LSE_Current : constant Long_Integer := Byte'Pos (Read (Least_Significant_Index)) mod 2**LSE_Offset;
         begin
            --  WORKAROUND: Componolit/Workarounds#26
            pragma Warnings (Off, "no Global contract available for ""*""");
            RFLX_Lemmas.Mult_Ge_0 (Conversions.To_Big_Integer (LSE_Value), Conversions.To_Big_Integer (Pow2_LSE_Offset));
            RFLX_Lemmas.Mult_Limit (Conversions.To_Big_Integer (LSE_Value), LSE_Bits, Conversions.To_Big_Integer (Pow2_LSE_Offset), LSE_Offset);
            pragma Assert (LSE_Value * Pow2_LSE_Offset <= 2**(LSE_Bits + LSE_Offset));
            --  WORKAROUND: Componolit/Workarounds#15
            pragma Annotate (GNATprove, False_Positive, "assertion",
                             "direct re-expression of lemma postcondition");
            pragma Annotate (GNATprove, False_Positive, "overflow check",
                             "consequence of lemma postcondition");
            RFLX_Lemmas.Left_Shift_Limit (Conversions.To_Big_Integer (LSE_Value), LSE_Bits, LSE_Offset);
            pragma Warnings (On, "no Global contract available for ""*""");

            Write (Least_Significant_Index, Byte'Val (LSE_Current + LSE_Value * Pow2_LSE_Offset));
            V := Value'Pos (Val) / 2**LSE_Bits;
         end;

         --  LSE + 1 .. MSE - 1
         for I in Least_Significant_Index + 1 .. Most_Significant_Index - 1
         loop
            Write (I, Byte'Val (V mod 2**Byte'Size));
            V := V / 2**Byte'Size;
         end loop;

         --  MSE
         declare
            MSE_Current : constant Long_Integer := Byte'Pos (Read (Most_Significant_Index)) / 2**MSE_Bits;
            MSE_Value   : constant Long_Integer := V mod 2**MSE_Bits;
            pragma Assert (MSE_Value < 2**MSE_Bits);
         begin
            --  WORKAROUND: Componolit/Workarounds#26
            pragma Warnings (Off, "no Global contract available for ""*""");
            RFLX_Lemmas.Right_Shift_Limit (Conversions.To_Big_Integer (Byte'Pos (Read (Most_Significant_Index))), MSE_Offset, MSE_Bits);
            pragma Assert (2**MSE_Offset <= Natural'Last);
            RFLX_Lemmas.Left_Shift_Limit (Conversions.To_Big_Integer (MSE_Current), MSE_Offset, MSE_Bits);
            pragma Warnings (On, "no Global contract available for ""*""");
            pragma Assert (MSE_Current >= 0);
            pragma Assert (2**MSE_Bits >= 0);
            pragma Assert (MSE_Current * 2**MSE_Bits in 0 .. 2**ES - 2**MSE_Bits);
            pragma Assert (MSE_Value in 0 .. 2**MSE_Bits - 1);
            Write (Most_Significant_Index, Byte'Val (MSE_Current * 2**MSE_Bits + MSE_Value));
         end;
      end if;
   end Insert;

end {prefix}RFLX_Generic_Types;
