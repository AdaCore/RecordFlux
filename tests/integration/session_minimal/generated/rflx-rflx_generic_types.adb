pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package body RFLX.RFLX_Generic_Types with
  SPARK_Mode
is

   --
   --  Terminology
   --
   --  -----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX----    Data
   --
   --     |-------|-------|-------|-------|-------|       Value Bytes
   --     3  LMB  11      19      27      35 RMB  43
   --
   --  |----|                                     |----|
   --  LME_Offset                               RME_Offset
   --
   --       |--|                               |--|
   --     LME_Size                          RME_Size
   --
   --  |-------|-------|-------|-------|-------|-------|  Data Bytes
   --  0       8       16      24      32      40
   --     LME                                     RME
   --
   --  LME: Leftmost Element of Data
   --  RME: Rightmost Element of Data
   --
   --  LSB: Leftmost Byte of Value
   --  RMB: Rightmost Byte of Value
   --
   --  LME_Offset: Bits the LME is shifted right relative to first of LME
   --  RME_Offset: Bits the RME is shifted left relative to last of RME
   --
   --  LME_Size: Number of bits of LME contained in LMB
   --  RME_Size: Number of bits of RME contained in RMB
   --
   --  LME_Index: Index pointing to LME
   --  RME_Index: Index pointing to RME
   --

   use RFLX.RFLX_Arithmetic;

   function U64_Extract
      (Data       : Bytes;
       Off        : Offset;
       Value_Size : Positive) return U64 with
      Pre =>
        (Value_Size in 1 .. U64'Size
        and then Long_Integer ((Natural (Off) + Value_Size - 1) / Byte'Size) < Data'Length),
     Post =>
       (if Value_Size < U64'Size then U64_Extract'Result < 2**Value_Size)
   is
      RME_Index : constant Index := Index (Long_Integer (Data'Last) - Long_Integer (Off) / Byte'Size);
      LME_Index : constant Index := Index (Long_Integer (Data'Last) - (Long_Integer (Off) + Long_Integer (Value_Size) - 1) / Byte'Size);

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : constant Natural := Byte'Size - RME_Offset;

      LME_Size   : constant Natural := (RME_Offset + Value_Size + Byte'Size - 1) mod Byte'Size + 1;
      LME_Offset : constant Natural := Byte'Size - LME_Size;
      Result : U64 := 0;

   begin
      --  This function simply iterates over all data bytes that contain
      --  relevant data, from most significant to least significant, and adds
      --  them up in Result, shifting the Result before the addition as needed
      --  (see helper function Shift_Add).

      --  We track the number of bits that are contained in Result to bound the
      --  current value of Result by 2 ** (number of bits). At the end of the
      --  function, the number of bits should be Value_Size.

      --  We start with the most significant byte. In network-byte order this
      --  is the rightmost byte. We need to take into account the case where
      --  this is the only byte.
      declare
         Tmp : U64 := Mask_Upper (Byte'Pos (Data (LME_Index)), LME_Size);
      begin
         if RME_Index = LME_Index then
            Tmp := Right_Shift (Tmp, RME_Offset, LME_Size);
         end if;
         Result := Result + Tmp;
      end;

      --  If it was the only byte, we are done.

      if RME_Index = LME_Index then
         pragma Assert (Result < 2 ** (LME_Size - RME_Offset));
         return Result;
      end if;

      pragma Assert (Fits_Into (Result, LME_Size));

      --  We now iterate over the "inner bytes" excluding the two extreme bytes.
      for I in LME_Index + 1  .. RME_Index  - 1 loop
         Result :=
           Shift_Add
             (Result,
              Byte'Pos (Data (I)),
              Byte'Size,
              Natural (I - LME_Index) * Byte'Size - LME_Offset);
         pragma Loop_Invariant
           (Fits_Into (Result, Natural (I - LME_Index + 1) * Byte'Size - LME_Offset));
      end loop;

      --  We now add the relevant bits from the last byte.
      pragma Assert (RME_Size in 1 .. U64'Size);
      declare
         Bits_To_Read : constant U64 :=
           Right_Shift (Byte'Pos (Data (RME_Index)), RME_Offset, Byte'Size);
      begin
         Result := Shift_Add (Result, Bits_To_Read, RME_Size, Value_Size - RME_Size);
      end;
      return Result;
   end U64_Extract;

   function U64_Extract_LE
      (Data       : Bytes;
       Off        : Offset;
       Value_Size : Positive) return U64 with
      Pre =>
        (Value_Size in 1 .. U64'Size
        and then Long_Integer ((Natural (Off) + Value_Size - 1) / Byte'Size) < Data'Length),
     Post =>
       (if Value_Size < U64'Size then U64_Extract_LE'Result < 2**Value_Size)
   is
      RME_Index : constant Index := Index (Long_Integer (Data'Last) - Long_Integer (Off) / Byte'Size);
      LME_Index : constant Index := Index (Long_Integer (Data'Last) - (Long_Integer (Off) + Long_Integer (Value_Size) - 1) / Byte'Size);

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : constant Natural := Byte'Size - RME_Offset;

      LME_Size   : constant Natural := (RME_Offset + Value_Size + Byte'Size - 1) mod Byte'Size + 1;
      Result : U64 := 0;

   begin
      --  This function is identical in structure to the U64_Extract function.
      --  See the comments there for more details. However, in little endian we
      --  traverse the relevant bytes in the opposite order.

      declare
         Tmp : U64 := Byte'Pos (Data (RME_Index));
      begin
         if RME_Index = LME_Index then
            Tmp := Mask_Upper (Tmp, LME_Size);
         end if;
         Tmp :=
           Right_Shift
             (Tmp,
              RME_Offset,
              (if RME_Index = LME_Index then LME_Size else Byte'Size));
         Result := Result + Tmp;
      end;

      if RME_Index = LME_Index then
         pragma Assert (Fits_Into (Result, Value_Size));
         return Result;
      end if;

      pragma Assert (Fits_Into (Result, RME_Size));

      for I in reverse LME_Index + 1  .. RME_Index  - 1 loop
         Result :=
            Shift_Add
               (Result,
                Byte'Pos (Data (I)),
                Byte'Size,
                Natural (RME_Index - I) * Byte'Size - RME_Offset);
         pragma Loop_Invariant
           (Fits_Into (Result, Natural (RME_Index - I + 1) * Byte'Size - RME_Offset));
      end loop;

      pragma Assert (LME_Size < U64'Size);
      Result :=
        Shift_Add (Result,
                   Mask_Upper (Byte'Pos (Data (LME_Index)), LME_Size),
                   LME_Size,
                   Value_Size - LME_Size);
      pragma Assert (Fits_Into (Result, Value_Size));
      return Result;
   end U64_Extract_LE;

   function Extract (Data : Bytes;
                     Off  : Offset;
                     BO   : Byte_Order) return Value
   is
      pragma Compile_Time_Error ((if Value'Size = 64 then
                                    U64 (Value'First) /= U64'First or U64 (Value'Last) /= U64'Last
                                 else
                                    U64 (Value'Last) - U64 (Value'First) /= U64 (2**Value'Size) - 1),
                                 "Value must cover entire value range");
   begin
      if BO = High_Order_First then
         return Value (U64_Extract (Data, Off, Value'Size));
      else
         return Value (U64_Extract_LE (Data, Off, Value'Size));
      end if;
   end Extract;

   procedure U64_Insert (Val        :        U64;
                         Data       : in out Bytes;
                         Off        :        Offset;
                         Value_Size : Positive) with
     Pre =>
       Value_Size <= U64'Size
       and then (if Value_Size < U64'Size then Val < 2**Value_Size)
       and then Long_Integer (Natural (Off) + Value_Size - 1) / Byte'Size < Data'Length
   is
      RME_Index : constant Index := Index (Long_Integer (Data'Last) - Long_Integer (Off) / Byte'Size);
      LME_Index : constant Index := Index (Long_Integer (Data'Last) - (Long_Integer (Off) + Long_Integer (Value_Size) - 1) / Byte'Size);

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : constant Natural := Byte'Size - RME_Offset;

      LME_Size   : constant Natural := (RME_Offset + Value_Size + Byte'Size - 1) mod Byte'Size + 1;

      RV : U64;
   begin

      --  Handle the case where there is only one byte

      if RME_Index = LME_Index then
         declare
            D : constant U64 := Byte'Pos (Data (RME_Index));
            pragma Assert (Fits_Into (D, Byte'Size));
            L_Bits : constant U64 := Mask_Lower (D, RME_Offset + Value_Size, Byte'Size);
            R_Bits : constant U64 := Mask_Upper (D, RME_Offset);
            Bits_To_Add : constant U64 := Left_Shift (Val, RME_Offset, Value_Size);
            Result : constant U64 :=
              Add (L_Bits, Add (Bits_To_Add, R_Bits, RME_Offset + Value_Size, RME_Offset), Byte'Size, RME_Offset + Value_Size);
         begin
            Data (RME_Index) := Byte'Val (Result);
         end;

      else
         --  Case where more than one byte needs to be written. Iterate over
         --  the relevant bytes of the data array, and write each byte while
         --  preserving bits that should not be overwritten.

         --  We start with the least significant byte (RME in network byte
         --  order).

         declare
            L_Bits : constant U64 := Mask_Upper (Byte'Pos (Data (RME_Index)), RME_Offset);
            V_Bits : constant U64 := Mask_Upper (Val, RME_Size);
            V_Value : constant U64 := Left_Shift (V_Bits, RME_Offset, RME_Size);
         begin
            Data (RME_Index) := Byte'Val (L_Bits + V_Value);
            RV := Right_Shift (Val, RME_Size, Value_Size);
         end;

         pragma Assert (RME_Size < Value_Size);
         pragma Assert (Fits_Into (RV, Value_Size - RME_Size));

         --  The inner bytes are fully overwritten.

         for I in reverse LME_Index + 1 .. RME_Index - 1
         loop
            Data (I) := Byte'Val (RV mod 2**Byte'Size);
            RV := Right_Shift (RV, Byte'Size, Value_Size - RME_Size - Natural (RME_Index - I - 1) * Byte'Size);
            pragma Loop_Invariant (Fits_Into (RV, Value_Size - RME_Size - Natural (RME_Index - I) * Byte'Size));
         end loop;

         --  The last byte (LME in network byte order).

         pragma Assert (LME_Size = Value_Size - RME_Size - Natural (RME_Index - LME_Index - 1) * Byte'Size);
         pragma Assert (Fits_Into (RV, LME_Size));
         declare
            U_Value : constant U64 := Mask_Lower (Byte'Pos (Data (LME_Index)), LME_Size, Byte'Size);
            Sum : U64;
         begin
            Sum := Add (U_Value, RV, Byte'Size, LME_Size);
            Data (LME_Index) := Byte'Val (Sum);
         end;
      end if;
   end U64_Insert;

   procedure U64_Insert_LE (Val        :        U64;
                            Data       : in out Bytes;
                            Off        :        Offset;
                            Value_Size : Positive) with
     Pre =>
       Value_Size <= U64'Size
       and then (if Value_Size < U64'Size then Val < 2**Value_Size)
       and then Long_Integer (Natural (Off) + Value_Size - 1) / Byte'Size < Data'Length
   is
      RME_Index : constant Index := Index (Long_Integer (Data'Last) - Long_Integer (Off) / Byte'Size);
      LME_Index : constant Index := Index (Long_Integer (Data'Last) - (Long_Integer (Off) + Long_Integer (Value_Size) - 1) / Byte'Size);

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : constant Natural := Byte'Size - RME_Offset;

      LME_Size   : constant Natural := (RME_Offset + Value_Size + Byte'Size - 1) mod Byte'Size + 1;

      RV : U64;
   begin
      --  This function is identical in structure to the U64_Insert function.
      --  Refer to the comments there.

      if RME_Index = LME_Index then
         declare
            D : constant U64 := Byte'Pos (Data (RME_Index));
            L_Bits : constant U64 := Mask_Lower (D, RME_Offset + Value_Size, Byte'Size);
            R_Bits : constant U64 := Mask_Upper (D, RME_Offset);
            Bits_To_Add : constant U64 := Left_Shift (Val, RME_Offset, Value_Size);
            Result : constant U64 :=
              Add (L_Bits,
                   Add (Bits_To_Add, R_Bits, Value_Size + RME_Offset, RME_Offset),
                   Byte'Size,
                   RME_Offset + Value_Size);
         begin
            Data (RME_Index) := Byte'Val (Result);
         end;

      else
         declare
            L_Bits : constant U64 := Mask_Lower (Byte'Pos (Data (LME_Index)), LME_Size, Byte'Size);
            V_Bits : constant U64 := Mask_Upper (Val, LME_Size);
         begin
            Data (LME_Index) := Byte'Val (Add (L_Bits, V_Bits, Byte'Size, LME_Size));
         end;
         RV := Right_Shift (Val, LME_Size, Value_Size);
         pragma Assert (Fits_Into (RV, Value_Size - LME_Size));

         for I in LME_Index + 1 .. RME_Index - 1
         loop
            Data (I) := Byte'Val (RV mod 2**Byte'Size);
            RV := Right_Shift (RV, Byte'Size, Value_Size - LME_Size - Natural (I - LME_Index - 1) * Byte'Size);
            pragma Loop_Invariant (Fits_Into (RV, Value_Size - LME_Size - Natural (I - LME_Index) * Byte'Size));
         end loop;

         pragma Assert (RME_Size = Value_Size - LME_Size - Natural (RME_Index - LME_Index - 1) * Byte'Size);
         pragma Assert (Fits_Into (RV, RME_Size));
         declare
            U_Value : constant U64 := Mask_Upper (Byte'Pos (Data (RME_Index)), RME_Offset);
            R_Value : constant U64 := Left_Shift (RV, RME_Offset, RME_Size);
         begin
            Data (RME_Index) := Byte'Val (Add (R_Value, U_Value, Byte'Size, RME_Offset));
         end;
      end if;
   end U64_Insert_LE;

   procedure Insert (Val  :        Value;
                     Data : in out Bytes;
                     Off  :        Offset;
                     BO   : Byte_Order)
   is
      pragma Compile_Time_Error ((if Value'Size = 64 then
                                    U64 (Value'First) /= U64'First or U64 (Value'Last) /= U64'Last
                                 else
                                    U64 (Value'Last) - U64 (Value'First) /= U64 (2**Value'Size) - 1),
                                 "Value must cover entire value range");
   begin
      if BO = High_Order_First then
         U64_Insert (U64 (Val), Data, Off, Value'Size);
      else
         U64_Insert_LE (U64 (Val), Data, Off, Value'Size);
      end if;
   end Insert;

end RFLX.RFLX_Generic_Types;
