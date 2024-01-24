pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

with {prefix}RFLX_Arithmetic;

package body {prefix}RFLX_Generic_Types.Generic_Operations with
  SPARK_Mode
is

   --
   -- Terminology
   --
   -- -----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX----   Data
   --
   --    |-------|-------|-------|-------|-------|       Value Bytes
   --    3  LMB  11      19      27      35 RMB  43
   --
   -- |----|                                     |----|
   -- LME_Offset                               RME_Offset
   --
   --      |--|                               |--|
   --    LME_Size                          RME_Size
   --
   -- |-------|-------|-------|-------|-------|-------|  Data Bytes
   -- 0       8       16      24      32      40
   --    LME                                     RME
   --
   -- LME: Leftmost Element of Data
   -- RME: Rightmost Element of Data
   --
   -- LSB: Leftmost Byte of Value
   -- RMB: Rightmost Byte of Value
   --
   -- LME_Offset: Bits the LME is shifted right relative to first of LME
   -- RME_Offset: Bits the RME is shifted left relative to last of RME
   --
   -- LME_Size: Number of bits of LME contained in LMB
   -- RME_Size: Number of bits of RME contained in RMB
   --
   -- LME_Index: Index pointing to LME
   -- RME_Index: Index pointing to RME
   --

   use {prefix}RFLX_Arithmetic;

   procedure Get_Index_Offset
      (First, Last : Long_Integer;
       Off         : Offset;
       Value_Size  : Positive;
       RME_Index   : out Index;
       LME_Index   : out Index;
       RME_Size    : out Natural;
       LME_Size    : out Natural)
     with
       Pre =>
       (Value_Size in 1 .. U64'Size
        and then Last >= Long_Integer (Index'First) and then Last <= Long_Integer (Index'Last)
        and then First >= Long_Integer (Index'First) and then First <= Long_Integer (Index'Last)
        and then Long_Integer ((Natural (Off) + Value_Size - 1) / Byte'Size) < Long_Integer (Last - First + 1)),
       Post =>
         (RME_Index = Index (Last - Long_Integer (Off) / Byte'Size)
          and then LME_Index = Index (Last - (Long_Integer (Off) + Long_Integer (Value_Size) - 1) / Byte'Size)
          and then RME_Size = Byte'Size - Natural (Off)
          and then LME_Size = (Natural (Off) + Value_Size + Byte'Size - 1) mod Byte'Size + 1)
   is
   begin
      RME_Index := Index (Last - Long_Integer (Off) / Byte'Size);
      LME_Index := Index (Last - (Long_Integer (Off) + Long_Integer (Value_Size) - 1) / Byte'Size);
      RME_Size := Byte'Size - Natural (Off);
      LME_Size := (Natural (Off) + Value_Size + Byte'Size - 1) mod Byte'Size + 1;
   end Get_Index_Offset;

   function U64_Extract
      (Buffer     : Bytes;
       First      : Index;
       Last       : Index;
       Off        : Offset;
       Value_Size : Positive) return U64
   with
     Pre =>
       (First >= Buffer'First
        and then Last <= Buffer'Last
        and then Value_Size in 1 .. U64'Size
        and then Long_Integer ((Natural (Off) + Value_Size - 1) / Byte'Size) < Buffer (First .. Last)'Length),
     Post =>
       (if Value_Size < U64'Size then U64_Extract'Result < 2**Value_Size)
   is
      Data : constant Bytes := Buffer (First .. Last);

      RME_Index : Index;
      LME_Index : Index;

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : Natural;

      LME_Size   : Natural;
      LME_Offset : Natural;
      Result : U64 := 0;

   begin
      -- This function simply iterates over all data bytes that contain
      -- relevant data, from most significant to least significant, and adds
      -- them up in Result, shifting the Result before the addition as needed
      -- (see helper function Shift_Add).

      -- We track the number of bits that are contained in Result to bound the
      -- current value of Result by 2 ** (number of bits). At the end of the
      -- function, the number of bits should be Value_Size.

      -- We start with the most significant byte. In network-byte order this
      -- is the rightmost byte. We need to take into account the case where
      -- this is the only byte.

      Get_Index_Offset (Long_Integer (Data'First), Long_Integer (Data'Last), Off, Value_Size, RME_Index, LME_Index, RME_Size, LME_Size);
      LME_Offset := Byte'Size - LME_Size;

      declare
         Tmp : U64 := Mask_Upper (Byte'Pos (Data (LME_Index)), LME_Size);
      begin
         if RME_Index = LME_Index then
            Tmp := Right_Shift (Tmp, RME_Offset, LME_Size);
         end if;
         Result := Result + Tmp;
      end;

      -- If it was the only byte, we are done.

      if RME_Index = LME_Index then
         pragma Assert (Result < 2 ** (LME_Size - RME_Offset));
         return Result;
      end if;

      pragma Assert (Fits_Into (Result, LME_Size));

      -- We now iterate over the "inner bytes" excluding the two extreme bytes.
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

      -- We now add the relevant bits from the last byte.
      pragma Assert (RME_Size in 1 .. U64'Size);
      pragma Assert (if LME_Index + 1 <= RME_Index - 1 then Fits_Into (Result, Natural (RME_Index - LME_Index) * Byte'Size - LME_Offset));
      pragma Assert (if LME_Index + 1 > RME_Index - 1 then Fits_Into (Result, Natural (RME_Index - LME_Index) * Byte'Size - LME_Offset));
      pragma Assert (Value_Size - RME_Size = Natural (RME_Index - LME_Index) * Byte'Size - LME_Offset);
      pragma Assert (Fits_Into (Result, Value_Size - RME_Size));
      declare
         Bits_To_Read : constant U64 :=
           Right_Shift (Byte'Pos (Data (RME_Index)), RME_Offset, Byte'Size);
      begin
         Result := Shift_Add (Result, Bits_To_Read, RME_Size, Value_Size - RME_Size);
      end;
      return Result;
   end U64_Extract;

   function U64_Extract_LE
      (Buffer     : Bytes;
       First      : Index;
       Last       : Index;
       Off        : Offset;
       Value_Size : Positive) return U64
   with
     Pre =>
       (First >= Buffer'First
        and then Last <= Buffer'Last
        and then Value_Size in 1 .. U64'Size
        and then Long_Integer ((Natural (Off) + Value_Size - 1) / Byte'Size) < Buffer (First .. Last)'Length),
     Post =>
       (if Value_Size < U64'Size then U64_Extract_LE'Result < 2**Value_Size)
   is
      Data : constant Bytes := Buffer (First .. Last);

      RME_Index : Index;
      LME_Index : Index;

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : Natural;

      LME_Size   : Natural;
      Result : U64 := 0;

   begin
      -- This function is identical in structure to the U64_Extract function.
      -- See the comments there for more details. However, in little endian we
      -- traverse the relevant bytes in the opposite order.

      Get_Index_Offset (Long_Integer (Data'First), Long_Integer (Data'Last), Off, Value_Size, RME_Index, LME_Index, RME_Size, LME_Size);

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
      pragma Assert (if LME_Index + 1 <= RME_Index - 1 then Fits_Into (Result, Natural (RME_Index - LME_Index) * Byte'Size - RME_Offset));
      pragma Assert (if LME_Index + 1 > RME_Index - 1 then Fits_Into (Result, Natural (RME_Index - LME_Index) * Byte'Size - RME_Offset));
      pragma Assert (Value_Size - LME_Size = Natural (RME_Index - LME_Index) * Byte'Size - RME_Offset);
      pragma Assert (Fits_Into (Result, Value_Size - LME_Size));
      Result :=
        Shift_Add (Result,
                   Mask_Upper (Byte'Pos (Data (LME_Index)), LME_Size),
                   LME_Size,
                   Value_Size - LME_Size);
      pragma Assert (Fits_Into (Result, Value_Size));
      return Result;
   end U64_Extract_LE;

   procedure U64_Insert
      (Val        : U64;
       Buffer     : in out Bytes;
       First      : Index;
       Last       : Index;
       Off        : Offset;
       Value_Size : Positive;
       BO         : Byte_Order)
   with
     Pre =>
       First >= Buffer'First
       and then Last <= Buffer'Last
       and then Value_Size <= U64'Size
       and then (if Value_Size < U64'Size then Val < 2**Value_Size)
       and then Long_Integer (Natural (Off) + Value_Size - 1) / Byte'Size < Buffer (First .. Last)'Length,
     Post =>
       Buffer'First = Buffer'Old'First and Buffer'Last = Buffer'Old'Last
   is
      RME_Index : Index;
      LME_Index : Index;

      RME_Offset : constant Natural := Natural (Off);
      RME_Size   : Natural;

      LME_Size   : Natural;

      RV : U64;
   begin
      Get_Index_Offset (Long_Integer (First), Long_Integer (Last), Off, Value_Size, RME_Index, LME_Index, RME_Size, LME_Size);

      if RME_Index = LME_Index then
         declare
            D : constant U64 := Byte'Pos (Buffer (RME_Index));
            pragma Assert (Fits_Into (D, Byte'Size));
            L_Bits : constant U64 := Mask_Lower (D, RME_Offset + Value_Size, Byte'Size);
            R_Bits : constant U64 := Mask_Upper (D, RME_Offset);
            Bits_To_Add : constant U64 := Left_Shift (Val, RME_Offset, Value_Size);
            Result : constant U64 :=
              Add (L_Bits, Add (Bits_To_Add, R_Bits, RME_Offset + Value_Size, RME_Offset), Byte'Size, RME_Offset + Value_Size);
         begin
            Buffer (RME_Index) := Byte'Val (Result);
         end;

      else
         case BO is
            when Low_Order_First =>
               declare
                  L_Bits : constant U64 := Mask_Lower (Byte'Pos (Buffer (LME_Index)), LME_Size, Byte'Size);
                  V_Bits : constant U64 := Mask_Upper (Val, LME_Size);
               begin
                  Buffer (LME_Index) := Byte'Val (Add (L_Bits, V_Bits, Byte'Size, LME_Size));
               end;
               RV := Right_Shift (Val, LME_Size, Value_Size);
               pragma Assert (Fits_Into (RV, Value_Size - LME_Size));

               for I in LME_Index + 1 .. RME_Index - 1
               loop
                  Buffer (I) := Byte'Val (RV mod 2**Byte'Size);
                  RV := Right_Shift (RV, Byte'Size, Value_Size - LME_Size - Natural (I - LME_Index - 1) * Byte'Size);
                  pragma Loop_Invariant (Fits_Into (RV, Value_Size - LME_Size - Natural (I - LME_Index) * Byte'Size));
               end loop;

               pragma Assert (RME_Size = Value_Size - LME_Size - Natural (RME_Index - LME_Index - 1) * Byte'Size);
               pragma Assert (Fits_Into (RV, RME_Size));
               declare
                  U_Value : constant U64 := Mask_Upper (Byte'Pos (Buffer (RME_Index)), RME_Offset);
                  R_Value : constant U64 := Left_Shift (RV, RME_Offset, RME_Size);
               begin
                  Buffer (RME_Index) := Byte'Val (Add (R_Value, U_Value, Byte'Size, RME_Offset));
               end;
            when High_Order_First =>
               pragma Assert (LME_Size = Value_Size - RME_Size - Natural (RME_Index - LME_Index - 1) * Byte'Size);
               declare
                  L_Bits : constant U64 := Mask_Upper (Byte'Pos (Buffer (RME_Index)), RME_Offset);
                  V_Bits : constant U64 := Mask_Upper (Val, RME_Size);
                  V_Value : constant U64 := Left_Shift (V_Bits, RME_Offset, RME_Size);
               begin
                  Buffer (RME_Index) := Byte'Val (L_Bits + V_Value);
                  RV := Right_Shift (Val, RME_Size, Value_Size);
               end;

               pragma Assert (RME_Size < Value_Size);
               pragma Assert (Fits_Into (RV, Value_Size - RME_Size));

               for I in reverse LME_Index + 1 .. RME_Index - 1
               loop
                  Buffer (I) := Byte'Val (RV mod 2**Byte'Size);
                  RV := Right_Shift (RV, Byte'Size, Value_Size - RME_Size - Natural (RME_Index - I - 1) * Byte'Size);
                  pragma Loop_Invariant (Fits_Into (RV, Value_Size - RME_Size - Natural (RME_Index - I) * Byte'Size));
               end loop;

               pragma Assert (LME_Size = Value_Size - RME_Size - Natural (RME_Index - LME_Index - 1) * Byte'Size);
               pragma Assert (Fits_Into (RV, LME_Size));
               declare
                  U_Value : constant U64 := Mask_Lower (Byte'Pos (Buffer (LME_Index)), LME_Size, Byte'Size);
                  Sum : U64;
               begin
                  Sum := Add (U_Value, RV, Byte'Size, LME_Size);
                  Buffer (LME_Index) := Byte'Val (Sum);
               end;
         end case;
      end if;
   end U64_Insert;

   function Extract
      (Buffer : Bytes;
       First  : Index;
       Last   : Index;
       Off    : Offset;
       Size   : Positive;
       BO     : Byte_Order) return U64
   is
   begin
      if BO = High_Order_First then
         return U64_Extract (Buffer, First, Last, Off, Size);
      else
         return U64_Extract_LE (Buffer, First, Last, Off, Size);
      end if;
   end Extract;

   function Extract
      (Buffer : Bytes;
       First  : Index;
       Last   : Index;
       Off    : Offset;
       Size   : Positive;
       BO     : Byte_Order) return Base_Integer
   is
   begin
      return Base_Integer (U64'(Extract (Buffer, First, Last, Off, Size, BO)));
   end Extract;

   procedure Insert
      (Val    : U64;
       Buffer : in out Bytes;
       First  : Index;
       Last   : Index;
       Off    : Offset;
       Size   : Positive;
       BO     : Byte_Order)
   is
   begin
      U64_Insert (Val, Buffer, First, Last, Off, Size, BO);
   end Insert;

   procedure Insert
      (Val    : Base_Integer;
       Buffer : in out Bytes;
       First  : Index;
       Last   : Index;
       Off    : Offset;
       Size   : Positive;
       BO     : Byte_Order)
   is
   begin
      Lemma_Size (Val, Size);
      Insert (U64 (Val), Buffer, First, Last, Off, Size, BO);
   end Insert;

end {prefix}RFLX_Generic_Types.Generic_Operations;
