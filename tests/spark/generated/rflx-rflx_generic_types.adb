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
   --     3  MSB  11      19      27      35 LSB  43
   --
   --  |----|                                     |----|
   --  MSE_Offset                               LSE_Offset
   --
   --       |--|                               |--|
   --     MSE_Size                          LSE_Size
   --
   --  |-------|-------|-------|-------|-------|-------|  Data Bytes
   --  0       8       16      24      32      40
   --     MSE                                     LSE
   --
   --  LSE: Least Significant Element of Data
   --  MSE: Most Significant Element of Data
   --
   --  LSB: Least Significant Byte of Value
   --  MSB: Most Significant Byte of Value
   --
   --  LSE_Offset: Bits the LSE is shifted left relative to last of LSE
   --  MSE_Offset: Bits the MSE is shifted right relative to first of MSE
   --
   --  LSE_Size: Number of bits of LSE contained in LSB
   --  MSE_Size: Number of bits of MSE contained in MSB
   --
   --  LSE_Index: Index pointing to LSE
   --  MSE_Index: Index pointing to MSE
   --

   use type RFLX.RFLX_Arithmetic.U64;

   function Left_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64 renames RFLX.RFLX_Arithmetic.Left_Shift;

   function Right_Shift (Value : U64; Value_Size : Positive; Length : Natural) return U64 renames RFLX.RFLX_Arithmetic.Right_Shift;

   function Mod_Pow2 (Value : U64; Exp : Natural) return U64 renames RFLX.RFLX_Arithmetic.Mod_Pow2;

   function Pow2 (Exp : Natural) return U64 renames RFLX.RFLX_Arithmetic.Pow2;

   function U64_Extract_Intermediate (Data_Current : U64;
                                      Data_Next    : U64;
                                      LSE_Size     : Natural;
                                      LSE_Offset   : Natural;
                                      Shift_Length : Natural) return U64 with
     Pre =>
       Data_Current < 2**Byte'Size
       and then Data_Next < 2**Byte'Size
       and then LSE_Size in 0 .. Byte'Size
       and then LSE_Offset in 0 .. Byte'Size - 1
       and then Shift_Length <= U64'Size - Byte'Size
       and then LSE_Size + LSE_Offset = Byte'Size,
     Post =>
       (if Byte'Size + Shift_Length < U64'Size
        then U64_Extract_Intermediate'Result <= Pow2 (Byte'Size + Shift_Length) - Pow2 (Shift_Length)
        else U64_Extract_Intermediate'Result <= U64'Last - Pow2 (Shift_Length) + 1)
   is
      Current : constant U64 := Right_Shift (Data_Current, Byte'Size, LSE_Offset);
      Next    : constant U64 := (if LSE_Offset > 0
                                 then Left_Shift (Data_Next mod Pow2 (LSE_Offset), LSE_Offset, LSE_Size)
                                 else Data_Next mod Pow2 (LSE_Offset));
   begin
      return Left_Shift (Current + Next, Byte'Size, Shift_Length);
   end U64_Extract_Intermediate;

   function U64_Extract_Remaining (Data          : U64;
                                   Element_Count : Natural;
                                   MSE_Size      : Natural;
                                   LSE_Offset    : Natural) return U64 with
     Pre =>
       MSE_Size in 0 .. Byte'Size
       and LSE_Offset in 0 .. Byte'Size - 1
       and MSE_Size - LSE_Offset in 1 .. Byte'Size
       and Element_Count <= 7,
     Post =>
       (if
          MSE_Size - LSE_Offset + (Byte'Size * Element_Count) < U64'Size
        then
          U64_Extract_Remaining'Result <= Pow2 (MSE_Size - LSE_Offset + (Byte'Size * Element_Count))
                                          - Pow2 (Byte'Size * Element_Count)
          and Pow2 (MSE_Size - LSE_Offset + (Byte'Size * Element_Count)) >= Pow2 (Byte'Size * Element_Count)
        else
          U64_Extract_Remaining'Result <= U64'Last - Pow2 (Byte'Size * Element_Count) + 1)
   is
      Remaining_Bits : constant U64 := Right_Shift (Data mod Pow2 (MSE_Size), MSE_Size, LSE_Offset);
   begin
      return Left_Shift (Remaining_Bits, MSE_Size - LSE_Offset, Byte'Size * Element_Count);
   end U64_Extract_Remaining;

   function U64_Extract (Data       : Bytes;
                         Off        : Offset;
                         Value_Size : Positive) return U64 with
     Pre =>
       (Value_Size in 1 .. U64'Size
        and then Long_Integer ((Natural (Off) + Value_Size - 1) / Byte'Size) < Data'Length
        and then (Natural (Off) + Value_Size - 1) / Byte'Size <= Natural'Size
        and then (Byte'Size - Natural (Off) mod Byte'Size) < Long_Integer'Size - 1),
     Post =>
       (if Value_Size < U64'Size then U64_Extract'Result < 2**Value_Size)
   is
      LSE_Index : constant Long_Integer := Long_Integer (Off) / Byte'Size;
      MSE_Index : constant Long_Integer := Long_Integer (Natural (Off) + Value_Size - 1) / Byte'Size;

      LSE_Offset : constant Natural := Natural (Natural (Off) mod Byte'Size);
      LSE_Size   : constant Natural := Byte'Size - LSE_Offset;

      MSE_Size   : constant Natural := (LSE_Offset + Value_Size + Byte'Size - 1) mod Byte'Size + 1;

      function D (Idx : Long_Integer) return Byte with
        Pre =>
          Idx >= 0 and then Idx < Data'Length and then Value_Size in 1 .. 64
      is
         function ES return Natural is (Byte'Size) with Post => ES'Result = Byte'Size;
         E : constant Natural := (LSE_Offset + Value_Size + Byte'Size - 1) mod ES + 1;
         pragma Assert (2**Byte'Size = 2**ES);
      begin
         declare
            Mask : constant Natural := (if Idx < MSE_Index then 2**ES else 2**E);
            Val  : constant Byte := Data (Index (Long_Integer (Data'Last) - Idx));
         begin
            return Byte'Val (Byte'Pos (Val) mod Mask);
         end;
      end D;

      Result : U64 := 0;
   begin
      for I in LSE_Index .. MSE_Index - 1
      loop
         Result := Result + U64_Extract_Intermediate (Byte'Pos (D (I)),
                                                      Byte'Pos (D (I + 1)),
                                                      LSE_Size,
                                                      LSE_Offset,
                                                      Byte'Size * Natural (I - LSE_Index));
         pragma Loop_Invariant (I - LSE_Index <= 7);
         pragma Loop_Invariant (if Byte'Size + Byte'Size * (I - LSE_Index) /= U64'Size
                                then Result < 2**(Byte'Size + Byte'Size * Natural (I - LSE_Index)));
      end loop;

      if MSE_Size > LSE_Offset then
         declare
            Element_Count : constant Natural := Natural (MSE_Index - LSE_Index);
            Val : constant U64 := U64_Extract_Remaining (Byte'Pos (D (MSE_Index)), Element_Count, MSE_Size, LSE_Offset);
         begin
            Result := Result + Val;
         end;
      end if;

      return (if Value_Size < U64'Size then Result mod Pow2 (Value_Size) else Result);
   end U64_Extract;

   function Extract (Data : Bytes;
                     Off  : Offset) return Value
   is
      pragma Compile_Time_Error ((if Value'Size = 64 then
                                    U64 (Value'First) /= U64'First or U64 (Value'Last) /= U64'Last
                                 else
                                    U64 (Value'Last) - U64 (Value'First) /= U64 (2**Value'Size) - 1),
                                 "Value must cover entire value range");
   begin
      return Value (U64_Extract (Data, Off, Value'Size));
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
      LSE_Index : constant Long_Integer := Long_Integer (Off) / Byte'Size;
      MSE_Index : constant Long_Integer := Long_Integer (Natural (Off) + Natural (Value_Size) - 1) / Byte'Size;

      LSE_Offset : constant Natural := Natural (Natural (Off) mod Byte'Size);
      LSE_Size   : constant Natural := Byte'Size - LSE_Offset;

      MSE_Size   : constant Natural := (LSE_Offset + Value_Size + Byte'Size - 1) mod Byte'Size + 1;
      MSE_Offset : constant Natural := Byte'Size - MSE_Size;

      function Read (Idx : Long_Integer) return Byte with
        Pre =>
          Idx >= 0 and then Idx < Data'Length
      is
      begin
         return Data (Index (Long_Integer (Data'Last) - Idx));
      end Read;

      procedure Write (Idx : Long_Integer; Element : Byte) with
        Pre =>
          Idx >= 0 and then Idx < Data'Length
      is
      begin
         Data (Index (Long_Integer (Data'Last) - Idx)) := Element;
      end Write;

      RV : U64;
   begin
      if LSE_Index = MSE_Index then
         --
         --  LSE/MSE
         --
         --  ---XX---
         --
         --  |--|-|--|
         --  0 U V L 8
         --
         --  U: Unchanged upper bits
         --  V: Bits to set
         --  L: Unchanged lower bits
         --
         declare
            L_Size   : constant Natural := LSE_Offset;
            V_Size   : constant Natural := Value_Size;

            L_Bits  : constant U64 := Mod_Pow2 (Byte'Pos (Read (LSE_Index)), L_Size);
            V_Bits  : constant U64 := Val;
            U_Bits  : constant U64 := Right_Shift (Byte'Pos (Read (MSE_Index)), Byte'Size, L_Size + V_Size);

            L_Value : constant U64 := L_Bits;
            V_Value : constant U64 := Left_Shift (V_Bits, V_Size, L_Size);
            U_Value : constant U64 := (if L_Size + V_Size < Byte'Size
                                       then Left_Shift (U_Bits, Byte'Size - L_Size - V_Size, L_Size + V_Size)
                                       else U_Bits);

            pragma Assert (L_Value + V_Value + U_Value < Pow2 (Byte'Size));
         begin
            Write (LSE_Index, Byte'Val (L_Value + V_Value + U_Value));
         end;

      else
         --
         --  LSE
         --
         --  XXXX----
         --
         --  |---|---|
         --  0 V   L 8
         --
         --  V: Bits to set
         --  L: Unchanged lower bits
         --
         declare
            L_Bits : constant U64 := Mod_Pow2 (Byte'Pos (Read (LSE_Index)), LSE_Offset);
            V_Bits : constant U64 := Mod_Pow2 (Val, LSE_Size);

            V_Value : constant U64 := Left_Shift (V_Bits, LSE_Size, LSE_Offset);
         begin
            Write (LSE_Index, Byte'Val (L_Bits + V_Value));
            RV := Right_Shift (Val, Value_Size, LSE_Size);
         end;

         --  LSE + 1 .. MSE - 1
         for I in LSE_Index + 1 .. MSE_Index - 1
         loop
            pragma Loop_Invariant (RV < 2**(Value_Size - LSE_Size - Natural (I - LSE_Index - 1) * Byte'Size));
            Write (I, Byte'Val (RV mod 2**Byte'Size));
            RV := Right_Shift (RV, Value_Size - LSE_Size - Natural (I - LSE_Index - 1) * Byte'Size, Byte'Size);
         end loop;

         --
         --  MSE
         --
         --  ----XXXX
         --
         --  |---|---|
         --  0 U   V 8
         --
         --  U: Unchanged upper bits
         --  V: Bits to set
         --
         declare
            V_Size : constant Natural := MSE_Size;
            U_Size : constant Natural := MSE_Offset;

            V_Bits : constant U64 := Mod_Pow2 (RV, V_Size);
            U_Bits : constant U64 := Right_Shift (Byte'Pos (Read (MSE_Index)), Byte'Size, V_Size);

            U_Value : constant U64 := (if U_Size > 0 then Left_Shift (U_Bits, U_Size, V_Size) else U_Bits);
         begin
            Write (MSE_Index, Byte'Val (V_Bits + U_Value));
         end;
      end if;
   end U64_Insert;

   procedure Insert (Val  :        Value;
                     Data : in out Bytes;
                     Off  :        Offset)
   is
      pragma Compile_Time_Error ((if Value'Size = 64 then
                                    U64 (Value'First) /= U64'First or U64 (Value'Last) /= U64'Last
                                 else
                                    U64 (Value'Last) - U64 (Value'First) /= U64 (2**Value'Size) - 1),
                                 "Value must cover entire value range");
   begin
      U64_Insert (U64 (Val), Data, Off, Value'Size);
   end Insert;

end RFLX.RFLX_Generic_Types;
