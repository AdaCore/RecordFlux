with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.TLV.Message;

package body RFLX.TLV_Tests is

   use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Index;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("TLV");
   end Name;

   Value_Length : RFLX_Builtin_Types.Length;

   procedure Store_Value_Length (Buffer : RFLX_Builtin_Types.Bytes) is
   begin
      Value_Length := Buffer'Length;
   end Store_Value_Length;

   procedure Get_Value_Length is new TLV.Message.Generic_Get_Value (Store_Value_Length);

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 3) :=
     (others => 0);

   function Valid_Data_Length (L : RFLX_Builtin_Types.Length) return Boolean is
      (L <= Data'Length)
   with
      SPARK_Mode;

   procedure Write_Data (Buffer : out RFLX_Builtin_Types.Bytes) with
      SPARK_Mode,
      Pre => Valid_Data_Length (Buffer'Length)
   is
   begin
      Buffer := Data (Data'First .. Data'First + Buffer'Length - 1);
   end Write_Data;

   procedure Test_Parsing_TLV_Data (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      use type RFLX.RFLX_Types.Base_Integer;
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 1, 2, 3, 4);
      Context : TLV.Message.Context;
      Tag     : TLV.Tag;
      Length  : TLV.Length;
      Value   : RFLX_Builtin_Types.Bytes := (0, 0, 0, 0);
   begin
      TLV.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      TLV.Message.Verify_Message (Context);
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Tag), "Invalid Tag");
      if TLV.Message.Valid (Context, TLV.Message.F_Tag) then
         Tag := TLV.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Data), "Unexpected Tag");
         Assert (TLV.To_Base_Integer (Tag) = 1, "Invalid conversion of Tag");
         Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
         if TLV.Message.Valid (Context, TLV.Message.F_Length) then
            Length := TLV.Message.Get_Length (Context);
            Assert (Length'Image, TLV.Length'Image (4), "Unexpected Length");
            Assert (TLV.Message.Present (Context, TLV.Message.F_Value), "Invalid Value");
            if TLV.Message.Present (Context, TLV.Message.F_Value) then
               Get_Value_Length (Context);
               Assert (Value_Length'Image, RFLX_Builtin_Types.Length'Image (4), "Unexpected Value length");
               TLV.Message.Get_Value
                 (Context,
                  Value (Value'First
                    .. RFLX_Builtin_Types.Index (RFLX_Builtin_Types.Length (Value'First)
                      + RFLX_Types.To_Length (TLV.Message.Field_Size (Context, TLV.Message.F_Value)) - 1)));
               Assert (Value, (1, 2, 3, 4), "Unexpected Value");
               declare
                  Data : RFLX_Types.Bytes := (0, 0, 0, 0);
               begin
                  TLV.Message.Get_Value (Context, Data);
                  Assert (Data, (1, 2, 3, 4), "Unexpected Value returned by function");
               end;
            end if;
         end if;
      end if;
      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid Message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid Message");
      Assert (TLV.Message.Byte_Size (Context)'Image, RFLX_Builtin_Types.Length'Image (7), "Invalid message size");

      TLV.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (56)'Image, "Invalid Context.Last");
   end Test_Parsing_TLV_Data;

   procedure Test_Parsing_TLV_Data_Zero (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 0);
      Context : TLV.Message.Context;
      Tag     : TLV.Tag;
      Length  : TLV.Length;
   begin
      TLV.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      TLV.Message.Verify_Message (Context);
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Tag), "Invalid Tag");
      if TLV.Message.Valid (Context, TLV.Message.F_Tag) then
         Tag := TLV.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Data), "Unexpected Tag");
         Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
         if TLV.Message.Valid (Context, TLV.Message.F_Length) then
            Length := TLV.Message.Get_Length (Context);
            Assert (Length'Image, TLV.Length'Image (0), "Unexpected Length");
            Assert (not TLV.Message.Present (Context, TLV.Message.F_Value), "Valid Value");
         end if;
      end if;
      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid Message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid Message");
      Assert (TLV.Message.Byte_Size (Context)'Image, RFLX_Builtin_Types.Length'Image (3), "Invalid message size");

      TLV.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (24)'Image, "Invalid Context.Last");
   end Test_Parsing_TLV_Data_Zero;

   procedure Test_Parsing_TLV_Error (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 3);
      Context : TLV.Message.Context;
      Tag     : TLV.Tag;
   begin
      TLV.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      TLV.Message.Verify_Message (Context);
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Tag), "Invalid Tag");
      if TLV.Message.Valid (Context, TLV.Message.F_Tag) then
         Tag := TLV.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Error), "Unexpected Tag");
      end if;
      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid Message");
      Assert (TLV.Message.Valid_Message (Context), "Invalid Message");

      TLV.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (8)'Image, "Invalid Context.Last");
   end Test_Parsing_TLV_Error;

   procedure Test_Parsing_Invalid_TLV_Invalid_Tag (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      TLV.Message.Verify_Message (Context);
      Assert (not TLV.Message.Well_Formed_Message (Context), "Well formed message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (16)'Image, "Invalid Context.Last");
   end Test_Parsing_Invalid_TLV_Invalid_Tag;

   procedure Test_Generating_TLV_Data (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 1, 2, 3, 4);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0);
      Context  : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Set_Tag (Context, TLV.Msg_Data);
      TLV.Message.Set_Length (Context, 4);
      TLV.Message.Set_Value (Context, (1, 2, 3, 4));

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");
      Assert (TLV.Message.Byte_Size (Context)'Image, RFLX_Builtin_Types.Length'Image (7), "Invalid message size");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Context.Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_TLV_Data;

   procedure Test_Generating_TLV_Data_Generic (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Value is new TLV.Message.Generic_Set_Value (Write_Data, Valid_Data_Length);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 1, 2, 3, 4);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0);
      Context  : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Set_Tag (Context, TLV.Msg_Data);
      TLV.Message.Set_Length (Context, 4);
      Data := (1, 2, 3, 4);
      Set_Value (Context, Data'Length);

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Context.Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_TLV_Data_Generic;

   procedure Test_Generating_TLV_Data_Zero (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 0);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0);
      Context  : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Set_Tag (Context, TLV.Msg_Data);
      TLV.Message.Set_Length (Context, 0);
      TLV.Message.Set_Value_Empty (Context);

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Context.Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_TLV_Data_Zero;

   procedure Test_Generating_TLV_Error (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected : RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 3);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 0);
      Context  : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Set_Tag (Context, TLV.Msg_Error);

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message");
      Assert (TLV.Message.Valid_Message (Context), "Invalid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Context.Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_TLV_Error;

   procedure Test_Read_Write_Reset (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 0, 0, 0, 0);
      Context : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      TLV.Message.Verify_Message (Context);

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message after initialization");

      declare
         pragma Warnings (Off, "subprogram ""Read"" has no effect");
         procedure Read (Buffer : RFLX_Builtin_Types.Bytes) is
         begin
            Assert (Buffer, (1, 0, 4, 0, 0, 0, 0), "Invalid binary representation");
         end Read;
         pragma Warnings (On, "subprogram ""Read"" has no effect");
         pragma Warnings (Off, "subprogram ""Message_Read"" has no effect");
         procedure Message_Read is new TLV.Message.Generic_Read (Read);
         pragma Warnings (On, "subprogram ""Message_Read"" has no effect");
      begin
         Message_Read (Context);
      end;

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message after reading");
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
      Assert (TLV.Message.Get_Length (Context)'Image, TLV.Length (4)'Image, "Invalid length after reading");

      TLV.Message.Reset (Context);

      Assert (not TLV.Message.Well_Formed_Message (Context), "Well formed message after reset");

      declare
         procedure Write
            (Buffer                       : out RFLX_Builtin_Types.Bytes;
             Length                       : out RFLX_Builtin_Types.Length;
             Unused_Context_Buffer_Length :     RFLX_Builtin_Types.Length;
             Unused_Offset                :     RFLX_Builtin_Types.Length)
         with Post => Length = 7 and Length <= Buffer'Length
         is
         begin
            Assert (Buffer'Length = 7, "Invalid buffer length");
            Buffer := (1, 0, 2, 0, 0, 0, 0);
            Length := Buffer'Length;
         end Write;
         procedure Message_Write is new TLV.Message.Generic_Write (Write);
      begin
         Message_Write (Context);
      end;

      Assert (not TLV.Message.Well_Formed_Message (Context), "Well formed message after writing");

      TLV.Message.Verify_Message (Context);

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message after verification");
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
      Assert (TLV.Message.Get_Length (Context)'Image, TLV.Length (2)'Image, "Invalid length after writing");

      TLV.Message.Reset (Context);

      Assert (not TLV.Message.Well_Formed_Message (Context), "Well formed message after reset");

      declare
         function Write_Pre
            (Unused_Context_Buffer_Length : RFLX_Builtin_Types.Length;
             Offset                       : RFLX_Builtin_Types.Length)
             return Boolean
         is
            (Offset <= 3);
         procedure Write_4
            (Buffer                : out RFLX_Builtin_Types.Bytes;
             Length                : out RFLX_Builtin_Types.Length;
             Context_Buffer_Length :     RFLX_Builtin_Types.Length;
             Offset                :     RFLX_Builtin_Types.Length)
         with
            Pre =>
               Write_Pre (Context_Buffer_Length, Offset),
            Post =>
               Length <= Buffer'Length
         is
            Target : constant RFLX_Types.Bytes := (1, 0, 2, 0, 0, 0, 0);
         begin
            Assert (Context_Buffer_Length = 7, "Invalid context buffer length");
            Assert (Buffer'Length = 7 - Offset, "Invalid buffer length");
            Buffer := (others => 0);
            Buffer (Buffer'First .. Buffer'First + 3) :=
               Target (Target'First + RFLX_Types.Index (Offset + 1) - 1
                       .. Target'First + RFLX_Types.Index (Offset + 1) + 2);
            Length := 4;
         end Write_4;
         procedure Message_Write is new TLV.Message.Generic_Write (Write_4, Write_Pre);
      begin
         Message_Write (Context, 0);
         Message_Write (Context, 3);
      end;

      Assert (not TLV.Message.Well_Formed_Message (Context), "Well formed message after writing");

      TLV.Message.Verify_Message (Context);

      Assert (TLV.Message.Well_Formed_Message (Context), "Invalid message after verification");
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
      Assert (TLV.Message.Get_Length (Context)'Image, TLV.Length (2)'Image, "Invalid length after writing");

      TLV.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (56)'Image, "Invalid Context.Last");
   end Test_Read_Write_Reset;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_TLV_Data'Access, "Parsing TLV Data Message");
      Register_Routine (T, Test_Parsing_TLV_Data_Zero'Access, "Parsing TLV Data Message (zero length)");
      Register_Routine (T, Test_Parsing_TLV_Error'Access, "Parsing TLV Error Message");
      Register_Routine (T, Test_Parsing_Invalid_TLV_Invalid_Tag'Access, "Parsing Invalid TLV (invalid tag)");
      Register_Routine (T, Test_Generating_TLV_Data'Access, "Generating TLV Data Message");
      Register_Routine (T, Test_Generating_TLV_Data_Generic'Access, "Generating TLV Data Message (generic setter)");
      Register_Routine (T, Test_Generating_TLV_Data_Zero'Access, "Generating TLV Data Message (zero length)");
      Register_Routine (T, Test_Generating_TLV_Error'Access, "Generating TLV Error Message");
      Register_Routine (T, Test_Read_Write_Reset'Access, "Reading/Writing/Resetting Message");
   end Register_Tests;

end RFLX.TLV_Tests;
