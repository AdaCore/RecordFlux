with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Length;
with RFLX.RFLX_Types;

with RFLX.TLV.Message; use type RFLX.TLV.Tag_Base;

package body RFLX.TLV_Tests is

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

   procedure Get_Value_Length is new TLV.Message.Get_Value (Store_Value_Length);

   Data : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 3) :=
     (others => 0);

   function Valid_Data_Length (L : RFLX_Builtin_Types.Length) return Boolean is
      (L <= Data'Length);

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
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 0, 0, 0, 0);
      Context : TLV.Message.Context;
      Tag     : TLV.Tag;
      Length  : TLV.Length;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Verify_Message (Context);
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Tag), "Invalid Tag");
      if TLV.Message.Valid (Context, TLV.Message.F_Tag) then
         Tag := TLV.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Data), "Unexpected Tag");
         Assert (TLV.To_Base (Tag) = 1, "Invalid conversion of Tag");
         Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
         if TLV.Message.Valid (Context, TLV.Message.F_Length) then
            Length := TLV.Message.Get_Length (Context);
            Assert (Length'Image, TLV.Length'Image (4), "Unexpected Length");
            Assert (TLV.Message.Present (Context, TLV.Message.F_Value), "Invalid Value");
            if TLV.Message.Present (Context, TLV.Message.F_Value) then
               Get_Value_Length (Context);
               Assert (Value_Length'Image, RFLX_Builtin_Types.Length'Image (4), "Unexpected Value length");
            end if;
         end if;
      end if;
      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid Message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid Message");

      TLV.Message.Take_Buffer (Context, Buffer);
      Free_Bytes_Ptr (Buffer);

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
      TLV.Message.Initialize (Context, Buffer);
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
      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid Message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid Message");

      TLV.Message.Take_Buffer (Context, Buffer);
      Free_Bytes_Ptr (Buffer);

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
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Verify_Message (Context);
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Tag), "Invalid Tag");
      if TLV.Message.Valid (Context, TLV.Message.F_Tag) then
         Tag := TLV.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Error), "Unexpected Tag");
      end if;
      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid Message");
      Assert (TLV.Message.Valid_Message (Context), "Invalid Message");

      TLV.Message.Take_Buffer (Context, Buffer);
      Free_Bytes_Ptr (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (8)'Image, "Invalid Context.Last");
   end Test_Parsing_TLV_Error;

   procedure Test_Parsing_Invalid_TLV_Invalid_Tag (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context : TLV.Message.Context;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Verify_Message (Context);
      Assert (not TLV.Message.Structural_Valid_Message (Context), "Structural valid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);
      Free_Bytes_Ptr (Buffer);

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

      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
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
      Set_Value (Context);

      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
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

      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
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

      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (TLV.Message.Valid_Message (Context), "Invalid message");

      TLV.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      Free_Bytes_Ptr (Expected);
      Free_Bytes_Ptr (Buffer);
   end Test_Generating_TLV_Error;

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
   end Register_Tests;

end RFLX.TLV_Tests;
