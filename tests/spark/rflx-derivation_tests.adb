with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.Derivation.Message;
with RFLX.TLV;

package body RFLX.Derivation_Tests is

   use type RFLX.RFLX_Builtin_Types.Length, RFLX.RFLX_Builtin_Types.Index, RFLX.RFLX_Builtin_Types.Bit_Length;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Derivation");
   end Name;

   Value_Length : RFLX_Builtin_Types.Length;

   procedure Store_Value_Length (Buffer : RFLX_Builtin_Types.Bytes) is
   begin
      Value_Length := Buffer'Length;
   end Store_Value_Length;

   procedure Get_Value_Length is new Derivation.Message.Generic_Get_Value (Store_Value_Length);

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

   procedure Test_Parsing (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      use type RFLX_Types.Base_Integer;
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 0, 0, 0, 0);
      Context : Derivation.Message.Context;
      Tag     : TLV.Tag;
      Length  : TLV.Length;
   begin
      Derivation.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      Derivation.Message.Verify_Message (Context);
      Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Tag), "Invalid Tag");
      if Derivation.Message.Valid (Context, Derivation.Message.F_Tag) then
         Tag := Derivation.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Data), "Unexpected Tag");
         Assert (TLV.To_Base_Integer (Tag) = 1, "Invalid conversion of Tag");
         Assert (Derivation.Message.Valid (Context, Derivation.Message.F_Length), "Invalid Length");
         if Derivation.Message.Valid (Context, Derivation.Message.F_Length) then
            Length := Derivation.Message.Get_Length (Context);
            Assert (Length'Image, TLV.Length'Image (4), "Unexpected Length");
            Assert (Derivation.Message.Present (Context, Derivation.Message.F_Value), "Invalid Value");
            if Derivation.Message.Present (Context, Derivation.Message.F_Value) then
               Get_Value_Length (Context);
               Assert (Value_Length'Image, RFLX_Builtin_Types.Length'Image (4), "Unexpected Value length");
            end if;
         end if;
      end if;
      Assert (Derivation.Message.Well_Formed_Message (Context), "Invalid Message");
      Assert (not Derivation.Message.Valid_Message (Context), "Valid Message");

      Derivation.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (56)'Image, "Invalid Context.Last");
   end Test_Parsing;

   procedure Test_Generating (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      procedure Set_Value is new Derivation.Message.Generic_Set_Value (Write_Data, Valid_Data_Length);
      Expected : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 4, 0, 0, 0, 0);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0, 0, 0, 0);
      Context  : Derivation.Message.Context;
   begin
      Derivation.Message.Initialize (Context, Buffer);
      Derivation.Message.Set_Tag (Context, TLV.Msg_Data);
      Derivation.Message.Set_Length (Context, 4);
      Data := (0, 0, 0, 0);
      Set_Value (Context, Data'Length);

      Assert (Derivation.Message.Well_Formed_Message (Context), "Invalid message");
      Assert (not Derivation.Message.Valid_Message (Context), "Valid message");

      Derivation.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Context.Last)
              - RFLX_Types.To_Index (Context.First) + 1), Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Context.Last)), Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing'Access, "Parsing");
      Register_Routine (T, Test_Generating'Access, "Generating");
   end Register_Tests;

end RFLX.Derivation_Tests;
