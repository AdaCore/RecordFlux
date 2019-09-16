with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.TLV.Message;

package body RFLX.TLV.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("TLV");
   end Name;

   Value_Length : Types.Length;

   procedure Store_Value_Length (Buffer : Types.Bytes) is
   begin
      Value_Length := Buffer'Length;
   end Store_Value_Length;

   procedure Get_Value_Length is new TLV.Message.Get_Value (Store_Value_Length);

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");

   procedure Test_TLV_Data (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(64, 4, 0, 0, 0, 0);
      Context : TLV.Message.Context := TLV.Message.Create;
      Tag     : TLV.Tag;
      Length  : TLV.Length;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Verify_Message (Context);
      Assert (TLV.Message.Valid (Context, TLV.Message.F_Tag), "Invalid Tag");
      if TLV.Message.Valid (Context, TLV.Message.F_Tag) then
         Tag := TLV.Message.Get_Tag (Context);
         Assert (Tag'Image, TLV.Tag'Image (TLV.Msg_Data), "Unexpected Tag");
         Assert (Convert (Tag) = 1, "Invalid conversion of Tag");
         Assert (TLV.Message.Valid (Context, TLV.Message.F_Length), "Invalid Length");
         if TLV.Message.Valid (Context, TLV.Message.F_Length) then
            Length := TLV.Message.Get_Length (Context);
            Assert (Length'Image, TLV.Length'Image (4), "Unexpected Length");
            Assert (TLV.Message.Present (Context, TLV.Message.F_Value), "Invalid Value");
            if TLV.Message.Present (Context, TLV.Message.F_Value) then
               Get_Value_Length (Context);
               Assert (Value_Length'Image, Types.Length'Image (4), "Unexpected Value length");
            end if;
         end if;
      end if;
      Assert (TLV.Message.Structural_Valid_Message (Context), "Structural invalid Message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid Message");
   end Test_TLV_Data;

   procedure Test_TLV_Data_Zero (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(64, 0);
      Context : TLV.Message.Context := TLV.Message.Create;
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
   end Test_TLV_Data_Zero;

   procedure Test_TLV_Error (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(1 => 192);
      Context : TLV.Message.Context := TLV.Message.Create;
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
   end Test_TLV_Error;

   procedure Test_Invalid_TLV_Invalid_Tag (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : Types.Bytes_Ptr := new Types.Bytes'(0, 0);
      Context : TLV.Message.Context := TLV.Message.Create;
   begin
      TLV.Message.Initialize (Context, Buffer);
      TLV.Message.Verify_Message (Context);
      Assert (not TLV.Message.Structural_Valid_Message (Context), "Structural valid message");
      Assert (not TLV.Message.Valid_Message (Context), "Valid message");
   end Test_Invalid_TLV_Invalid_Tag;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_TLV_Data'Access, "TLV Data Message");
      Register_Routine (T, Test_TLV_Data_Zero'Access, "TLV Data Message (zero length)");
      Register_Routine (T, Test_TLV_Error'Access, "TLV Error Message");
      Register_Routine (T, Test_Invalid_TLV_Invalid_Tag'Access, "Invalid TLV (invalid tag)");
   end Register_Tests;

end RFLX.TLV.Tests;
