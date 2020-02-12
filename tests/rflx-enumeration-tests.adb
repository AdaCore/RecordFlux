with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.Builtin_Types; use type RFLX.Builtin_Types.Length;
with RFLX.Types;

with RFLX.Enumeration.Message;

package body RFLX.Enumeration.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Enumeration");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");

   procedure Test_Parsing_Enumeration_Known (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer   : Builtin_Types.Bytes_Ptr := new Builtin_Types.Bytes'(32, 0);
      Context  : Enumeration.Message.Context := Enumeration.Message.Create;
      Priority : Enumeration.Priority;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Verify_Message (Context);

      if Enumeration.Message.Valid (Context, Enumeration.Message.F_Priority) then
         Priority := Enumeration.Message.Get_Priority (Context);
         if Priority.Known then
            Assert (Priority.Enum'Image, Enumeration.Priority_Enum'Image (Enumeration.LOW), "Unexpected Priority");
         else
            Assert (False, "Invalid Priority.Known");
         end if;
      else
         Assert (False, "Invalid Priority");
      end if;
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid Message");
   end Test_Parsing_Enumeration_Known;

   procedure Test_Parsing_Enumeration_Unknown (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer   : Builtin_Types.Bytes_Ptr := new Builtin_Types.Bytes'(160, 0);
      Context  : Enumeration.Message.Context := Enumeration.Message.Create;
      Priority : Enumeration.Priority;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Verify_Message (Context);

      if Enumeration.Message.Valid (Context, Enumeration.Message.F_Priority) then
         Priority := Enumeration.Message.Get_Priority (Context);
         if Priority.Known then
            Assert (False, "Invalid Priority.Known");
         else
            Assert (Priority.Raw'Image, Enumeration.Priority_Base'Image (5), "Unexpected Priority");
         end if;
      else
         Assert (False, "Invalid Priority");
      end if;
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid Message");
   end Test_Parsing_Enumeration_Unknown;

   procedure Test_Generating_Enumeration (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected : Builtin_Types.Bytes_Ptr := new Builtin_Types.Bytes'(Builtin_Types.Index'First => 32);
      Buffer   : Builtin_Types.Bytes_Ptr := new Builtin_Types.Bytes'(0, 0);
      Context  : Enumeration.Message.Context := Enumeration.Message.Create;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Set_Priority (Context, Enumeration.LOW);

      Assert (Enumeration.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid message");

      Enumeration.Message.Take_Buffer (Context, Buffer);

      Assert (Builtin_Types.Length'Image (Types.Byte_Index (Context.Last) - Types.Byte_Index (Context.First) + 1), Expected'Length'Img, "Invalid buffer length");
      Assert (Buffer.all (Types.Byte_Index (Context.First) .. Types.Byte_Index (Context.Last)), Expected.all, "Invalid binary representation");
   end Test_Generating_Enumeration;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_Enumeration_Known'Access, "Parsing Enumeration Known");
      Register_Routine (T, Test_Parsing_Enumeration_Unknown'Access, "Parsing Enumeration Unknown");
      Register_Routine (T, Test_Generating_Enumeration'Access, "Generating Enumeration");
   end Register_Tests;

end RFLX.Enumeration.Tests;
