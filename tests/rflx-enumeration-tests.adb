with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.Enumeration.Message;

package body RFLX.Enumeration.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Enumeration");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");

   procedure Test_Enumeration_Known (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer   : Types.Bytes_Ptr := new Types.Bytes'(32, 0);
      Context  : Enumeration.Message.Context_Type := Enumeration.Message.Create;
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
   end Test_Enumeration_Known;

   procedure Test_Enumeration_Unknown (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer   : Types.Bytes_Ptr := new Types.Bytes'(160, 0);
      Context  : Enumeration.Message.Context_Type := Enumeration.Message.Create;
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
   end Test_Enumeration_Unknown;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Enumeration_Known'Access, "Enumeration Known");
      Register_Routine (T, Test_Enumeration_Unknown'Access, "Enumeration Unknown");
   end Register_Tests;

end RFLX.Enumeration.Tests;
