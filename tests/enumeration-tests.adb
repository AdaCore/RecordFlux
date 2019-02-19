with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with Enumeration.Message;

package body Enumeration.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Enumeration");
   end Name;

   procedure Test_Enumeration_Known (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer   : Types.Bytes := Types.Bytes'(32, 0);
      Priority : Enumeration.Priority;
   begin
      Enumeration.Message.Label (Buffer);
      Assert (Enumeration.Message.Valid_Priority (Buffer), "Invalid Priority");
      if Enumeration.Message.Valid_Priority (Buffer) then
         Priority := Enumeration.Message.Get_Priority (Buffer);
         if Priority.Known then
            Assert (Priority.Enum'Image, Enumeration.Priority_Enum'Image (Enumeration.LOW), "Unexpected Priority");
         else
            Assert (False, "Invalid Priority.Known");
         end if;
      end if;
      Assert (Enumeration.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Enumeration_Known;

   procedure Test_Enumeration_Unknown (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer   : Types.Bytes := Types.Bytes'(160, 0);
      Priority : Enumeration.Priority;
   begin
      Enumeration.Message.Label (Buffer);
      Assert (Enumeration.Message.Valid_Priority (Buffer), "Invalid Priority");
      if Enumeration.Message.Valid_Priority (Buffer) then
         Priority := Enumeration.Message.Get_Priority (Buffer);
         if Priority.Known then
            Assert (False, "Invalid Priority.Known");
         else
            Assert (Priority.Raw'Image, Enumeration.Priority_Base'Image (5), "Unexpected Priority");
         end if;
      end if;
      Assert (Enumeration.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Enumeration_Unknown;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Enumeration_Known'Access, "Enumeration Known");
      Register_Routine (T, Test_Enumeration_Unknown'Access, "Enumeration Unknown");
   end Register_Tests;

end Enumeration.Tests;
