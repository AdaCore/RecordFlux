with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Length;
with RFLX.RFLX_Types;

with RFLX.Enumeration.Message;

package body RFLX.Enumeration.Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Enumeration");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");

   procedure Test_Parsing_Enumeration_Known (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(32, 0);
      Context : Enumeration.Message.Context := Enumeration.Message.Create;
      Prio    : Enumeration.Priority;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Verify_Message (Context);

      if Enumeration.Message.Valid (Context, Enumeration.Message.F_Priority) then
         Prio := Enumeration.Message.Get_Priority (Context);
         if Prio.Known then
            Assert (Prio.Enum'Image, Enumeration.Priority_Enum'Image (Enumeration.LOW), "Unexpected Priority");
         else
            Assert (False, "Invalid Priority.Known");
         end if;
      else
         Assert (False, "Invalid Priority");
      end if;
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid Message");
   end Test_Parsing_Enumeration_Known;

   procedure Test_Parsing_Enumeration_Unknown (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(160, 0);
      Context : Enumeration.Message.Context := Enumeration.Message.Create;
      Prio    : Enumeration.Priority;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Verify_Message (Context);

      if Enumeration.Message.Valid (Context, Enumeration.Message.F_Priority) then
         Prio := Enumeration.Message.Get_Priority (Context);
         if Prio.Known then
            Assert (False, "Invalid Priority.Known");
         else
            Assert (Prio.Raw'Image, Enumeration.Priority_Base'Image (5), "Unexpected Priority");
         end if;
      else
         Assert (False, "Invalid Priority");
      end if;
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid Message");
   end Test_Parsing_Enumeration_Unknown;

   procedure Test_Generating_Enumeration (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected : constant RFLX_Builtin_Types.Bytes_Ptr :=
        new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 32);
      Buffer   : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context  : Enumeration.Message.Context := Enumeration.Message.Create;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Set_Priority (Context, Enumeration.LOW);

      Assert (Enumeration.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid message");

      Enumeration.Message.Take_Buffer (Context, Buffer);

      Assert (RFLX_Builtin_Types.Length'Image (RFLX_Types.Byte_Index (Context.Last)
              - RFLX_Types.Byte_Index (Context.First) + 1),
              Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.Byte_Index (Context.First) .. RFLX_Types.Byte_Index (Context.Last)),
              Expected.all,
              "Invalid binary representation");
   end Test_Generating_Enumeration;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_Enumeration_Known'Access, "Parsing Enumeration Known");
      Register_Routine (T, Test_Parsing_Enumeration_Unknown'Access, "Parsing Enumeration Unknown");
      Register_Routine (T, Test_Generating_Enumeration'Access, "Generating Enumeration");
   end Register_Tests;

end RFLX.Enumeration.Tests;
