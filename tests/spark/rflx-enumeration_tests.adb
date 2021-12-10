with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.Enumeration.Message;

package body RFLX.Enumeration_Tests is

   use type RFLX.RFLX_Builtin_Types.Index, RFLX.RFLX_Builtin_Types.Bit_Length;

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Enumeration");
   end Name;

   procedure Test_Parsing_Enumeration_Known (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0);
      Context : Enumeration.Message.Context;
      Prio    : Enumeration.Priority;
   begin
      Enumeration.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      Enumeration.Message.Verify_Message (Context);

      if Enumeration.Message.Valid (Context, Enumeration.Message.F_Priority) then
         Prio := Enumeration.Message.Get_Priority (Context);
         if Prio.Known then
            Assert (Prio.Enum'Image, Enumeration.Priority_Enum'Image (Enumeration.Low), "Unexpected Priority");
         else
            Assert (False, "Invalid Priority.Known");
         end if;
      else
         Assert (False, "Invalid Priority");
      end if;
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid Message");

      Enumeration.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (16)'Image, "Invalid Context.Last");
   end Test_Parsing_Enumeration_Known;

   procedure Test_Parsing_Enumeration_Unknown (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer  : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(5, 0);
      Context : Enumeration.Message.Context;
      Prio    : Enumeration.Priority;
   begin
      Enumeration.Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));

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

      Enumeration.Message.Take_Buffer (Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (Context.Last'Image, RFLX_Builtin_Types.Bit_Length (16)'Image, "Invalid Context.Last");
   end Test_Parsing_Enumeration_Unknown;

   procedure Test_Generating_Enumeration (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Expected     : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(RFLX_Builtin_Types.Index'First => 4);
      Buffer       : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0);
      Context      : Enumeration.Message.Context;
      Message_Last : RFLX_Builtin_Types.Bit_Length;
   begin
      Enumeration.Message.Initialize (Context, Buffer);

      Enumeration.Message.Set_Priority (Context, Enumeration.Medium);

      Assert (Enumeration.Message.Structural_Valid_Message (Context), "Structural invalid message");
      Assert (Enumeration.Message.Valid_Message (Context), "Invalid message");

      Message_Last := Enumeration.Message.Message_Last (Context);
      Enumeration.Message.Take_Buffer (Context, Buffer);

      Assert (Message_Last >= RFLX_Types.Bit_Index'First, "Invalid range for Message_Last");
      Assert (RFLX_Builtin_Types.Index'Image (RFLX_Types.To_Index (Message_Last)
              - RFLX_Types.To_Index (Context.First) + 1),
              Expected'Length'Img,
              "Invalid buffer length");
      Assert (Buffer.all (RFLX_Types.To_Index (Context.First) .. RFLX_Types.To_Index (Message_Last)),
              Expected.all,
              "Invalid binary representation");

      RFLX_Types.Free (Expected);
      RFLX_Types.Free (Buffer);
   end Test_Generating_Enumeration;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parsing_Enumeration_Known'Access, "Parsing Enumeration Known");
      Register_Routine (T, Test_Parsing_Enumeration_Unknown'Access, "Parsing Enumeration Unknown");
      Register_Routine (T, Test_Generating_Enumeration'Access, "Generating Enumeration");
   end Register_Tests;

end RFLX.Enumeration_Tests;
