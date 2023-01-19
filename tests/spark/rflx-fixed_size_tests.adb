with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.Fixed_Size.Simple_Message;
with RFLX.Universal;

package body RFLX.Fixed_Size_Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Fixed_Size");
   end Name;

   procedure Test_To_Structure (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer    : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 2, 3, 4);
      Context   : Fixed_Size.Simple_Message.Context;
      Structure : Fixed_Size.Simple_Message.Structure;
      package Message renames Fixed_Size.Simple_Message;
   begin
      Message.Initialize (Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      Message.Verify_Message (Context);

      Assert (Message.Well_Formed_Message (Context), "Invalid message");

      Message.To_Structure (Context, Structure);

      Assert (Structure.Message_Type.Known, "Unknown Message_Type");
      Assert (Structure.Message_Type.Enum'Img, Universal.OT_Data'Img, "Unexpected Message_Type");
      Assert (Structure.Data, (2, 3, 4), "Unexpected Data");

      -- Eng/RecordFlux/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");
      RFLX_Types.Free (Buffer);
   end Test_To_Structure;

   procedure Test_To_Context (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer    : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(0, 0, 0, 0);
      Context   : Fixed_Size.Simple_Message.Context;
      Structure : constant Fixed_Size.Simple_Message.Structure := (Message_Type => (Known => True,
                                                                                    Enum => Universal.OT_Data),
                                                                   Data => (2, 3, 4));
      Data      : RFLX_Builtin_Types.Bytes := (0, 0, 0);
      package Message renames Fixed_Size.Simple_Message;
   begin
      Message.Initialize (Context, Buffer);
      Message.To_Context (Structure, Context);

      Assert (Message.Well_Formed_Message (Context), "Invalid message");
      Assert (Message.Get_Message_Type (Context).Known, "Unknown Message_Type");
      Assert (Message.Get_Message_Type (Context).Enum'Img, Universal.OT_Data'Img, "Unexpected Message_Type");

      Message.Get_Data (Context, Data);

      Assert (Data, (2, 3, 4), "Unexpected Data");

      -- Eng/RecordFlux/Workarounds#32
      pragma Warnings (Off, "unused assignment to ""Context""");
      pragma Warnings (Off, """Context"" is set by ""*"" but not used after the call");
      Message.Take_Buffer (Context, Buffer);
      pragma Warnings (On, """Context"" is set by ""*"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Context""");
      RFLX_Types.Free (Buffer);
   end Test_To_Context;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_To_Structure'Access, "Converting context to structure");
      Register_Routine (T, Test_To_Context'Access, "Converting structure to context");
   end Register_Tests;

end RFLX.Fixed_Size_Tests;
