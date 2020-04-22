with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Length;

with RFLX.TLV.Message;
with RFLX.In_TLV.Contains;

package body RFLX.In_TLV.Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_TLV");
   end Name;

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");
   pragma Warnings (Off, "unused assignment to ""TLV_Message_Context""");

   procedure Test_Null_In_TLV (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer              : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(64, 0);
      TLV_Message_Context : TLV.Message.Context := TLV.Message.Create;
      Valid               : Boolean;
   begin
      TLV.Message.Initialize (TLV_Message_Context, Buffer);
      TLV.Message.Verify_Message (TLV_Message_Context);
      Valid := TLV.Message.Structural_Valid_Message (TLV_Message_Context);
      Assert (Valid, "Structural invalid TLV message");
      if Valid then
         Valid := In_TLV.Contains.Null_Message_In_TLV_Message_Value (TLV_Message_Context);
         Assert (Valid, "TLV message contains no null message");

      end if;
   end Test_Null_In_TLV;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Null_In_TLV'Access, "Null message in TLV");
   end Register_Tests;

end RFLX.In_TLV.Tests;
