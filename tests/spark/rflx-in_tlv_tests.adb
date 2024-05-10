with SPARK; use SPARK;
with SPARK.Assertions; use SPARK.Assertions;

with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

with RFLX.TLV.Message;
with RFLX.In_TLV.Contains;

package body RFLX.In_TLV_Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("In_TLV");
   end Name;

   procedure Test_Null_In_TLV (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer              : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 0, 0);
      TLV_Message_Context : TLV.Message.Context;
      Valid               : Boolean;
   begin
      TLV.Message.Initialize (TLV_Message_Context, Buffer, RFLX_Types.To_Last_Bit_Index (Buffer'Last));
      TLV.Message.Verify_Message (TLV_Message_Context);
      Valid := TLV.Message.Well_Formed_Message (TLV_Message_Context);
      Assert (Valid, "Invalid TLV message");
      if Valid then
         Valid := In_TLV.Contains.Null_Msg_Message_In_TLV_Message_Value (TLV_Message_Context);
         Assert (Valid, "TLV message contains no null message");
      end if;

      TLV.Message.Take_Buffer (TLV_Message_Context, Buffer);
      RFLX_Types.Free (Buffer);

      Assert (TLV_Message_Context.Last'Image, RFLX_Builtin_Types.Bit_Length (24)'Image, "Invalid Context.Last");
   end Test_Null_In_TLV;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Null_In_TLV'Access, "Null message in TLV");
   end Register_Tests;

end RFLX.In_TLV_Tests;
