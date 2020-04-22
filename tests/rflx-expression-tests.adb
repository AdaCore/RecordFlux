with RFLX.RFLX_Types; use type RFLX.RFLX_Types.Bytes;

with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with RFLX.Expression.Message;

package body RFLX.Expression.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Expression");
   end Name;

   Payload_Content : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 1);

   procedure Store_Payload (Buffer : RFLX_Types.Bytes) is
   begin
      Payload_Content := Buffer;
   end Store_Payload;

   procedure Get_Payload_Content is new Expression.Message.Get_Payload (Store_Payload);

   procedure Test_Expression_Valid (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : RFLX_Types.Bytes_Ptr := new RFLX_Types.Bytes'(1, 2);
      Context : Expression.Message.Context := Expression.Message.Create;
   begin
      Expression.Message.Initialize (Context, Buffer);

      Expression.Message.Verify_Message (Context);

      if Expression.Message.Valid (Context, Expression.Message.Payload) then
         Get_Payload_Content (Context);
         Assert (Payload_Content = (1, 2), "Invalid Payload Content");
      else
         Assert (False, "Invalid Payload");
      end if;
      Assert (Expression.Message.Structural_Valid_Message (Context), "Invalid Message");
   end Test_Expression_Valid;

   procedure Test_Expression_Invalid (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : RFLX_Types.Bytes_Ptr := new RFLX_Types.Bytes'(1, 1);
      Context : Expression.Message.Context := Expression.Message.Create;
   begin
      Expression.Message.Initialize (Context, Buffer);

      Expression.Message.Verify_Message (Context);

      Assert (not Expression.Message.Valid (Context, Expression.Message.Payload), "Valid Payload");
      Assert (not Expression.Message.Structural_Valid_Message (Context), "Valid Message");
   end Test_Expression_Invalid;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Expression_Valid'Access, "Valid");
      Register_Routine (T, Test_Expression_Invalid'Access, "Invalid");
   end Register_Tests;

end RFLX.Expression.Tests;
