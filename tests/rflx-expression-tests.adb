with RFLX.RFLX_Builtin_Types; use type RFLX.RFLX_Builtin_Types.Bytes, RFLX.RFLX_Builtin_Types.Length;

with SPARK.Assertions; use SPARK.Assertions;

with RFLX.Expression.Message;

package body RFLX.Expression.Tests is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Expression");
   end Name;

   Payload_Content : RFLX_Builtin_Types.Bytes (RFLX_Builtin_Types.Index'First .. RFLX_Builtin_Types.Index'First + 1);

   procedure Store_Payload (Buffer : RFLX_Builtin_Types.Bytes) is
   begin
      Payload_Content := Buffer;
   end Store_Payload;

   procedure Get_Payload_Content is new Expression.Message.Get_Payload (Store_Payload);

   --  WORKAROUND: Componolit/Workarounds#7
   pragma Warnings (Off, "unused assignment to ""Buffer""");

   procedure Test_Expression_Valid (T : in out AUnit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Pre => True
   is
      pragma Unreferenced (T);
      Buffer : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 2);
      Context : Expression.Message.Context := Expression.Message.Create;
   begin
      Expression.Message.Initialize (Context, Buffer);

      Expression.Message.Verify_Message (Context);

      if Expression.Message.Structural_Valid (Context, Expression.Message.F_Payload) then
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
      Buffer : RFLX_Builtin_Types.Bytes_Ptr := new RFLX_Builtin_Types.Bytes'(1, 1);
      Context : Expression.Message.Context := Expression.Message.Create;
   begin
      Expression.Message.Initialize (Context, Buffer);

      Expression.Message.Verify_Message (Context);

      Assert (not Expression.Message.Structural_Valid (Context, Expression.Message.F_Payload),
              "Structural Valid Payload");
      Assert (not Expression.Message.Structural_Valid_Message (Context), "Structural Valid Message");
   end Test_Expression_Invalid;

   overriding
   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Expression_Valid'Access, "Valid");
      Register_Routine (T, Test_Expression_Invalid'Access, "Invalid");
   end Register_Tests;

end RFLX.Expression.Tests;
