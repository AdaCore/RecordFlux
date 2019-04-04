with Types; use type Types.Bytes;

with SPARK.Assertions; use SPARK.Assertions;
with SPARK.File_IO; use SPARK.File_IO;

with Expression.Message;

package body Expression.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Expression");
   end Name;

   procedure Test_Expression_Valid (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Types.Bytes := Types.Bytes'(1, 2);
      Valid  : Boolean;
   begin
      Expression.Message.Label (Buffer);
      Valid := Expression.Message.Valid_Payload (Buffer);
      Assert (Valid, "Invalid Payload");
      if Valid then
         Assert (Buffer (Expression.Message.Get_Payload_First (Buffer) .. Expression.Message.Get_Payload_Last (Buffer)) = (1, 2), "Invalid Payload Content");
      end if;
      Assert (Expression.Message.Is_Valid (Buffer), "Invalid Message");
   end Test_Expression_Valid;

   procedure Test_Expression_Invalid (T : in out Aunit.Test_Cases.Test_Case'Class)
     with SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      Buffer : Types.Bytes := Types.Bytes'(1, 1);
      Valid  : Boolean;
   begin
      Expression.Message.Label (Buffer);
      Valid := Expression.Message.Valid_Payload (Buffer);
      Assert (not Valid, "Valid Payload");
      Assert (not Expression.Message.Is_Valid (Buffer), "Valid Message");
   end Test_Expression_Invalid;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Expression_Valid'Access, "Valid");
      Register_Routine (T, Test_Expression_Invalid'Access, "Invalid");
   end Register_Tests;

end Expression.Tests;
