with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Ada.Command_Line;

with RFLX.Sequence_Tests;

procedure Test_Sequence is
   pragma Warnings (Off, "use of an anonymous access type allocator");

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.all.Add_Test (new RFLX.Sequence_Tests.Test);
      return Result;
   end Suite;

   pragma Warnings (On, "use of an anonymous access type allocator");

   function Run is new AUnit.Run.Test_Runner_With_Status (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Status : AUnit.Status;

   use type AUnit.Status;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Status := Run (Reporter);
   Ada.Command_Line.Set_Exit_Status
     (if Status = AUnit.Success then Ada.Command_Line.Success else Ada.Command_Line.Failure);
end Test_Sequence;
