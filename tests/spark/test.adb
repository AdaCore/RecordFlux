with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Ada.Command_Line;

with RFLX.Builtin_Types_Tests;
with RFLX.Custom_Types_Tests;
with RFLX.Ethernet_Tests;
with RFLX.IPv4_Tests;
with RFLX.In_Ethernet_Tests;
with RFLX.In_IPv4_Tests;
with RFLX.TLV_Tests;
with RFLX.In_TLV_Tests;
with RFLX.Enumeration_Tests;
with RFLX.Arrays_Tests;
with RFLX.Derivation_Tests;
with RFLX.Expression_Tests;

procedure Test is
   pragma Warnings (Off, "use of an anonymous access type allocator");

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.all.Add_Test (new RFLX.Builtin_Types_Tests.Test);
      Result.all.Add_Test (new RFLX.Custom_Types_Tests.Test);
      Result.all.Add_Test (new RFLX.Ethernet_Tests.Test);
      Result.all.Add_Test (new RFLX.IPv4_Tests.Test);
      Result.all.Add_Test (new RFLX.In_Ethernet_Tests.Test);
      Result.all.Add_Test (new RFLX.In_IPv4_Tests.Test);
      Result.all.Add_Test (new RFLX.TLV_Tests.Test);
      Result.all.Add_Test (new RFLX.In_TLV_Tests.Test);
      Result.all.Add_Test (new RFLX.Enumeration_Tests.Test);
      Result.all.Add_Test (new RFLX.Arrays_Tests.Test);
      Result.all.Add_Test (new RFLX.Derivation_Tests.Test);
      Result.all.Add_Test (new RFLX.Expression_Tests.Test);
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
end Test;
