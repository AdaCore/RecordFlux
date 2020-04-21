with Ada.Command_Line;
with AUnit.Reporter.Text;
with AUnit.Run;
use type AUnit.Status;

with Test_Suite;

procedure Test is
   function Run is new AUnit.Run.Test_Runner_With_Status (Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Status : constant AUnit.Status := Run (Reporter);
begin
   if Status = AUnit.Success then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test;
