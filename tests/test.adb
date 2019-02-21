with Ada.Command_Line;
with AUnit.Reporter.Text;
with AUnit.Run;
use type Aunit.Status;

with Test_Suite;

procedure Test is
   function Run is new Aunit.Run.Test_Runner_With_Status (Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Status : Aunit.Status := Run (Reporter);
begin
   if Status = Aunit.Success then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test;
