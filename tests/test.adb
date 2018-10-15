with AUnit.Reporter.Text;
with AUnit.Run;
with Test_Suite;

procedure Test is
   procedure Runner is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test;
