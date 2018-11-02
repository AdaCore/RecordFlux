with AUnit;
with AUnit.Test_Cases;

package In_IPv4.Tests is
   pragma Elaborate_Body;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure Register_Tests (T : in out Test);

end In_IPv4.Tests;
