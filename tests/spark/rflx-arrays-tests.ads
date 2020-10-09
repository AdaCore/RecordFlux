with AUnit;
with AUnit.Test_Cases;

package RFLX.Arrays.Tests is
   pragma Elaborate_Body;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding
   function Name (T : Test) return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test);

end RFLX.Arrays.Tests;
