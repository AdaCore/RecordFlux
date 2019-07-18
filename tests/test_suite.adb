with RFLX.Types.Tests;
with RFLX.Ethernet.Tests;
with RFLX.IPv4.Tests;
with RFLX.In_Ethernet.Tests;
with RFLX.In_IPv4.Tests;
with RFLX.TLV.Tests;
with RFLX.Enumeration.Tests;
with RFLX.Arrays.Tests;
with RFLX.Derivation.Tests;
--  with RFLX.Expression.Tests;  --  ISSUE: Componolit/RecordFlux#60

package body Test_Suite is

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (new RFLX.Types.Tests.Test);
      Result.Add_Test (new RFLX.Ethernet.Tests.Test);
      Result.Add_Test (new RFLX.IPv4.Tests.Test);
      Result.Add_Test (new RFLX.In_Ethernet.Tests.Test);
      Result.Add_Test (new RFLX.In_IPv4.Tests.Test);
      Result.Add_Test (new RFLX.TLV.Tests.Test);
      Result.Add_Test (new RFLX.Enumeration.Tests.Test);
      Result.Add_Test (new RFLX.Arrays.Tests.Test);
      Result.Add_Test (new RFLX.Derivation.Tests.Test);
      --  Result.Add_Test (new RFLX.Expression.Tests.Test);  --  ISSUE: Componolit/RecordFlux#60
      return Result;
   end Suite;

end Test_Suite;
