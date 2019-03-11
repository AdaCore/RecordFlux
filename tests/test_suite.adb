with Types.Tests;
with Ethernet.Tests;
with IPv4.Tests;
with In_Ethernet.Tests;
with In_IPv4.Tests;
with TLV.Tests;
with Enumeration.Tests;
with Arrays.Tests;

package body Test_Suite is

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (new Types.Tests.Test);
      Result.Add_Test (new Ethernet.Tests.Test);
      Result.Add_Test (new IPv4.Tests.Test);
      Result.Add_Test (new In_Ethernet.Tests.Test);
      Result.Add_Test (new In_IPv4.Tests.Test);
      Result.Add_Test (new TLV.Tests.Test);
      Result.Add_Test (new Enumeration.Tests.Test);
      Result.Add_Test (new Arrays.Tests.Test);
      return Result;
   end Suite;

end Test_Suite;
