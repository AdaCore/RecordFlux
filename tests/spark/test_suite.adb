with RFLX.Builtin_Types_Tests;
with RFLX.Custom_Types_Tests;
with RFLX.Ethernet.Tests;
with RFLX.IPv4.Tests;
with RFLX.In_Ethernet.Tests;
with RFLX.In_IPv4.Tests;
with RFLX.TLV.Tests;
with RFLX.In_TLV.Tests;
with RFLX.Enumeration.Tests;
with RFLX.Arrays.Tests;
with RFLX.Derivation.Tests;
with RFLX.Expression.Tests;

package body Test_Suite is

   pragma Warnings (Off, "use of an anonymous access type allocator");

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.all.Add_Test (new RFLX.Builtin_Types_Tests.Test);
      Result.all.Add_Test (new RFLX.Custom_Types_Tests.Test);
      Result.all.Add_Test (new RFLX.Ethernet.Tests.Test);
      Result.all.Add_Test (new RFLX.IPv4.Tests.Test);
      Result.all.Add_Test (new RFLX.In_Ethernet.Tests.Test);
      Result.all.Add_Test (new RFLX.In_IPv4.Tests.Test);
      Result.all.Add_Test (new RFLX.TLV.Tests.Test);
      Result.all.Add_Test (new RFLX.In_TLV.Tests.Test);
      Result.all.Add_Test (new RFLX.Enumeration.Tests.Test);
      Result.all.Add_Test (new RFLX.Arrays.Tests.Test);
      Result.all.Add_Test (new RFLX.Derivation.Tests.Test);
      Result.all.Add_Test (new RFLX.Expression.Tests.Test);
      return Result;
   end Suite;

   pragma Warnings (On, "use of an anonymous access type allocator");

end Test_Suite;
