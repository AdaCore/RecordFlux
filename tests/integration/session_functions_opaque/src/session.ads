pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with RFLX.RFLX_Types;
with RFLX.Test.Session;

package Session with
   SPARK_Mode,
   Elaborate_Body
is

   type Context is new RFLX.Test.Session.Context with null record;

   pragma Warnings (Off, """Ctx"" is not modified, could be IN");
   pragma Warnings (Off, "unused variable ""Ctx""");

   overriding
   procedure Check_Size
      (Ctx    : in out Context;
       Size   :        RFLX.Test.Size;
       Data   :        RFLX.RFLX_Types.Bytes;
       Result :    out Boolean);

   pragma Warnings (On, "unused variable ""Ctx""");
   pragma Warnings (On, """Ctx"" is not modified, could be IN");

end Session;
