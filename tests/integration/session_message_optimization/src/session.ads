pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

with RFLX.RFLX_Types;
with RFLX.Test.Option_Data;
with RFLX.Test.Session;

package Session with
   SPARK_Mode,
   Elaborate_Body
is

   type Context is new RFLX.Test.Session.Context with null record;

   pragma Warnings (Off, """Ctx"" is not modified, could be IN");
   pragma Warnings (Off, "unused variable ""Ctx""");

   overriding
   procedure Get_Option_Data
      (Ctx          : in out Context;
       Data         :        RFLX.RFLX_Types.Bytes;
       Result       :    out RFLX.Test.Option_Data.Structure);

   pragma Warnings (On, "unused variable ""Ctx""");
   pragma Warnings (On, """Ctx"" is not modified, could be IN");

end Session;
