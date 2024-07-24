pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

package RFLX.Test.Session_Functions with
   SPARK_Mode
is

   type Context is null record;

   function Initialize return Context;

   procedure Finalize (Ctx : in out Context);

end RFLX.Test.Session_Functions;
