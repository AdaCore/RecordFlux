pragma Style_Checks ("N3aAbCdefhiIklnOprStux");

package body RFLX.Test.Session_Functions with
   SPARK_Mode
is

   function Initialize return Context is
      (null record);

   procedure Finalize (Ctx : in out Context)
   is
      pragma Unreferenced (Ctx);
   begin
      null;
   end Finalize;

end RFLX.Test.Session_Functions;
