package body RFLX.SPDM_Responder.Session_Functions with
   SPARK_Mode
is

   function Initialize return Context is
      (Instance => System.Null_Address);

   procedure Finalize (Ctx : in out Context)
   is
   begin
      null;
   end Finalize;

end RFLX.SPDM_Responder.Session_Functions;
