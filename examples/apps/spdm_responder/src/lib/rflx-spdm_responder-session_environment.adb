package body RFLX.SPDM_Responder.Session_Environment with
   SPARK_Mode
is

   procedure Plat_Initialize (State : out Session_Environment.State) with
      SPARK_Mode => Off
   is
      procedure C_Interface (Instance : out System.Address) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_initialize";
   begin
      C_Interface (State.Instance);
   end Plat_Initialize;

end RFLX.SPDM_Responder.Session_Environment;
