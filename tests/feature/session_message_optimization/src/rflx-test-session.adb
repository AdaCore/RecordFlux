package body RFLX.Test.Session with
   SPARK_Mode
is

   procedure Get_Option_Data
      (Ctx         : in out RFLX.Test.Session_Functions.Context;
       Data        :        RFLX.RFLX_Types.Bytes;
       RFLX_Result :    out RFLX.Test.Option_Data.Structure)
   is
      pragma Unreferenced (Ctx);
      use type RFLX.RFLX_Types.Index;
   begin
      RFLX_Result.Length := Data'Length;
      RFLX_Result.Data := (others => 0);
      RFLX_Result.Data (RFLX_Result.Data'First .. RFLX_Result.Data'First + Data'Length - 1) := Data;
   end Get_Option_Data;

end RFLX.Test.Session;
