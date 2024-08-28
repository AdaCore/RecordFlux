with RFLX.Universal;

package body RFLX.Test.Session with
   SPARK_Mode
is

   procedure Get_Option_Data
      (State       : in out RFLX.Test.Session_Environment.State;
       Data        :        RFLX.RFLX_Types.Bytes;
       RFLX_Result :    out RFLX.Test.Option_Data.Structure)
   is
      pragma Unreferenced (State);
      use type RFLX.RFLX_Types.Index;
      use type RFLX.Universal.Length;
      Length : constant RFLX.Universal.Length := (if Data'Length <= RFLX.Universal.Length'Last then Data'Length else 0);
   begin
      RFLX_Result.Length := Length;
      RFLX_Result.Data := (others => 0);
      if Length > 0 then
         RFLX_Result.Data (RFLX_Result.Data'First .. RFLX_Result.Data'First + Data'Length - 1) := Data;
      end if;
   end Get_Option_Data;

end RFLX.Test.Session;
