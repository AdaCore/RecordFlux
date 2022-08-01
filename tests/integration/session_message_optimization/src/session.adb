package body Session with
   SPARK_Mode
is

   overriding
   procedure Get_Option_Data
      (Ctx          : in out Context;
       Data         :        RFLX.RFLX_Types.Bytes;
       Result       :    out RFLX.Test.Option_Data.Structure)
   is
      use type RFLX.RFLX_Types.Index;
   begin
      Result.Length := Data'Length;
      Result.Data := (others => 0);
      Result.Data (Result.Data'First .. Result.Data'First + Data'Length - 1) := Data;
   end Get_Option_Data;

end Session;
