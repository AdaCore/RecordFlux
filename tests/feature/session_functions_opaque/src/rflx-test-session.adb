package body RFLX.Test.Session with
   SPARK_Mode
is

   procedure Check_Size
      (State       : in out RFLX.Test.Session_Environment.State;
       Size        :        RFLX.Test.Size;
       Data        :        RFLX.RFLX_Types.Bytes;
       RFLX_Result :    out Boolean)
   is
      pragma Unreferenced (State);
   begin
      RFLX_Result := Size / 8 = Data'Length;
   end Check_Size;

end RFLX.Test.Session;
