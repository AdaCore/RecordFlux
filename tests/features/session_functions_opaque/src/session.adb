package body Session with
   SPARK_Mode
is

   overriding
   procedure Check_Size
      (Ctx    : in out Context;
       Size   :        RFLX.Test.Size;
       Data   :        RFLX.RFLX_Types.Bytes;
       Result :    out Boolean)
   is
      use type RFLX.Test.Size;
   begin
      Result := Size / 8 = Data'Length;
   end Check_Size;

end Session;
