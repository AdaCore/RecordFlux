package body RFLX.Test.Session with
   SPARK_Mode
is
   Global : RFLX.Test.Length := 0;

   procedure Get_Message_Type
      (Ctx         : in out RFLX.Test.Session_Functions.Context;
       RFLX_Result :    out RFLX.Universal.Option_Type)
   is
      pragma Unreferenced (Ctx);
   begin
      RFLX_Result := (Known => True, Enum => RFLX.Universal.OT_Data);
   end Get_Message_Type;

   procedure Create_Message
      (Ctx          : in out RFLX.Test.Session_Functions.Context;
       Message_Type :        RFLX.Universal.Option_Type;
       Length       :        RFLX.Test.Length;
       Data         :        RFLX.RFLX_Types.Bytes;
       RFLX_Result  :    out RFLX.Test.Definite_Message.Structure)
   is
      pragma Unreferenced (Ctx);
      use type RFLX.RFLX_Types.Index;
   begin
      RFLX_Result.Message_Type := Message_Type;
      RFLX_Result.Length := Length;
      RFLX_Result.Data := (others => 0);
      RFLX_Result.Data (RFLX_Result.Data'First .. RFLX_Result.Data'First + Data'Length - 1) := Data;
   end Create_Message;

   procedure Valid_Message
      (Ctx           : in out RFLX.Test.Session_Functions.Context;
       Message_Type  :        RFLX.Universal.Option_Type;
       Strict        :        Boolean;
       RFLX_Result   :    out RFLX.Test.Result)
   is
      pragma Unreferenced (Ctx);
      use type RFLX.Universal.Option_Type;
   begin
      Global := 8;
      RFLX_Result := (if Strict and then Message_Type = (Known => True, Enum => RFLX.Universal.OT_Data)
                 then RFLX.Test.M_Valid
                 else RFLX.Test.M_Invalid);
   end Valid_Message;

   procedure Byte_Size
      (Ctx          : in out RFLX.Test.Session_Functions.Context;
       RFLX_Result  :    out RFLX.Test.Length)
   is
      pragma Unreferenced (Ctx);
   begin
      RFLX_Result := Global;
   end Byte_Size;

end RFLX.Test.Session;
