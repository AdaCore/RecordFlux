package body Session with
   SPARK_Mode
is

   overriding
   procedure Get_Message_Type
      (Ctx    : in out Context;
       Result :    out RFLX.Universal.Option_Type)
   is
   begin
      Result := (Known => True, Enum => RFLX.Universal.OT_Data);
   end Get_Message_Type;

   overriding
   procedure Create_Message
      (Ctx          : in out Context;
       Message_Type :        RFLX.Universal.Option_Type;
       Length       :        RFLX.Test.Length;
       Data         :        RFLX.RFLX_Types.Bytes;
       Result       :    out RFLX.Test.Definite_Message.Structure)
   is
      use type RFLX.RFLX_Types.Index;
   begin
      Result.Message_Type := Message_Type;
      Result.Length := Length;
      Result.Data := (others => 0);
      Result.Data (Result.Data'First .. Result.Data'First + Data'Length - 1) := Data;
   end Create_Message;

   overriding
   procedure Valid_Message
      (Ctx           : in out Context;
       Message_Type  :        RFLX.Universal.Option_Type;
       Strict        :        Boolean;
       Result        :    out RFLX.Test.Result)
   is
      use type RFLX.Universal.Option_Type;
   begin
      Result := (if Strict and then Message_Type = (Known => True, Enum => RFLX.Universal.OT_Data)
                 then RFLX.Test.M_Valid
                 else RFLX.Test.M_Invalid);
   end Valid_Message;

   overriding
   procedure Byte_Size
      (Ctx           : in out Context;
       Result        :    out RFLX.Test.Length)
   is
   begin
      Result := 8;
   end Byte_Size;

end Session;
