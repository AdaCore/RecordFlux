package body Channel with
   SPARK_Mode => Off
is

   procedure Send (Buffer : RFLX.RFLX_Types.Bytes) is null;

   procedure Receive (Buffer : out RFLX.RFLX_Types.Bytes; Length : out RFLX.RFLX_Types.Length) is null;

   function Has_Message return Boolean is (False);

end Channel;
