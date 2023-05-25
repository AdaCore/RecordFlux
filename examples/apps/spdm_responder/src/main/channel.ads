pragma SPARK_Mode;

with RFLX.RFLX_Types;

generic
package Channel is

   procedure Send (Buffer : RFLX.RFLX_Types.Bytes);

   procedure Receive (Buffer : out RFLX.RFLX_Types.Bytes; Length : out RFLX.RFLX_Types.Length);

   function Has_Message return Boolean;

end Channel;
