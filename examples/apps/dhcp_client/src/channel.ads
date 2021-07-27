with GNAT.Sockets;
with RFLX.RFLX_Builtin_Types;

package Channel with
  SPARK_Mode
is

   Socket : GNAT.Sockets.Socket_Type;

   procedure Send (Buffer : RFLX.RFLX_Builtin_Types.Bytes) with
      Global =>
         (Output => Socket);

   use type RFLX.RFLX_Builtin_Types.Length;

   procedure Receive (Buffer : out RFLX.RFLX_Builtin_Types.Bytes; Length : out RFLX.RFLX_Builtin_Types.Length) with
      Post =>
         Length <= Buffer'Length;

   function Has_Data return Boolean;

end Channel;
