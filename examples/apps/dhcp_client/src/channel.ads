with GNAT.Sockets;
with RFLX.RFLX_Builtin_Types;

package Channel with
   SPARK_Mode
is

   procedure Initialize (Socket : out GNAT.Sockets.Socket_Type) with
      Global =>
         null;

   procedure Send (Socket : in out GNAT.Sockets.Socket_Type;
                   Buffer :        RFLX.RFLX_Builtin_Types.Bytes) with
      Global =>
         null;

   use type RFLX.RFLX_Builtin_Types.Length;

   procedure Receive (Socket : in out GNAT.Sockets.Socket_Type;
                      Buffer :    out RFLX.RFLX_Builtin_Types.Bytes;
                      Length :    out RFLX.RFLX_Builtin_Types.Length) with
      Post =>
         Length <= Buffer'Length,
      Global =>
         null;

end Channel;
