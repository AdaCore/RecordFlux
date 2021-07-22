with Ada.Streams;

package body Channel is

   use type RFLX.RFLX_Builtin_Types.Index;
   use type Ada.Streams.Stream_Element_Offset;

   function To_Ada_Stream (Buffer : RFLX.RFLX_Builtin_Types.Bytes) return Ada.Streams.Stream_Element_Array with
     Pre => Buffer'First = 1
   is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Data (Ada.Streams.Stream_Element_Offset (I)) := Ada.Streams.Stream_Element (Buffer (I));
      end loop;
      return Data;
   end To_Ada_Stream;

   function To_RFLX_Bytes (Buffer : Ada.Streams.Stream_Element_Array) return RFLX.RFLX_Builtin_Types.Bytes with
     Pre => Buffer'First = 1
   is
      Data : RFLX.RFLX_Builtin_Types.Bytes (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Data (RFLX.RFLX_Builtin_Types.Index (I)) := RFLX.RFLX_Builtin_Types.Byte (Buffer (I));
      end loop;
      return Data;
   end To_RFLX_Bytes;

   procedure Send (Buffer : RFLX.RFLX_Builtin_Types.Bytes)
   is
      Data : constant Ada.Streams.Stream_Element_Array (1 .. Buffer'Length) := To_Ada_Stream (Buffer);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      pragma Unreferenced (Last);
      GNAT.Sockets.Send_Socket (Socket => Send_Socket,
                                Item => Data,
                                Last => Last,
                                To => GNAT.Sockets.Sock_Addr_Type'(Family => GNAT.Sockets.Family_Inet,
                                                                   Addr => GNAT.Sockets.Inet_Addr ("255.255.255.255"),
                                                                   Port => 67));
      Network := True;
   end Send;

   procedure Receive (Buffer : out RFLX.RFLX_Builtin_Types.Bytes; Length : out RFLX.RFLX_Builtin_Types.Length) is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Receive_Socket (Socket => Receive_Socket,
                                   Item => Data,
                                   Last => Last);
      Buffer := To_RFLX_Bytes (Data);
      Length := RFLX.RFLX_Builtin_Types.Length (Last);
   end Receive;

   function Has_Data return Boolean is
      Request : GNAT.Sockets.Request_Type := GNAT.Sockets.Request_Type'(Name => GNAT.Sockets.N_Bytes_To_Read,
                                                                        Size => 0);
   begin
      GNAT.Sockets.Control_Socket (Socket => Receive_Socket, Request => Request);
      return Request.Size > 0;
   end Has_Data;

begin

   GNAT.Sockets.Create_Socket (Socket => Send_Socket,
                               Family => GNAT.Sockets.Family_Inet,
                               Mode => GNAT.Sockets.Socket_Datagram);
   GNAT.Sockets.Set_Socket_Option (Socket => Send_Socket,
                                   Level => GNAT.Sockets.Socket_Level,
                                   Option => (GNAT.Sockets.Broadcast, True));
   GNAT.Sockets.Create_Socket (Socket => Receive_Socket,
                               Family => GNAT.Sockets.Family_Inet,
                               Mode => GNAT.Sockets.Socket_Datagram);
   GNAT.Sockets.Set_Socket_Option (Socket => Receive_Socket,
                                   Level => GNAT.Sockets.Socket_Level,
                                   Option => (GNAT.Sockets.Broadcast, True));
   GNAT.Sockets.Set_Socket_Option (Socket => Receive_Socket,
                                   Level => GNAT.Sockets.Socket_Level,
                                   Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                              Timeout => GNAT.Sockets.Timeval_Duration (5)));
   GNAT.Sockets.Bind_Socket (Socket  => Receive_Socket,
                             Address => (Family => GNAT.Sockets.Family_Inet,
                                         Addr => GNAT.Sockets.Any_Inet_Addr,
                                         Port => 68));

end Channel;
