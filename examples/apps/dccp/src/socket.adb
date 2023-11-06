with Ada.Streams;

package body Socket with
  SPARK_Mode => Off
is
   function Is_Open (Chan : Channel) return Boolean is (Chan.Is_Open);

   function Initialize
     (Port : Natural; Server : Boolean := False) return Channel
   is
      Socket : GNAT.Sockets.Socket_Type;
   begin
      GNAT.Sockets.Create_Socket
        (Socket => Socket, Mode => GNAT.Sockets.Socket_Datagram);

      GNAT.Sockets.Set_Socket_Option
        (Socket => Socket, Level => GNAT.Sockets.IP_Protocol_For_IP_Level,
         Option => (GNAT.Sockets.Reuse_Address, True));

      if Server then
         GNAT.Sockets.Bind_Socket
           (Socket  => Socket,
            Address =>
              (Family => GNAT.Sockets.Family_Inet,
               Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
               Port   => GNAT.Sockets.Port_Type (Port)));
      end if;

      return
        Channel'
          (Socket  => Socket, Port => GNAT.Sockets.Port_Type (Port),
           Is_Open => True);
   end Initialize;

   procedure Receive
     (Chan :     Channel; Data : out RFLX.RFLX_Types.Bytes;
      Last : out RFLX.RFLX_Types.Index; Success : out Boolean)
   is
      Recv_Data : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      Recv_Last : Ada.Streams.Stream_Element_Offset;
      From      : GNAT.Sockets.Sock_Addr_Type;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Success := False;

      GNAT.Sockets.Receive_Socket
        (Socket => Chan.Socket, Item => Recv_Data, Last => Recv_Last,
         From   => From);

      for I in Recv_Data'First .. Recv_Last loop
         declare
            J : constant RFLX.RFLX_Types.Index :=
              Data'First + RFLX.RFLX_Types.Index (I) -
              RFLX.RFLX_Types.Index (Recv_Data'First);
         begin
            if J not in Data'Range then
               return;
            end if;
            Data (J) := RFLX.RFLX_Types.Byte (Recv_Data (I));
         end;
      end loop;

      Last := Data'First + RFLX.RFLX_Types.Index (Recv_Last - Recv_Data'First);
      Success := True;
   end Receive;

   procedure Close (Chan : in out Channel) is
   begin
      GNAT.Sockets.Close_Socket (Chan.Socket);
      Chan.Is_Open := False;
   end Close;

   procedure Send (Chan : Channel; Data : RFLX.RFLX_Types.Bytes) is
      Unused_Last : Ada.Streams.Stream_Element_Offset;
      Address     : GNAT.Sockets.Sock_Addr_Type;
      Send_Data   : Ada.Streams.Stream_Element_Array (1 .. Data'Length);
      use type Ada.Streams.Stream_Element_Offset;
   begin

      for I in Data'Range loop
         Send_Data
           (Send_Data'First +
            Ada.Streams.Stream_Element_Offset (I - Data'First)) :=
           Ada.Streams.Stream_Element (Data (I));
      end loop;

      Address.Port := Chan.Port;
      Address.Addr := GNAT.Sockets.Inet_Addr ("127.0.0.1");

      GNAT.Sockets.Send_Socket
        (Socket => Chan.Socket, Item => Send_Data, Last => Unused_Last,
         To     => Address);
   end Send;
end Socket;
