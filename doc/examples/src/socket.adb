with Ada.Containers.Hashed_Sets;
with Ada.Streams;
with Ada.Strings.Hash;

with GNAT.Sockets;

package body Socket with
   SPARK_Mode => Off
is
   Channel : GNAT.Sockets.Socket_Type;
   use type GNAT.Sockets.Port_Type;
   use type GNAT.Sockets.Sock_Addr_Type;

   function Hash_Addr
     (Addr : GNAT.Sockets.Sock_Addr_Type) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (GNAT.Sockets.Image (Addr)));

   package Client_Set is new Ada.Containers.Hashed_Sets
     (Element_Type        => GNAT.Sockets.Sock_Addr_Type, Hash => Hash_Addr,
      Equivalent_Elements => "=");

   Clients : Client_Set.Set;

   procedure Initialize (Port : Natural) is
   begin
      GNAT.Sockets.Create_Socket
        (Socket => Channel, Mode => GNAT.Sockets.Socket_Datagram);
      GNAT.Sockets.Set_Socket_Option
        (Socket => Channel, Level => GNAT.Sockets.IP_Protocol_For_IP_Level,
         Option => (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket
        (Socket  => Channel,
         Address =>
           (Family => GNAT.Sockets.Family_Inet,
            Addr   => GNAT.Sockets.Any_Inet_Addr,
             Port  => GNAT.Sockets.Port_Type (Port)));
   end Initialize;

   procedure Send (Data : RFLX.RFLX_Types.Bytes) is
      Send_Data : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      use type RFLX.RFLX_Types.Index;
      use type Ada.Streams.Stream_Element_Offset;
   begin

      for I in Data'Range loop
         Send_Data
           (Send_Data'First +
            Ada.Streams.Stream_Element_Offset (I - Data'First)) :=
           Ada.Streams.Stream_Element (Data (I));
      end loop;

      for Client of Clients loop

         declare
            Last : Ada.Streams.Stream_Element_Offset :=
              Send_Data'First +
              Ada.Streams.Stream_Element_Offset (Data'Length) - 1;
         begin
            GNAT.Sockets.Send_Socket
              (Socket => Channel, Item => Send_Data, Last => Last,
               To     => Client);
         end;
      end loop;

   end Send;

   procedure Receive (Data : out RFLX.RFLX_Types.Bytes; Success : out Boolean)
   is
      Recv_Data : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      Last      : Ada.Streams.Stream_Element_Offset;
      From      : GNAT.Sockets.Sock_Addr_Type;
      use type RFLX.RFLX_Types.Index;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Success := False;

      GNAT.Sockets.Receive_Socket
        (Socket => Channel, Item => Recv_Data, Last => Last, From => From);

      for I in Recv_Data'First .. Last loop
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

      if not Clients.Contains (From) then
         Clients.Insert (From);
      end if;

      Success := True;
   end Receive;

end Socket;
