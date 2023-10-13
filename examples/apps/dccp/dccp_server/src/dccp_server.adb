with Ada.Text_IO;
with RFLX.DCCP.Packet;
with RFLX.RFLX_Types;
with Socket;
with Msg_Read;

procedure DCCP_Server with
  SPARK_Mode => On, Annotate => (GNATprove, Might_Not_Return)
is
   use RFLX;

   Channel : Socket.Channel       := Socket.Initialize (1_234, Server => True);
   Buffer  : RFLX_Types.Bytes_Ptr :=
     new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
   Last    : RFLX_Types.Index;
   Success : Boolean;
   Context : DCCP.Packet.Context;
   use type RFLX_Types.Bytes_Ptr;
   use type RFLX_Types.Index;
begin
   pragma Warnings
     (Off, """Context"" is set by ""*"" but not used after the call");

   Ada.Text_IO.Put_Line ("Server started");

   loop
      pragma Loop_Invariant (Buffer /= null);
      pragma Loop_Invariant (Buffer'Last < RFLX_Types.Index'Last);
      pragma Loop_Invariant (not DCCP.Packet.Has_Buffer (Context));

      --  Listen for data on the connection
      Socket.Receive (Channel, Buffer.all, Last, Success);
      if not Success or Buffer'Length = 0 then
         Ada.Text_IO.Put_Line ("Socket failure :(");
         exit;
      end if;

      Ada.Text_IO.Put_Line ("Got message, length " & Last'Image);

      --  Get Context ready
      DCCP.Packet.Initialize
        (Context, Buffer, RFLX.RFLX_Types.To_Last_Bit_Index (Last));

      --  Verify Context lines up with spec'd fields
      DCCP.Packet.Verify_Message (Context);

      if DCCP.Packet.Well_Formed_Message (Context) then
         declare
            Msg_Type : constant DCCP.Type_Field :=
              DCCP.Packet.Get_Packet_Type (Context);
         begin
            case Msg_Type is
               when DCCP.DCCP_REQUEST =>
                  Msg_Read.DCCP_REQUEST (Context);

               when DCCP.DCCP_ACK =>
                  Msg_Read.DCCP_ACK (Context);

               when DCCP.DCCP_DATA_ACK =>
                  Msg_Read.DCCP_DATA_ACK (Context);

               when DCCP.DCCP_DATA =>
                  Msg_Read.DCCP_DATA (Context);

               when DCCP.DCCP_CLOSE =>
                  Msg_Read.DCCP_CLOSE (Context);

               when DCCP.DCCP_RESET =>
                  Msg_Read.DCCP_RESET (Context);

               when DCCP.DCCP_RESPONSE =>
                  Msg_Read.DCCP_RESPONSE (Context);

               when others =>
                  Ada.Text_IO.Put_Line ("Unsupported Message Received!");
            end case;
         end;
      else
         Ada.Text_IO.Put_Line ("Message invalid");
      end if;

      DCCP.Packet.Take_Buffer (Context, Buffer);

   end loop;

   RFLX.RFLX_Types.Free (Buffer);
   Socket.Close (Channel);

end DCCP_Server;
