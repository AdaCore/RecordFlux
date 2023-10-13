with Socket;
with Msg_Write;

procedure DCCP_Client with
  SPARK_Mode => On
is
   Channel : Socket.Channel := Socket.Initialize (1234);
begin
   Msg_Write.Send_Request (Channel);
   Msg_Write.Send_Response (Channel);
   Msg_Write.Send_Ack (Channel);
   Msg_Write.Send_Data_Ack (Channel);
   Msg_Write.Send_Data (Channel);
   Msg_Write.Send_Close (Channel);
   Msg_Write.Send_Reset (Channel);

   Socket.Close (Channel);
end DCCP_Client;
