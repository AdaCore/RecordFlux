with Socket;

package Msg_Write with
  SPARK_Mode
is
   procedure Send_Request (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

   procedure Send_Response (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

   procedure Send_Ack (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

   procedure Send_Data_Ack (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

   procedure Send_Data (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

   procedure Send_Close (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

   procedure Send_Reset (Channel : Socket.Channel)
     with Pre => Socket.Is_Open (Channel);

end Msg_Write;
