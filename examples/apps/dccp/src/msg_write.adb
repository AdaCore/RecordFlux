with Ada.Text_IO;
with RFLX.DCCP.Packet;
with RFLX.DCCP.Option;
with RFLX.DCCP.Options;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

package body Msg_Write with
  SPARK_Mode
is
   use RFLX;
   use type RFLX.RFLX_Builtin_Types.Bit_Length;

   pragma Warnings
     (Off, """*Context"" is set by ""*"" but not used after the call");

   procedure Send
     (Channel :     Socket.Channel; Context : in out DCCP.Packet.Context;
      Buffer  : out RFLX.RFLX_Types.Bytes_Ptr) with
     Pre  =>
      DCCP.Packet.Has_Buffer (Context)
      and then not Context'Constrained
      and then Socket.Is_Open (Channel),
     Post =>
      not DCCP.Packet.Has_Buffer (Context) and then not Context'Constrained
   is
      Last : RFLX.RFLX_Types.Index;
   begin
      DCCP.Packet.Verify_Message (Context);
      if not DCCP.Packet.Well_Formed_Message (Context) then
         DCCP.Packet.Take_Buffer (Context, Buffer);
         Ada.Text_IO.Put_Line ("Invalid message created!");
         return;
      end if;
      Last := RFLX.RFLX_Types.To_Index (DCCP.Packet.Message_Last (Context));
      DCCP.Packet.Take_Buffer (Context, Buffer);
      Socket.Send (Channel, Buffer.all (Buffer'First .. Last));
   end Send;

   --  *****************************
   --  Send a simple REQUEST Message
   --  *****************************
   procedure Send_Request (Channel : Socket.Channel) is
      --  The Buffer is used for raw message data
      Buffer  : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Context : DCCP.Packet.Context;
   begin
      Ada.Text_IO.Put ("Start DCCP REQUEST Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      DCCP.Packet.Set_Source_Port (Context, 32_772);
      DCCP.Packet.Set_Destination_Port (Context, 5_001);
      DCCP.Packet.Set_Data_Offset (Context, 5);
      DCCP.Packet.Set_CCVal (Context, 0);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 16#08_DB#);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_REQUEST);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 16#00_04_29_01_6D_DC#);
      DCCP.Packet.Set_Service_Code (Context, 0);
      DCCP.Packet.Set_Data (Context, RFLX.RFLX_Types.Bytes'(1 .. 0 => 0));

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      Ada.Text_IO.Put_Line ("Finished!");
      RFLX.RFLX_Types.Free (Buffer);
   end Send_Request;

   --  ******************************
   --  Send a simple RESPONSE Message
   --  ******************************
   procedure Send_Response (Channel : Socket.Channel) is
      Buffer  : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Context : DCCP.Packet.Context;
   begin
      Ada.Text_IO.Put ("Start DCCP RESPONSE Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      DCCP.Packet.Set_Source_Port (Context, 5_001);
      DCCP.Packet.Set_Destination_Port (Context, 32_772);
      DCCP.Packet.Set_Data_Offset (Context, 7);
      DCCP.Packet.Set_CCVal (Context, 0);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 27_074);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_RESPONSE);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 0);
      DCCP.Packet.Set_Ack_Reserved_Long (Context, 0);
      DCCP.Packet.Set_Ack_Number_Long (Context, 0);
      DCCP.Packet.Set_Service_Code (Context, 0);

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      RFLX.RFLX_Types.Free (Buffer);

      Ada.Text_IO.Put_Line ("Finished!");
   end Send_Response;

   --  *************************
   --  Send a simple ACK Message
   --  *************************
   procedure Send_Ack (Channel : Socket.Channel) is
      --  The Buffer is used for raw message data
      Buffer                : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Context               : DCCP.Packet.Context;
      Opt_Context           : DCCP.Option.Context;
      Options_Array_Context : DCCP.Options.Context;

   begin
      Ada.Text_IO.Put ("Start DCCP ACK Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      DCCP.Packet.Set_Source_Port (Context, 32_772);
      DCCP.Packet.Set_Destination_Port (Context, 5_001);
      DCCP.Packet.Set_Data_Offset (Context, 11);
      DCCP.Packet.Set_CCVal (Context, 0);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 16#04_80#);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_ACK);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 16#00_04_29_01_6D_DD#);
      DCCP.Packet.Set_Ack_Reserved_Long (Context, 0);
      DCCP.Packet.Set_Ack_Number_Long (Context, 16#00_08_F0_E9_9B_42#);

      DCCP.Packet.Switch_To_Options (Context, Options_Array_Context);

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  PADDING

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.PADDING);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  PADDING

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.PADDING);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  CCID3_RCV_RATE

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.CCID3_RCV_RATE);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 6);
      --  4 bytes
      DCCP.Option.Set_Receive_Rate (Opt_Context, 0);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  CCID3_RCV_RATE

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.CCID3_LOSS_EVT_RATE);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 6);
      --  4 bytes
      DCCP.Option.Set_Loss_Event_Rate (Opt_Context, 0);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  TIMESTAMP

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.TIMESTAMP);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 6);
      --  4 bytes
      DCCP.Option.Set_Timestamp_Option (Opt_Context, 16#F9_F5_C7_03#);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      pragma Assert
        (DCCP.Options.Size (Options_Array_Context) =
         DCCP.Packet.Field_Size (Context, DCCP.Packet.F_Options));
      DCCP.Packet.Update_Options (Context, Options_Array_Context);

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      RFLX.RFLX_Types.Free (Buffer);

      Ada.Text_IO.Put_Line ("Finished!");
   end Send_Ack;

   --  ******************************
   --  Send a simple DATA ACK Message
   --  ******************************
   procedure Send_Data_Ack (Channel : Socket.Channel) is
      Buffer                : RFLX.RFLX_Types.Bytes_Ptr      :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Context               : DCCP.Packet.Context;
      Opt_Context           : DCCP.Option.Context;
      Options_Array_Context : DCCP.Options.Context;
      Data                  : constant RFLX.RFLX_Types.Bytes :=
        (16#20#, 16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#,
         16#28#, 16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#,
         16#30#, 16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#,
         16#38#, 16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#,
         16#40#, 16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#,
         16#48#, 16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#,
         16#50#, 16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#,
         16#58#, 16#59#, 16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#,
         16#60#, 16#61#, 16#62#, 16#63#, 16#64#, 16#65#, 16#66#, 16#67#,
         16#68#, 16#69#, 16#6a#, 16#6b#, 16#6c#, 16#6d#, 16#6e#, 16#6f#,
         16#70#, 16#71#, 16#72#, 16#73#, 16#74#, 16#75#, 16#76#, 16#77#,
         16#78#, 16#79#, 16#7a#, 16#7b#, 16#7c#, 16#7d#, 16#7e#, 16#20#,
         16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#,
         16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#, 16#30#,
         16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#,
         16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#, 16#40#,
         16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#,
         16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#, 16#50#,
         16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#, 16#58#,
         16#59#, 16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#, 16#60#,
         16#61#, 16#62#, 16#63#, 16#64#, 16#65#, 16#66#, 16#67#, 16#68#,
         16#69#, 16#6a#, 16#6b#, 16#6c#, 16#6d#, 16#6e#, 16#6f#, 16#70#,
         16#71#, 16#72#, 16#73#, 16#74#, 16#75#, 16#76#, 16#77#, 16#78#,
         16#79#, 16#7a#, 16#7b#, 16#7c#, 16#7d#, 16#7e#, 16#20#, 16#21#,
         16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#, 16#29#,
         16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#, 16#30#, 16#31#,
         16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#, 16#39#,
         16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#, 16#40#, 16#41#,
         16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#, 16#49#,
         16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#, 16#50#, 16#51#,
         16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#, 16#58#, 16#59#,
         16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#, 16#60#, 16#61#);
   begin
      Ada.Text_IO.Put ("Start DCCP DATA ACK Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      --  NOTE: These MUST be set in the order in which they appear in the message.
      --        If set out of order, a "discriminant check" error (CONSTRAINT ERROR)
      --        will be thrown.
      DCCP.Packet.Set_Source_Port (Context, 32_772);
      DCCP.Packet.Set_Destination_Port (Context, 5_001);
      DCCP.Packet.Set_Data_Offset (Context, 12);
      DCCP.Packet.Set_CCVal (Context, 0);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 16#91_1E#);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_DATA_ACK);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 16#00_04_29_01_6D_DE#);
      DCCP.Packet.Set_Ack_Reserved_Long (Context, 0);
      DCCP.Packet.Set_Ack_Number_Long (Context, 16#00_08_F0_E9_9B_42#);

      --  START OPTIONS AREA

      DCCP.Packet.Switch_To_Options (Context, Options_Array_Context);

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  PADDING

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.PADDING);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  PADDING

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.PADDING);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  PADDING

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.PADDING);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  CCID3_RCV_RATE

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.CCID3_RCV_RATE);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 6);
      --  4 bytes
      DCCP.Option.Set_Receive_Rate (Opt_Context, 0);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  CCID3_RCV_RATE

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.CCID3_LOSS_EVT_RATE);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 6);
      --  4 bytes
      DCCP.Option.Set_Loss_Event_Rate (Opt_Context, 0);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  TIMESTAMP

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.TIMESTAMP);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 6);
      --  4 bytes
      DCCP.Option.Set_Timestamp_Option (Opt_Context, 16#F9_F5_C7_17#);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  NDP COUNT

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.NDP_COUNT);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 3);
      --  1 byte
      DCCP.Option.Set_NDP_Count_Opt
        (Opt_Context, (1 => 1)); -- special case for an array of one

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  Finish OPTIONS AREA
      DCCP.Packet.Update_Options (Context, Options_Array_Context);

      --  Set "Data"
      --  Lots'o bytes
      DCCP.Packet.Set_Data (Context, Data);

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      RFLX.RFLX_Types.Free (Buffer);

      Ada.Text_IO.Put_Line ("Finished!");
   end Send_Data_Ack;

   --  **************************
   --  Send a simple DATA Message
   --  **************************
   procedure Send_Data (Channel : Socket.Channel) is
      --  The Buffer is used for raw message data
      Buffer  : RFLX.RFLX_Types.Bytes_Ptr      :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Context : DCCP.Packet.Context;
      Data    : constant RFLX.RFLX_Types.Bytes :=
        (16#20#, 16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#,
         16#28#, 16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#,
         16#30#, 16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#,
         16#38#, 16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#,
         16#40#, 16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#,
         16#48#, 16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#,
         16#50#, 16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#,
         16#58#, 16#59#, 16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#,
         16#60#, 16#61#, 16#62#, 16#63#, 16#64#, 16#65#, 16#66#, 16#67#,
         16#68#, 16#69#, 16#6a#, 16#6b#, 16#6c#, 16#6d#, 16#6e#, 16#6f#,
         16#70#, 16#71#, 16#72#, 16#73#, 16#74#, 16#75#, 16#76#, 16#77#,
         16#78#, 16#79#, 16#7a#, 16#7b#, 16#7c#, 16#7d#, 16#7e#, 16#20#,
         16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#,
         16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#, 16#30#,
         16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#,
         16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#, 16#40#,
         16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#,
         16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#, 16#50#,
         16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#, 16#58#,
         16#59#, 16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#, 16#60#,
         16#61#, 16#62#, 16#63#, 16#64#, 16#65#, 16#66#, 16#67#, 16#68#,
         16#69#, 16#6a#, 16#6b#, 16#6c#, 16#6d#, 16#6e#, 16#6f#, 16#70#,
         16#71#, 16#72#, 16#73#, 16#74#, 16#75#, 16#76#, 16#77#, 16#78#,
         16#79#, 16#7a#, 16#7b#, 16#7c#, 16#7d#, 16#7e#, 16#20#, 16#21#,
         16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#, 16#29#,
         16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#, 16#30#, 16#31#,
         16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#, 16#39#,
         16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#, 16#40#, 16#41#,
         16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#, 16#49#,
         16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#, 16#50#, 16#51#,
         16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#, 16#58#, 16#59#,
         16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#, 16#60#, 16#61#);
   begin
      Ada.Text_IO.Put ("Start DCCP DATA Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      --  NOTE: These MUST be set in the order in which they appear in the message.
      --        If set out of order, a "discriminant check" error (CONSTRAINT ERROR)
      --        will be thrown.
      DCCP.Packet.Set_Source_Port (Context, 32_772);
      DCCP.Packet.Set_Destination_Port (Context, 5_001);
      DCCP.Packet.Set_Data_Offset (Context, 4);
      DCCP.Packet.Set_CCVal (Context, 2);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 16#4D_E1#);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_DATA);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 16#00_04_29_01_6D_E2#);

      DCCP.Packet.Set_Data (Context, Data);

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      RFLX.RFLX_Types.Free (Buffer);

      Ada.Text_IO.Put_Line ("Finished!");
   end Send_Data;

   --  ***************************
   --  Send a simple CLOSE Message
   --  ***************************
   procedure Send_Close (Channel : Socket.Channel) is
      --  The Buffer is used for raw message data
      Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);

      --  Working w/messages requires a context. It holds a ptr to the actual message,
      --  current state of message serialization after parsing, and actual field
      --  vals for scalar types
      Context : DCCP.Packet.Context;
   begin
      Ada.Text_IO.Put ("Start DCCP CLOSE Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      --  NOTE: These MUST be set in the order in which they appear in the message.
      --        If set out of order, a "discriminant check" error (CONSTRAINT ERROR)
      --        will be thrown.

      DCCP.Packet.Set_Source_Port (Context, 32_772);
      DCCP.Packet.Set_Destination_Port (Context, 5_001);
      DCCP.Packet.Set_Data_Offset (Context, 6);
      DCCP.Packet.Set_CCVal (Context, 0);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 16#5B_E4#);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_CLOSE);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 16#00_04_29_01_81_66#);

      DCCP.Packet.Set_Ack_Reserved_Long (Context, 0);
      DCCP.Packet.Set_Ack_Number_Long (Context, 8);

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      RFLX.RFLX_Types.Free (Buffer);

      Ada.Text_IO.Put_Line ("Finished!");
   end Send_Close;

   --  ***************************
   --  Send a simple RESET Message
   --  ***************************
   procedure Send_Reset (Channel : Socket.Channel) is
      --  The Buffer is used for raw message data
      Buffer                : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Context               : DCCP.Packet.Context;
      Opt_Context           : DCCP.Option.Context;
      Options_Array_Context : DCCP.Options.Context;
   begin
      Ada.Text_IO.Put ("Start DCCP RESET Send...");

      DCCP.Packet.Initialize (Context, Buffer);

      --  NOTE: These MUST be set in the order in which they appear in the message.
      --        If set out of order, a "discriminant check" error (CONSTRAINT ERROR)
      --        will be thrown.

      DCCP.Packet.Set_Source_Port (Context, 5_001);
      DCCP.Packet.Set_Destination_Port (Context, 32_772);
      DCCP.Packet.Set_Data_Offset (Context, 8);
      DCCP.Packet.Set_CCVal (Context, 0);
      DCCP.Packet.Set_CsCov (Context, 0);
      DCCP.Packet.Set_Checksum (Context, 16#44_A4#);
      DCCP.Packet.Set_Res_3 (Context, 0);
      DCCP.Packet.Set_Packet_Type (Context, DCCP.DCCP_RESET);
      DCCP.Packet.Set_X (Context, DCCP.EXTENDED);
      DCCP.Packet.Set_Res_8 (Context, 0);
      DCCP.Packet.Set_Sequence_Number_Long (Context, 16#00_08_F0_E9_9B_78#);
      DCCP.Packet.Set_Ack_Reserved_Long (Context, 0);
      DCCP.Packet.Set_Ack_Number_Long (Context, 4);
      DCCP.Packet.Set_Reset_Code (Context, DCCP.CLOSED);
      DCCP.Packet.Set_Data_1 (Context, 0);
      DCCP.Packet.Set_Data_2 (Context, 0);
      DCCP.Packet.Set_Data_3 (Context, 0);

      --  START OPTIONS AREA

      DCCP.Packet.Switch_To_Options (Context, Options_Array_Context);

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  PADDING

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.PADDING);

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  NDP COUNT

      DCCP.Options.Switch (Options_Array_Context, Opt_Context);

      --  1 byte
      DCCP.Option.Set_Option_Type (Opt_Context, DCCP.NDP_COUNT);
      --  1 byte
      DCCP.Option.Set_Option_Length (Opt_Context, 3);
      --  1 byte
      DCCP.Option.Set_NDP_Count_Opt
        (Opt_Context, (1 => 53)); -- special case for an array of one

      DCCP.Options.Update (Options_Array_Context, Opt_Context);

      --  Finish OPTIONS AREA

      DCCP.Packet.Update_Options (Context, Options_Array_Context);

      if DCCP.Packet.Has_Buffer (Context) then
         Send (Channel, Context, Buffer);
      end if;

      RFLX.RFLX_Types.Free (Buffer);

      Ada.Text_IO.Put_Line ("Finished!");
   end Send_Reset;

end Msg_Write;
