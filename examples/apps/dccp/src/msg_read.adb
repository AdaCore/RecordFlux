with Ada.Text_IO;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.DCCP.Option;
with RFLX.DCCP.Options;

package body Msg_Read with
  SPARK_Mode
is
   use RFLX.RFLX_Builtin_Types;
   use type DCCP.Ext_Seq_Type;
   package Byte_IO is new Ada.Text_IO.Modular_IO
     (RFLX.RFLX_Builtin_Types.Byte);

   pragma Warnings
     (Off, """*Context"" is set by ""*"" but not used after the call");
   pragma Warnings (Off, "subprogram ""Print_Data"" has no effect");

   procedure Print_Data_Block (Data_Block : RFLX.RFLX_Types.Bytes) with
     Always_Terminates;

   procedure Print_Data is new DCCP.Packet.Generic_Get_Data (Print_Data_Block);

   procedure Print_Generic_Header (Ctx : DCCP.Packet.Context) with
     Pre =>
      RFLX.DCCP.Packet.Has_Buffer (Ctx)
      and then DCCP.Packet.Well_Formed_Message (Ctx);

   procedure Print_Sequence_Number_Long (Ctx : DCCP.Packet.Context) with
     Pre =>
      RFLX.DCCP.Packet.Has_Buffer (Ctx)
      and then DCCP.Packet.Well_Formed_Message (Ctx)
      and then DCCP.Packet.Get_X (Ctx) = DCCP.EXTENDED;

   procedure Print_Ack_Number_Long (Ctx : DCCP.Packet.Context) with
     Pre =>
      RFLX.DCCP.Packet.Has_Buffer (Ctx)
      and then DCCP.Packet.Well_Formed_Message (Ctx)
      and then DCCP.Packet.Get_X (Ctx) = DCCP.EXTENDED
      and then DCCP.Packet.Get_Packet_Type (Ctx) /= DCCP.DCCP_DATA
      and then DCCP.Packet.Get_Packet_Type (Ctx) /= DCCP.DCCP_REQUEST;

   --  ***********************
   --  Print a buffer of bytes
   --  ***********************
   procedure Print_Data_Block (Data_Block : RFLX_Types.Bytes) with
     SPARK_Mode => Off
   is
   begin
      for Byte_Chunk of Data_Block loop
         Byte_IO.Put (Byte_Chunk, Base => 16);
         Ada.Text_IO.Put (' ');
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Data_Block;

   --  ***********************************************************************
   --  Read and print the header info that is present in ALL received messages
   --  ***********************************************************************
   procedure Print_Generic_Header (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line
        ("Packet Type: " &
         DCCP.Type_Field'Image (DCCP.Packet.Get_Packet_Type (Ctx)));
      Ada.Text_IO.Put_Line
        ("Source Port: " &
         DCCP.Port_Type'Image (DCCP.Packet.Get_Source_Port (Ctx)));
      Ada.Text_IO.Put_Line
        ("Destination Port: " &
         DCCP.Port_Type'Image (DCCP.Packet.Get_Destination_Port (Ctx)));
      Ada.Text_IO.Put_Line
        ("Data Offset: " &
         DCCP.Data_Offset_Type'Image (DCCP.Packet.Get_Data_Offset (Ctx)));
      Ada.Text_IO.Put_Line
        ("CCVal: " & DCCP.CCVal_Type'Image (DCCP.Packet.Get_CCVal (Ctx)));
      Ada.Text_IO.Put_Line
        ("CsCov: " &
         DCCP.Checksum_Coverage_Type'Image (DCCP.Packet.Get_CsCov (Ctx)));
      Ada.Text_IO.Put_Line
        ("Checksum: " &
         DCCP.Checksum_Type'Image (DCCP.Packet.Get_Checksum (Ctx)));
      Ada.Text_IO.Put_Line
        ("Reserved 3: " &
         DCCP.Reserved_3_Type'Image (DCCP.Packet.Get_Res_3 (Ctx)));
      Ada.Text_IO.Put_Line
        ("Sequence Type: " &
         DCCP.Ext_Seq_Type'Image (DCCP.Packet.Get_X (Ctx)));
   end Print_Generic_Header;

   --  ******************************************
   --  Read and print long sequence number chunks
   --  ******************************************
   procedure Print_Sequence_Number_Long (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line
        ("Reserved 8: " &
         DCCP.Reserved_8_Type'Image (DCCP.Packet.Get_Res_8 (Ctx)));
      Ada.Text_IO.Put_Line
        ("Sequence Number Long: " &
         DCCP.Sequence_Number_Long_Type'Image
           (DCCP.Packet.Get_Sequence_Number_Long (Ctx)));
   end Print_Sequence_Number_Long;

   --  *************************************************
   --  Read and print long acknowledgement number chunks
   --  *************************************************
   procedure Print_Ack_Number_Long (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line
        ("Acknowledge Reserved Long: " &
         DCCP.Reserved_16_Type'Image
           (DCCP.Packet.Get_Ack_Reserved_Long (Ctx)));
      Ada.Text_IO.Put_Line
        ("Acknowledge Number Long: " &
         DCCP.Ack_Number_Long_Type'Image
           (DCCP.Packet.Get_Ack_Number_Long (Ctx)));
   end Print_Ack_Number_Long;

   --  *********************************
   --  Read and print the options header
   --  *********************************
   procedure Print_Options (Ctx : in out DCCP.Packet.Context) with
     Pre =>
      RFLX.DCCP.Packet.Has_Buffer (Ctx)
      and then DCCP.Packet.Well_Formed_Message (Ctx) and then not Ctx'Constrained
      and then DCCP.Packet.Valid (Ctx, DCCP.Packet.F_Options),
     Post => RFLX.DCCP.Packet.Has_Buffer (Ctx) and then not Ctx'Constrained,
     Always_Terminates => False
   is
      Opt_Type_Field : DCCP.Opt_Type;
      Options_Sequence_Context : DCCP.Options.Context;
      Option_Element_Context : DCCP.Option.Context;
   begin
      --  *******
      --  OPTIONS - these are a SEQUENCE field, and require special effort to read
      --  *******

      --  switch message read context from HEADER to Options SEQUENCE
      DCCP.Packet.Switch_To_Options (Ctx, Options_Sequence_Context);

      --  loop through all elements in the Options SEQUENCE, if they exist
      while DCCP.Options.Has_Element (Options_Sequence_Context) loop
         pragma Loop_Invariant
           (DCCP.Options.Has_Buffer (Options_Sequence_Context));
         pragma Loop_Invariant (DCCP.Options.Valid (Options_Sequence_Context));
         pragma Loop_Invariant
           (Options_Sequence_Context.First =
            Options_Sequence_Context.First'Loop_Entry);
         pragma Loop_Invariant
           (Options_Sequence_Context.Last =
            Options_Sequence_Context.Last'Loop_Entry);
         pragma Loop_Invariant
           (not DCCP.Option.Has_Buffer (Option_Element_Context));
         pragma Loop_Invariant
           (Ctx.Buffer_First = Options_Sequence_Context.Buffer_First);
         pragma Loop_Invariant
           (Ctx.Buffer_Last = Options_Sequence_Context.Buffer_Last);

         --  switch message read context from Options SEQUENCE to current ELEMENT in sequence
         DCCP.Options.Switch
           (Options_Sequence_Context, Option_Element_Context);

         --  This MUST be called in order to retrieve fields from the message!
         --  It basically verifies the state of the message and sets flags up so you can access things
         DCCP.Option.Verify_Message (Option_Element_Context);

         if not DCCP.Option.Well_Formed_Message (Option_Element_Context) then
            DCCP.Options.Update
              (Options_Sequence_Context, Option_Element_Context);
            exit;
         end if;

         Opt_Type_Field :=
           DCCP.Option.Get_Option_Type (Option_Element_Context);

         case Opt_Type_Field is
            when DCCP.PADDING =>
               Ada.Text_IO.Put_Line
                 ("Option Type: " & DCCP.Opt_Type'Image (Opt_Type_Field));

            when DCCP.CCID3_RCV_RATE | DCCP.CCID3_LOSS_EVT_RATE
              | DCCP.TIMESTAMP | DCCP.NDP_COUNT =>
               Ada.Text_IO.Put_Line
                 ("Option Type: " & DCCP.Opt_Type'Image (Opt_Type_Field));
               Ada.Text_IO.Put_Line
                 ("Option Length: " &
                  DCCP.Option_Length_Type'Image
                    (DCCP.Option.Get_Option_Length (Option_Element_Context)));

               case Opt_Type_Field is
                  when DCCP.CCID3_RCV_RATE =>
                     Ada.Text_IO.Put_Line
                       ("Receive Rate: " &
                        DCCP.Receive_Rate_Type'Image
                          (DCCP.Option.Get_Receive_Rate
                             (Option_Element_Context)));
                  when DCCP.CCID3_LOSS_EVT_RATE =>
                     Ada.Text_IO.Put_Line
                       ("Loss Event Rate: " &
                        DCCP.Loss_Rate_Type'Image
                          (DCCP.Option.Get_Loss_Event_Rate
                             (Option_Element_Context)));
                  when DCCP.TIMESTAMP =>
                     Ada.Text_IO.Put_Line
                       ("Timestamp: " &
                        DCCP.Timestamp_Option_Type'Image
                          (DCCP.Option.Get_Timestamp_Option
                             (Option_Element_Context)));
                  when DCCP.NDP_COUNT =>
                     declare
                        Length : constant RFLX_Types.Bit_Length :=
                          DCCP.Option.Field_Size
                            (Option_Element_Context,
                             DCCP.Option.F_NDP_Count_Opt);
                        NDP_Option_Count : RFLX_Types.Bytes :=
                          (1 .. RFLX_Types.To_Index (Length) => 0);
                     begin
                        DCCP.Option.Get_NDP_Count_Opt
                          (Option_Element_Context, NDP_Option_Count);
                        Ada.Text_IO.Put_Line ("NDP Count: ");
                        Print_Data_Block (NDP_Option_Count);
                     end;
                  when others =>
                     Ada.Text_IO.Put_Line ("Unknown Option");
               end case;

            when others =>
               Ada.Text_IO.Put_Line ("Unknown Option");
         end case;

         DCCP.Options.Update
           (Options_Sequence_Context, Option_Element_Context);

      end loop;

      DCCP.Packet.Update_Options (Ctx, Options_Sequence_Context);

   end Print_Options;

   --  ****************************************************
   --  Read and print out the DCCP REQUEST Message Contents
   --  Note that the fields can be read in ANY order
   --  ****************************************************
   procedure DCCP_REQUEST (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP REQUEST READ");

      Print_Generic_Header (Ctx);
      Print_Sequence_Number_Long (Ctx);

      Ada.Text_IO.Put_Line
        ("Service Code: " &
         DCCP.Service_Code_Type'Image (DCCP.Packet.Get_Service_Code (Ctx)));

      Ada.Text_IO.Put_Line ("END DCCP REQUEST READ");

      Ada.Text_IO
        .New_Line; --  print a blank line to for an easy visual indication of message end
   end DCCP_REQUEST;

   --  ************************************************
   --  Read and print out the DCCP ACK Message Contents
   --  Note that the fields can be read in ANY order
   --  ************************************************
   procedure DCCP_ACK (Ctx : in out DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP ACK READ");

      Print_Generic_Header (Ctx);
      if DCCP.Packet.Get_X (Ctx) = DCCP.EXTENDED then
         Print_Sequence_Number_Long (Ctx);
         Print_Ack_Number_Long (Ctx);
      end if;
      if DCCP.Packet.Valid (Ctx, DCCP.Packet.F_Options) then
         Print_Options (Ctx);
      end if;

      Ada.Text_IO.Put_Line ("END DCCP ACK READ");

      Ada.Text_IO
        .New_Line; --  print a blank line to for an easy visual indication of message end
   end DCCP_ACK;

   --  *****************************************************
   --  Read and print out the DCCP DATA ACK Message Contents
   --  Note that the fields can be read in ANY order
   --  *****************************************************
   procedure DCCP_DATA_ACK (Ctx : in out DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP DATA ACK READ");

      Print_Generic_Header (Ctx);
      if DCCP.Packet.Get_X (Ctx) = DCCP.EXTENDED then
         Print_Sequence_Number_Long (Ctx);
         Print_Ack_Number_Long (Ctx);
      end if;
      if DCCP.Packet.Valid (Ctx, DCCP.Packet.F_Options) then
         Print_Options (Ctx);
      end if;

      if DCCP.Packet.Present (Ctx, DCCP.Packet.F_Data) then
         Ada.Text_IO.Put_Line ("Message Data:");
         Print_Data (Ctx);
      end if;
      Ada.Text_IO.Put_Line ("END DCCP DATA ACK READ");
      Ada.Text_IO.New_Line;
   end DCCP_DATA_ACK;

   --  *************************************************
   --  Read and print out the DCCP DATA Message Contents
   --  Note that the fields can be read in ANY order
   --  *************************************************
   procedure DCCP_DATA (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP DATA READ");

      Print_Generic_Header (Ctx);

      if DCCP.Packet.Get_X (Ctx) = DCCP.EXTENDED then
         Print_Sequence_Number_Long (Ctx);
      end if;

      if DCCP.Packet.Present (Ctx, DCCP.Packet.F_Data) then
         Ada.Text_IO.Put_Line ("Message Data:");
         Print_Data (Ctx);
      end if;
      Ada.Text_IO.Put_Line ("END DCCP DATA READ");
      Ada.Text_IO.New_Line;
   end DCCP_DATA;

   --  *****************************************************
   --  Read and print out the DCCP DATA ACK Message Contents
   --  Note that the fields can be read in ANY order
   --  *****************************************************
   procedure DCCP_CLOSE (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP CLOSE READ");

      Print_Generic_Header (Ctx);

      if DCCP.Packet.Get_X (Ctx) = DCCP.EXTENDED then
         Print_Sequence_Number_Long (Ctx);
         Print_Ack_Number_Long (Ctx);
      end if;

      Ada.Text_IO.Put_Line ("END DCCP CLOSE READ");

      Ada.Text_IO
        .New_Line; --  print a blank line to for an easy visual indication of message end
   end DCCP_CLOSE;

   --  *****************************************************
   --  Read and print out the DCCP DATA ACK Message Contents
   --  Note that the fields can be read in ANY order
   --  *****************************************************
   procedure DCCP_RESET (Ctx : in out DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP RESET READ");

      Print_Generic_Header (Ctx);
      Print_Sequence_Number_Long (Ctx);
      Print_Ack_Number_Long (Ctx);

      Ada.Text_IO.Put_Line
        ("Reset Code: " &
         DCCP.Reset_Code_Type'Image (DCCP.Packet.Get_Reset_Code (Ctx)));
      Ada.Text_IO.Put_Line
        ("Data 1:" & DCCP.Data_Type'Image (DCCP.Packet.Get_Data_1 (Ctx)));
      Ada.Text_IO.Put_Line
        ("Data 2:" & DCCP.Data_Type'Image (DCCP.Packet.Get_Data_2 (Ctx)));
      Ada.Text_IO.Put_Line
        ("Data 3:" & DCCP.Data_Type'Image (DCCP.Packet.Get_Data_3 (Ctx)));

      if DCCP.Packet.Valid (Ctx, DCCP.Packet.F_Options) then
         Print_Options (Ctx);
      end if;

      Ada.Text_IO.Put_Line ("END DCCP RESET READ");

      Ada.Text_IO
        .New_Line; --  print a blank line to for an easy visual indication of message end
   end DCCP_RESET;

   --  *****************************************************
   --  Read and print out the DCCP RESPONSE Message Contents
   --  Note that the fields can be read in ANY order
   --  *****************************************************
   procedure DCCP_RESPONSE (Ctx : DCCP.Packet.Context) is
   begin
      Ada.Text_IO.Put_Line ("START DCCP RESPONSE READ");

      Print_Generic_Header (Ctx);
      Print_Sequence_Number_Long (Ctx);
      Print_Ack_Number_Long (Ctx);

      Ada.Text_IO.Put_Line
        ("Service Code: " &
         DCCP.Service_Code_Type'Image (DCCP.Packet.Get_Service_Code (Ctx)));

      Ada.Text_IO.Put_Line ("END DCCP RESPONSE READ");

      Ada.Text_IO
        .New_Line; --  print a blank line to for an easy visual indication of message end
   end DCCP_RESPONSE;

end Msg_Read;
