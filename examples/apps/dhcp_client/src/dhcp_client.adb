pragma SPARK_Mode;

with Ada.Text_IO;

with GNAT.Sockets;

with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.DHCP_Client.Session;

with Channel;

procedure DHCP_Client is
   Socket : GNAT.Sockets.Socket_Type;

   package Session is new RFLX.DHCP_Client.Session;

   procedure Read with
      Pre =>
         Session.Initialized
         and then Session.Has_Data (Session.C_Channel),
      Post =>
         Session.Initialized
   is
      use type RFLX.RFLX_Types.Index;
      use type RFLX.RFLX_Types.Length;
      Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095)
         := (others => 0);
   begin
      if Buffer'Length >= Session.Read_Buffer_Size (Session.C_Channel) then
         Session.Read
            (Session.C_Channel,
             Buffer
                (Buffer'First
                 .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Session.Read_Buffer_Size (Session.C_Channel) + 1)));
         Channel.Send
            (Socket,
             Buffer
                (Buffer'First
                 .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Session.Read_Buffer_Size (Session.C_Channel) + 1)));
      else
         Ada.Text_IO.Put_Line ("Error: read buffer too small");
      end if;
   end Read;

   procedure Write with
      Pre =>
         Session.Initialized
         and then Session.Needs_Data (Session.C_Channel),
      Post =>
         Session.Initialized
   is
      use type RFLX.RFLX_Types.Index;
      use type RFLX.RFLX_Types.Length;
      Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
      Length : RFLX.RFLX_Builtin_Types.Length;
   begin
      Channel.Receive (Socket, Buffer, Length);
      if
         Length > 0
         and Length <= Session.Write_Buffer_Size (Session.C_Channel)
      then
         Session.Write
            (Session.C_Channel,
             Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
      end if;
   end Write;
begin
   Channel.Initialize (Socket);
   Session.Initialize;
   while Session.Active loop
      pragma Loop_Invariant (Session.Initialized);
      for C in Session.Channel'Range loop
         pragma Loop_Invariant (Session.Initialized);
         if Session.Has_Data (C) then
            Read;
         end if;
         if Session.Needs_Data (C) then
            Write;
         end if;
      end loop;
      Session.Run;
   end loop;
   --  ISSUE: Componolit/Workarounds#32
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   Session.Finalize;
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
end DHCP_Client;
