pragma SPARK_Mode;

with Ada.Text_IO;
with GNAT.Sockets;

with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.DHCP_Client.Session.FSM;

with Channel;

procedure DHCP_Client is
   package FSM renames RFLX.DHCP_Client.Session.FSM;
   package Types renames RFLX.RFLX_Types;

   procedure Read (Ctx : FSM.Context; Skt : in out GNAT.Sockets.Socket_Type) with
      Pre =>
         FSM.Initialized (Ctx)
         and then FSM.Has_Data (Ctx, FSM.C_Channel),
      Post =>
         FSM.Initialized (Ctx)
   is
      use type Types.Index;
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095)
         := (others => 0);
      Size : constant Types.Length := FSM.Read_Buffer_Size (Ctx, FSM.C_Channel);
   begin
      if Size = 0 then
         Ada.Text_IO.Put_Line ("Error: read buffer size is 0");
         return;
      end if;
      if Buffer'Length < Size then
         Ada.Text_IO.Put_Line ("Error: buffer too small");
         return;
      end if;
      FSM.Read
         (Ctx,
          FSM.C_Channel,
          Buffer (Buffer'First .. Buffer'First - 2 + Types.Index (Size + 1)));
      Channel.Send
         (Skt,
          Buffer (Buffer'First .. Buffer'First - 2 + Types.Index (Size + 1)));
   end Read;

   procedure Write (Ctx : in out FSM.Context; Skt : in out GNAT.Sockets.Socket_Type) with
      Pre =>
         FSM.Initialized (Ctx)
         and then FSM.Needs_Data (Ctx, FSM.C_Channel),
      Post =>
         FSM.Initialized (Ctx)
   is
      use type Types.Index;
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095);
      Length : RFLX.RFLX_Builtin_Types.Length;
   begin
      Channel.Receive (Skt, Buffer, Length);
      if
         Length > 0
         and Length <= FSM.Write_Buffer_Size (Ctx, FSM.C_Channel)
      then
         FSM.Write
            (Ctx,
             FSM.C_Channel,
             Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
      end if;
   end Write;

   Skt : GNAT.Sockets.Socket_Type;
   Ctx : FSM.Context;
begin
   Channel.Initialize (Skt);
   FSM.Initialize (Ctx);
   while FSM.Active (Ctx) loop
      pragma Loop_Invariant (FSM.Initialized (Ctx));
      for C in FSM.Channel'Range loop
         pragma Loop_Invariant (FSM.Initialized (Ctx));
         if FSM.Has_Data (Ctx, C) then
            Read (Ctx, Skt);
         end if;
         if FSM.Needs_Data (Ctx, C) then
            Write (Ctx, Skt);
         end if;
      end loop;
      FSM.Run (Ctx);
   end loop;
   pragma Warnings (Off, "statement has no effect");
   pragma Warnings (Off, """Ctx"" is set by ""Finalize"" but not used after the call");
   FSM.Finalize (Ctx);
   pragma Warnings (On, "statement has no effect");
   pragma Warnings (On, """Ctx"" is set by ""Finalize"" but not used after the call");
end DHCP_Client;
