pragma SPARK_Mode;

with Ada.Text_IO;

with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.DHCP_Client.Session;

with Channel;
with Session;

procedure DHCP_Client is
   package DHCP_Client_Session renames RFLX.DHCP_Client.Session;
   package Types renames RFLX.RFLX_Types;

   procedure Read (Ctx : in out Session.Context) with
      Pre =>
         DHCP_Client_Session.Initialized (Ctx)
         and then DHCP_Client_Session.Has_Data (Ctx, DHCP_Client_Session.C_Channel),
      Post =>
         DHCP_Client_Session.Initialized (Ctx)
   is
      use type Types.Index;
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095)
         := (others => 0);
      Size : constant Types.Length := DHCP_Client_Session.Read_Buffer_Size (Ctx, DHCP_Client_Session.C_Channel);
   begin
      if Size = 0 then
         Ada.Text_IO.Put_Line ("Error: read buffer size is 0");
         return;
      end if;
      if Buffer'Length < Size then
         Ada.Text_IO.Put_Line ("Error: buffer too small");
         return;
      end if;
      DHCP_Client_Session.Read
         (Ctx,
          DHCP_Client_Session.C_Channel,
          Buffer (Buffer'First .. Buffer'First - 2 + Types.Index (Size + 1)));
      Channel.Send
         (Ctx.Socket,
          Buffer (Buffer'First .. Buffer'First - 2 + Types.Index (Size + 1)));
   end Read;

   procedure Write (Ctx : in out Session.Context) with
      Pre =>
         DHCP_Client_Session.Initialized (Ctx)
         and then DHCP_Client_Session.Needs_Data (Ctx, DHCP_Client_Session.C_Channel),
      Post =>
         DHCP_Client_Session.Initialized (Ctx)
   is
      use type Types.Index;
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095);
      Length : RFLX.RFLX_Builtin_Types.Length;
   begin
      Channel.Receive (Ctx.Socket, Buffer, Length);
      if
         Length > 0
         and Length <= DHCP_Client_Session.Write_Buffer_Size (Ctx, DHCP_Client_Session.C_Channel)
      then
         DHCP_Client_Session.Write
            (Ctx,
             DHCP_Client_Session.C_Channel,
             Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
      end if;
   end Write;

   Ctx : Session.Context;
begin
   Channel.Initialize (Ctx.Socket);
   DHCP_Client_Session.Initialize (Ctx);
   while DHCP_Client_Session.Active (Ctx) loop
      pragma Loop_Invariant (DHCP_Client_Session.Initialized (Ctx));
      for C in DHCP_Client_Session.Channel'Range loop
         pragma Loop_Invariant (DHCP_Client_Session.Initialized (Ctx));
         if DHCP_Client_Session.Has_Data (Ctx, C) then
            Read (Ctx);
         end if;
         if DHCP_Client_Session.Needs_Data (Ctx, C) then
            Write (Ctx);
         end if;
      end loop;
      DHCP_Client_Session.Run (Ctx);
   end loop;
end DHCP_Client;
