pragma Warnings
  (Off, """Context"" is set by ""*"" but not used after the call");

with RFLX.Pub_Sub.Message;
with RFLX.RFLX_Types;

with DB;

package body Broker with SPARK_Mode
is
   use type RFLX.RFLX_Types.Bytes_Ptr;
   use type RFLX.RFLX_Types.Index;

   subtype Status is RFLX.Pub_Sub.Command
      range RFLX.Pub_Sub.ERROR .. RFLX.Pub_Sub.SUCCESS;

   procedure Send_Status (Id : RFLX.Pub_Sub.Identifier; St : Status)
      with Pre => Socket.Initialized;

   procedure Send_Status (Id : RFLX.Pub_Sub.Identifier;
                          St : Status)
   is
      Context : RFLX.Pub_Sub.Message.Context;
      Buffer  : RFLX.RFLX_Types.Bytes_Ptr :=
         new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
   begin
      RFLX.Pub_Sub.Message.Initialize (Context, Buffer);
      RFLX.Pub_Sub.Message.Set_Identifier (Context, Id);
      RFLX.Pub_Sub.Message.Set_Command (Context, St);
      RFLX.Pub_Sub.Message.Take_Buffer (Context, Buffer);
      Socket.Send (Buffer.all);
      RFLX.RFLX_Types.Free (Buffer);
   end Send_Status;

   procedure Run is
      Context : RFLX.Pub_Sub.Message.Context;
      Buffer  : RFLX.RFLX_Types.Bytes_Ptr :=
         new RFLX.RFLX_Types.Bytes'(1 .. 4_096 => 0);
      Success : Boolean;
      use type RFLX.Pub_Sub.Length;
   begin
      Socket.Receive (Buffer.all, Success);
      if not Success then
         RFLX.RFLX_Types.Free (Buffer);
         return;
      end if;

      RFLX.Pub_Sub.Message.Initialize
         (Context,
          Buffer,
          RFLX.RFLX_Types.To_Last_Bit_Index (Buffer'Last));

      RFLX.Pub_Sub.Message.Verify_Message (Context);

      if RFLX.Pub_Sub.Message.Well_Formed_Message (Context) then
         declare
            Id  : RFLX.Pub_Sub.Identifier :=
               RFLX.Pub_Sub.Message.Get_Identifier (Context);
            Cmd : RFLX.Pub_Sub.Command :=
               RFLX.Pub_Sub.Message.Get_Command (Context);
         begin
            case Cmd is
               when RFLX.Pub_Sub.SUBSCRIBE =>
                  DB.Subscribe (Id);
                  Send_Status (Id, RFLX.Pub_Sub.SUCCESS);
               when RFLX.Pub_Sub.UNSUBSCRIBE =>
                  if DB.Is_Subscribed (Id) then
                     DB.Unsubscribe (Id);
                     Send_Status
                        (Id,
                         RFLX.Pub_Sub.SUCCESS);
                  else
                     Send_Status
                        (Id,
                         RFLX.Pub_Sub.ERROR_NOT_SUBSCRIBED);
                  end if;
               when RFLX.Pub_Sub.PUBLISH =>
                  if DB.Is_Subscribed (Id) then
                     declare
                        Length : RFLX.Pub_Sub.Length
                           := RFLX.Pub_Sub.Message.Get_Length (Context);
                        Subscribers : DB.Identifiers
                           := DB.Current_Subscribers;
                     begin
                        if Subscribers'Length <= 1 then
                           Send_Status
                              (Id,
                               RFLX.Pub_Sub.ERROR_NO_SUBSCRIBERS);
                        elsif Length > 4_000 then
                           Send_Status
                              (Id,
                               RFLX.Pub_Sub.ERROR_MESSAGE_TOO_LONG);
                        else
                           RFLX.Pub_Sub.Message.Take_Buffer (Context, Buffer);
                           for Subscriber in Subscribers'Range loop
                              Socket.Send (Buffer.all);
                           end loop;
                        end if;
                     end;
                  else
                     Send_Status
                        (Id,
                         RFLX.Pub_Sub.ERROR_NOT_SUBSCRIBED);
                  end if;
               when others =>
                  Send_Status
                     (Id,
                      RFLX.Pub_Sub.ERROR);
            end case;
         end;
      end if;

   if RFLX.Pub_Sub.Message.Has_Buffer (Context) then
      RFLX.Pub_Sub.Message.Take_Buffer (Context, Buffer);
   end if;
   RFLX.RFLX_Types.Free (Buffer);

   end Run;

end Broker;
