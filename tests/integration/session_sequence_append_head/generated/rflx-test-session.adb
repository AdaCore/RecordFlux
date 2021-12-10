pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.TLV;
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;
use type RFLX.TLV.Tag;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
      Message_Tag : TLV.Tag;
      Tag : TLV.Tag;
      RFLX_Exception : Boolean := False;
   begin
      if
         not TLV.Messages.Has_Element (Messages_Ctx)
         or TLV.Messages.Available_Space (Messages_Ctx) < 32
      then
         P_Next_State := S_Terminated;
         return;
      end if;
      declare
         RFLX_Element_Messages_Ctx : TLV.Message.Context;
      begin
         TLV.Messages.Switch (Messages_Ctx, RFLX_Element_Messages_Ctx);
         TLV.Message.Set_Tag (RFLX_Element_Messages_Ctx, TLV.Msg_Data);
         TLV.Message.Set_Length (RFLX_Element_Messages_Ctx, 1);
         if TLV.Message.Valid_Length (RFLX_Element_Messages_Ctx, TLV.Message.F_Value, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
            TLV.Message.Set_Value (RFLX_Element_Messages_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, "unused assignment to ""RFLX_Element_Messages_Ctx""");
         pragma Warnings (Off, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
         TLV.Messages.Update (Messages_Ctx, RFLX_Element_Messages_Ctx);
         pragma Warnings (On, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""RFLX_Element_Messages_Ctx""");
      end;
      if RFLX_Exception then
         P_Next_State := S_Terminated;
         return;
      end if;
      if
         not TLV.Tags.Has_Element (Tags_Ctx)
         or TLV.Tags.Available_Space (Tags_Ctx) < TLV.Tag'Size
      then
         P_Next_State := S_Terminated;
         return;
      end if;
      TLV.Tags.Append_Element (Tags_Ctx, TLV.Msg_Error);
      if TLV.Messages.Valid (Messages_Ctx) then
         declare
            RFLX_Copy_Messages_Ctx : TLV.Messages.Context;
            RFLX_Copy_Messages_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Messages_Buffer := Test.Session_Allocator.Slot_Ptr_4;
            pragma Warnings (Off, "unused assignment");
            Test.Session_Allocator.Slot_Ptr_4 := null;
            pragma Warnings (On, "unused assignment");
            if TLV.Messages.Byte_Size (Messages_Ctx) <= RFLX_Copy_Messages_Buffer'Length then
               TLV.Messages.Copy (Messages_Ctx, RFLX_Copy_Messages_Buffer.all (RFLX_Copy_Messages_Buffer'First .. RFLX_Copy_Messages_Buffer'First + RFLX_Types.Index (TLV.Messages.Byte_Size (Messages_Ctx) + 1) - 2));
            else
               RFLX_Exception := True;
            end if;
            TLV.Messages.Initialize (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Messages_Buffer'First), TLV.Messages.Sequence_Last (Messages_Ctx));
            if TLV.Messages.Has_Element (RFLX_Copy_Messages_Ctx) then
               declare
                  RFLX_Head_Ctx : TLV.Message.Context;
                  RFLX_Target_Message_Buffer : RFLX.RFLX_Types.Bytes_Ptr;
               begin
                  TLV.Messages.Switch (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                  TLV.Message.Verify_Message (RFLX_Head_Ctx);
                  if TLV.Message.Structural_Valid_Message (RFLX_Head_Ctx) then
                     pragma Warnings (Off, "unused assignment to ""Message_Ctx""");
                     pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Message.Take_Buffer (Message_Ctx, RFLX_Target_Message_Buffer);
                     pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     pragma Warnings (On, "unused assignment to ""Message_Ctx""");
                     if TLV.Message.Byte_Size (RFLX_Head_Ctx) <= RFLX_Target_Message_Buffer'Length then
                        TLV.Message.Copy (RFLX_Head_Ctx, RFLX_Target_Message_Buffer.all (RFLX_Target_Message_Buffer'First .. RFLX_Target_Message_Buffer'First + RFLX_Types.Index (TLV.Message.Byte_Size (RFLX_Head_Ctx) + 1) - 2));
                     else
                        RFLX_Exception := True;
                     end if;
                     TLV.Message.Initialize (Message_Ctx, RFLX_Target_Message_Buffer, TLV.Message.Size (RFLX_Head_Ctx));
                     TLV.Message.Verify_Message (Message_Ctx);
                  else
                     RFLX_Exception := True;
                  end if;
                  pragma Warnings (Off, "unused assignment to ""RFLX_Head_Ctx""");
                  pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                  TLV.Messages.Update (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                  pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                  pragma Warnings (On, "unused assignment to ""RFLX_Head_Ctx""");
               end;
            else
               RFLX_Exception := True;
            end if;
            pragma Warnings (Off, "unused assignment to ""RFLX_Copy_Messages_Ctx""");
            pragma Warnings (Off, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            TLV.Messages.Take_Buffer (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer);
            pragma Warnings (On, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Warnings (On, "unused assignment to ""RFLX_Copy_Messages_Ctx""");
            pragma Warnings (Off, "unused assignment");
            Test.Session_Allocator.Slot_Ptr_4 := RFLX_Copy_Messages_Buffer;
            pragma Warnings (On, "unused assignment");
         end;
      else
         P_Next_State := S_Terminated;
         return;
      end if;
      if RFLX_Exception then
         P_Next_State := S_Terminated;
         return;
      end if;
      if RFLX_Exception then
         P_Next_State := S_Terminated;
         return;
      end if;
      if TLV.Message.Valid (Message_Ctx, TLV.Message.F_Tag) then
         Message_Tag := TLV.Message.Get_Tag (Message_Ctx);
      else
         P_Next_State := S_Terminated;
         return;
      end if;
      if
         TLV.Tags.Valid (Tags_Ctx)
         and then TLV.Tags.Has_Element (Tags_Ctx)
         and then TLV.Tags.Size (Tags_Ctx) >= TLV.Tag'Size
      then
         Tag := TLV.Tags.Head (Tags_Ctx);
      else
         P_Next_State := S_Terminated;
         return;
      end if;
      if
         Message_Tag = TLV.Msg_Data
         and then Tag = TLV.Msg_Error
      then
         P_Next_State := S_Reply;
      else
         P_Next_State := S_Terminated;
      end if;
   end Start;

   procedure Reply (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      P_Next_State := S_Terminated;
   end Reply;

   procedure Initialize is
      Messages_Buffer : RFLX_Types.Bytes_Ptr;
      Tags_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      Messages_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Messages.Initialize (Messages_Ctx, Messages_Buffer);
      Tags_Buffer := Test.Session_Allocator.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Tags.Initialize (Tags_Ctx, Tags_Buffer);
      Message_Buffer := Test.Session_Allocator.Slot_Ptr_3;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_3 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Message.Initialize (Message_Ctx, Message_Buffer);
      P_Next_State := S_Start;
   end Initialize;

   procedure Finalize is
      Messages_Buffer : RFLX_Types.Bytes_Ptr;
      Tags_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""Messages_Ctx""");
      pragma Warnings (Off, """Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Messages.Take_Buffer (Messages_Ctx, Messages_Buffer);
      pragma Warnings (On, """Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Messages_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := Messages_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""Tags_Ctx""");
      pragma Warnings (Off, """Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Tags.Take_Buffer (Tags_Ctx, Tags_Buffer);
      pragma Warnings (On, """Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Tags_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := Tags_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Message.Take_Buffer (Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_3 := Message_Buffer;
      pragma Warnings (On, "unused assignment");
      P_Next_State := S_Terminated;
   end Finalize;

   procedure Tick is
   begin
      case P_Next_State is
         when S_Start =>
            Start (P_Next_State);
         when S_Reply =>
            Reply (P_Next_State);
         when S_Terminated =>
            null;
      end case;
   end Tick;

   function In_IO_State return Boolean is
     (P_Next_State in S_Reply);

   procedure Run is
   begin
      Tick;
      while
         Active
         and not In_IO_State
      loop
         pragma Loop_Invariant (Initialized);
         Tick;
      end loop;
   end Run;

   function Has_Data (Chan : Channel) return Boolean is
     ((case Chan is
          when C_Channel =>
             (case P_Next_State is
                 when S_Reply =>
                    TLV.Message.Structural_Valid_Message (Message_Ctx)
                    and TLV.Message.Byte_Size (Message_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             (case P_Next_State is
                 when S_Reply =>
                    TLV.Message.Byte_Size (Message_Ctx),
                 when others =>
                    raise Program_Error)));

   procedure Read (Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      function Read_Pre (Message_Buffer : RFLX_Types.Bytes) return Boolean is
        (Buffer'Length > 0
         and then Offset < Message_Buffer'Length);
      procedure Read (Message_Buffer : RFLX_Types.Bytes) with
        Pre =>
          Read_Pre (Message_Buffer)
      is
         Length : constant RFLX_Types.Index := RFLX_Types.Index (RFLX_Types.Length'Min (Buffer'Length, Message_Buffer'Length - Offset));
         Buffer_Last : constant RFLX_Types.Index := Buffer'First - 1 + Length;
      begin
         Buffer (Buffer'First .. RFLX_Types.Index (Buffer_Last)) := Message_Buffer (RFLX_Types.Index (RFLX_Types.Length (Message_Buffer'First) + Offset) .. Message_Buffer'First - 2 + RFLX_Types.Index (Offset + 1) + Length);
      end Read;
      procedure TLV_Message_Read is new TLV.Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case P_Next_State is
               when S_Reply =>
                  TLV_Message_Read (Message_Ctx);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Read;

end RFLX.Test.Session;
