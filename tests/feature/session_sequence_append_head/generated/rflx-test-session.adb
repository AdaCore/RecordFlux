pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Bit_Length;

   use type RFLX.TLV.Tag;

   procedure Global (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Message_Tag : TLV.Tag;
      Tag : TLV.Tag;
      function Global_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_4 /= null
         and Ctx.P.Slots.Slot_Ptr_5 /= null
         and Ctx.P.Slots.Slot_Ptr_6 /= null
         and Ctx.P.Slots.Slot_Ptr_7 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Global_Invariant);
      -- tests/feature/session_sequence_append_head/test.rflx:17:10
      if
         not TLV.Messages.Has_Element (Ctx.P.Messages_Ctx)
         or TLV.Messages.Available_Space (Ctx.P.Messages_Ctx) < 32
      then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Global_Invariant);
         goto Finalize_Global;
      end if;
      declare
         RFLX_Element_Messages_Ctx : TLV.Message.Context;
      begin
         TLV.Messages.Switch (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
         pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Tag));
         TLV.Message.Set_Tag (RFLX_Element_Messages_Ctx, TLV.Msg_Data);
         pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Length));
         TLV.Message.Set_Length (RFLX_Element_Messages_Ctx, 1);
         if TLV.Message.Valid_Length (RFLX_Element_Messages_Ctx, TLV.Message.F_Value, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
            pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Value));
            TLV.Message.Set_Value (RFLX_Element_Messages_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
         else
            Ctx.P.Next_State := S_Final;
            pragma Warnings (Off, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
            TLV.Messages.Update (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
            pragma Warnings (On, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Global_Invariant);
            goto Finalize_Global;
         end if;
         pragma Warnings (Off, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
         TLV.Messages.Update (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
         pragma Warnings (On, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
      end;
      -- tests/feature/session_sequence_append_head/test.rflx:18:10
      if
         not TLV.Tags.Has_Element (Ctx.P.Tags_Ctx)
         or TLV.Tags.Available_Space (Ctx.P.Tags_Ctx) < TLV.Tag'Size
      then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Global_Invariant);
         goto Finalize_Global;
      end if;
      TLV.Tags.Append_Element (Ctx.P.Tags_Ctx, TLV.Msg_Error);
      -- tests/feature/session_sequence_append_head/test.rflx:19:10
      if TLV.Messages.Valid (Ctx.P.Messages_Ctx) then
         declare
            RFLX_Copy_Messages_Ctx : TLV.Messages.Context;
            RFLX_Copy_Messages_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Messages_Buffer := Ctx.P.Slots.Slot_Ptr_4;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_4 := null;
            pragma Warnings (On, "unused assignment");
            TLV.Messages.Copy (Ctx.P.Messages_Ctx, RFLX_Copy_Messages_Buffer.all (RFLX_Copy_Messages_Buffer'First .. RFLX_Copy_Messages_Buffer'First + RFLX_Types.Index (TLV.Messages.Byte_Size (Ctx.P.Messages_Ctx) + 1) - 2));
            TLV.Messages.Initialize (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Messages_Buffer'First), TLV.Messages.Sequence_Last (Ctx.P.Messages_Ctx));
            if TLV.Messages.Has_Element (RFLX_Copy_Messages_Ctx) then
               declare
                  RFLX_Head_Ctx : TLV.Message.Context;
                  RFLX_Target_Message_Buffer : RFLX.RFLX_Types.Bytes_Ptr;
               begin
                  TLV.Messages.Switch (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                  TLV.Message.Verify_Message (RFLX_Head_Ctx);
                  if TLV.Message.Well_Formed_Message (RFLX_Head_Ctx) then
                     pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Message.Take_Buffer (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer);
                     pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     if TLV.Message.Byte_Size (RFLX_Head_Ctx) <= RFLX_Target_Message_Buffer'Length then
                        TLV.Message.Copy (RFLX_Head_Ctx, RFLX_Target_Message_Buffer.all (RFLX_Target_Message_Buffer'First .. RFLX_Target_Message_Buffer'First + RFLX_Types.Index (TLV.Message.Byte_Size (RFLX_Head_Ctx) + 1) - 2));
                     else
                        Ctx.P.Next_State := S_Final;
                        TLV.Message.Initialize (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer);
                        pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                        TLV.Messages.Update (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                        pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                        pragma Warnings (Off, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        TLV.Messages.Take_Buffer (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer);
                        pragma Warnings (On, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                        pragma Assert (RFLX_Copy_Messages_Buffer /= null);
                        Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Messages_Buffer;
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                        pragma Assert (Global_Invariant);
                        goto Finalize_Global;
                     end if;
                     TLV.Message.Initialize (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer, TLV.Message.Size (RFLX_Head_Ctx));
                     TLV.Message.Verify_Message (Ctx.P.Message_Ctx);
                  else
                     Ctx.P.Next_State := S_Final;
                     pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                     TLV.Messages.Update (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                     pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                     pragma Warnings (Off, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Messages.Take_Buffer (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer);
                     pragma Warnings (On, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                     pragma Assert (RFLX_Copy_Messages_Buffer /= null);
                     Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Messages_Buffer;
                     pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                     pragma Assert (Global_Invariant);
                     goto Finalize_Global;
                  end if;
                  pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                  TLV.Messages.Update (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                  pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
               end;
            else
               Ctx.P.Next_State := S_Final;
               pragma Warnings (Off, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               TLV.Messages.Take_Buffer (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer);
               pragma Warnings (On, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
               pragma Assert (RFLX_Copy_Messages_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Messages_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
               pragma Assert (Global_Invariant);
               goto Finalize_Global;
            end if;
            pragma Warnings (Off, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            TLV.Messages.Take_Buffer (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer);
            pragma Warnings (On, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
            pragma Assert (RFLX_Copy_Messages_Buffer /= null);
            Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Messages_Buffer;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
         end;
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Global_Invariant);
         goto Finalize_Global;
      end if;
      -- tests/feature/session_sequence_append_head/test.rflx:20:10
      if TLV.Message.Valid (Ctx.P.Message_Ctx, TLV.Message.F_Tag) then
         Message_Tag := TLV.Message.Get_Tag (Ctx.P.Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Global_Invariant);
         goto Finalize_Global;
      end if;
      -- tests/feature/session_sequence_append_head/test.rflx:21:10
      if
         TLV.Tags.Valid (Ctx.P.Tags_Ctx)
         and then TLV.Tags.Has_Element (Ctx.P.Tags_Ctx)
         and then TLV.Tags.Size (Ctx.P.Tags_Ctx) >= TLV.Tag'Size
      then
         Tag := TLV.Tags.Head (Ctx.P.Tags_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Global_Invariant);
         goto Finalize_Global;
      end if;
      if
         Message_Tag = TLV.Msg_Data
         and then Tag = TLV.Msg_Error
      then
         Ctx.P.Next_State := S_Reply_1;
      else
         Ctx.P.Next_State := S_Final;
      end if;
      pragma Assert (Global_Invariant);
      <<Finalize_Global>>
   end Global;

   procedure Reply_1 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_1_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_4 /= null
         and Ctx.P.Slots.Slot_Ptr_5 /= null
         and Ctx.P.Slots.Slot_Ptr_6 /= null
         and Ctx.P.Slots.Slot_Ptr_7 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_1_Invariant);
      -- tests/feature/session_sequence_append_head/test.rflx:33:10
      Ctx.P.Next_State := S_Local;
      pragma Assert (Reply_1_Invariant);
   end Reply_1;

   procedure Local (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Local_Messages_Ctx : TLV.Messages.Context;
      Local_Tags_Ctx : TLV.Tags.Context;
      Message_Tag : TLV.Tag;
      Tag : TLV.Tag;
      Local_Messages_Buffer : RFLX_Types.Bytes_Ptr;
      Local_Tags_Buffer : RFLX_Types.Bytes_Ptr;
      function Local_Invariant return Boolean is
        (Global_Initialized (Ctx)
         and TLV.Messages.Has_Buffer (Local_Messages_Ctx)
         and Local_Messages_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Local_Messages_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_4 = null
         and Global_Initialized (Ctx)
         and TLV.Tags.Has_Buffer (Local_Tags_Ctx)
         and Local_Tags_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Local_Tags_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_5 = null
         and Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_6 /= null
         and Ctx.P.Slots.Slot_Ptr_7 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      Local_Messages_Buffer := Ctx.P.Slots.Slot_Ptr_4;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_4 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Messages.Initialize (Local_Messages_Ctx, Local_Messages_Buffer);
      Local_Tags_Buffer := Ctx.P.Slots.Slot_Ptr_5;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_5 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Tags.Initialize (Local_Tags_Ctx, Local_Tags_Buffer);
      pragma Assert (Local_Invariant);
      -- tests/feature/session_sequence_append_head/test.rflx:45:10
      if
         not TLV.Messages.Has_Element (Local_Messages_Ctx)
         or TLV.Messages.Available_Space (Local_Messages_Ctx) < 40
      then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      declare
         RFLX_Element_Local_Messages_Ctx : TLV.Message.Context;
      begin
         TLV.Messages.Switch (Local_Messages_Ctx, RFLX_Element_Local_Messages_Ctx);
         pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Local_Messages_Ctx, TLV.Message.F_Tag));
         TLV.Message.Set_Tag (RFLX_Element_Local_Messages_Ctx, TLV.Msg_Data);
         pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Local_Messages_Ctx, TLV.Message.F_Length));
         TLV.Message.Set_Length (RFLX_Element_Local_Messages_Ctx, 2);
         if TLV.Message.Valid_Length (RFLX_Element_Local_Messages_Ctx, TLV.Message.F_Value, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
            pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Local_Messages_Ctx, TLV.Message.F_Value));
            TLV.Message.Set_Value (RFLX_Element_Local_Messages_Ctx, (RFLX_Types.Byte'Val (3), RFLX_Types.Byte'Val (4)));
         else
            Ctx.P.Next_State := S_Final;
            pragma Warnings (Off, """RFLX_Element_Local_Messages_Ctx"" is set by ""Update"" but not used after the call");
            TLV.Messages.Update (Local_Messages_Ctx, RFLX_Element_Local_Messages_Ctx);
            pragma Warnings (On, """RFLX_Element_Local_Messages_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Local_Invariant);
            goto Finalize_Local;
         end if;
         pragma Warnings (Off, """RFLX_Element_Local_Messages_Ctx"" is set by ""Update"" but not used after the call");
         TLV.Messages.Update (Local_Messages_Ctx, RFLX_Element_Local_Messages_Ctx);
         pragma Warnings (On, """RFLX_Element_Local_Messages_Ctx"" is set by ""Update"" but not used after the call");
      end;
      -- tests/feature/session_sequence_append_head/test.rflx:47:10
      if
         not TLV.Messages.Has_Element (Ctx.P.Messages_Ctx)
         or TLV.Messages.Available_Space (Ctx.P.Messages_Ctx) < 32
      then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      declare
         RFLX_Element_Messages_Ctx : TLV.Message.Context;
      begin
         TLV.Messages.Switch (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
         pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Tag));
         TLV.Message.Set_Tag (RFLX_Element_Messages_Ctx, TLV.Msg_Data);
         pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Length));
         TLV.Message.Set_Length (RFLX_Element_Messages_Ctx, 1);
         if TLV.Message.Valid_Length (RFLX_Element_Messages_Ctx, TLV.Message.F_Value, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
            pragma Assert (TLV.Message.Sufficient_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Value));
            TLV.Message.Set_Value (RFLX_Element_Messages_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
         else
            Ctx.P.Next_State := S_Final;
            pragma Warnings (Off, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
            TLV.Messages.Update (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
            pragma Warnings (On, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Local_Invariant);
            goto Finalize_Local;
         end if;
         pragma Warnings (Off, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
         TLV.Messages.Update (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
         pragma Warnings (On, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
      end;
      -- tests/feature/session_sequence_append_head/test.rflx:48:10
      if
         not TLV.Tags.Has_Element (Local_Tags_Ctx)
         or TLV.Tags.Available_Space (Local_Tags_Ctx) < TLV.Tag'Size
      then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      TLV.Tags.Append_Element (Local_Tags_Ctx, TLV.Msg_Data);
      -- tests/feature/session_sequence_append_head/test.rflx:49:10
      if
         not TLV.Tags.Has_Element (Local_Tags_Ctx)
         or TLV.Tags.Available_Space (Local_Tags_Ctx) < TLV.Tag'Size
      then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      TLV.Tags.Append_Element (Local_Tags_Ctx, TLV.Msg_Error);
      -- tests/feature/session_sequence_append_head/test.rflx:50:10
      if TLV.Messages.Valid (Local_Messages_Ctx) then
         declare
            RFLX_Copy_Local_Messages_Ctx : TLV.Messages.Context;
            RFLX_Copy_Local_Messages_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Local_Messages_Buffer := Ctx.P.Slots.Slot_Ptr_6;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_6 := null;
            pragma Warnings (On, "unused assignment");
            TLV.Messages.Copy (Local_Messages_Ctx, RFLX_Copy_Local_Messages_Buffer.all (RFLX_Copy_Local_Messages_Buffer'First .. RFLX_Copy_Local_Messages_Buffer'First + RFLX_Types.Index (TLV.Messages.Byte_Size (Local_Messages_Ctx) + 1) - 2));
            TLV.Messages.Initialize (RFLX_Copy_Local_Messages_Ctx, RFLX_Copy_Local_Messages_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Local_Messages_Buffer'First), TLV.Messages.Sequence_Last (Local_Messages_Ctx));
            if TLV.Messages.Has_Element (RFLX_Copy_Local_Messages_Ctx) then
               declare
                  RFLX_Head_Ctx : TLV.Message.Context;
                  RFLX_Target_Message_Buffer : RFLX.RFLX_Types.Bytes_Ptr;
               begin
                  TLV.Messages.Switch (RFLX_Copy_Local_Messages_Ctx, RFLX_Head_Ctx);
                  TLV.Message.Verify_Message (RFLX_Head_Ctx);
                  if TLV.Message.Well_Formed_Message (RFLX_Head_Ctx) then
                     pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Message.Take_Buffer (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer);
                     pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Message.Copy (RFLX_Head_Ctx, RFLX_Target_Message_Buffer.all (RFLX_Target_Message_Buffer'First .. RFLX_Target_Message_Buffer'First + RFLX_Types.Index (TLV.Message.Byte_Size (RFLX_Head_Ctx) + 1) - 2));
                     TLV.Message.Initialize (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer, TLV.Message.Size (RFLX_Head_Ctx));
                     TLV.Message.Verify_Message (Ctx.P.Message_Ctx);
                  else
                     Ctx.P.Next_State := S_Final;
                     pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                     TLV.Messages.Update (RFLX_Copy_Local_Messages_Ctx, RFLX_Head_Ctx);
                     pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                     pragma Warnings (Off, """RFLX_Copy_Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Messages.Take_Buffer (RFLX_Copy_Local_Messages_Ctx, RFLX_Copy_Local_Messages_Buffer);
                     pragma Warnings (On, """RFLX_Copy_Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     pragma Assert (Ctx.P.Slots.Slot_Ptr_6 = null);
                     pragma Assert (RFLX_Copy_Local_Messages_Buffer /= null);
                     Ctx.P.Slots.Slot_Ptr_6 := RFLX_Copy_Local_Messages_Buffer;
                     pragma Assert (Ctx.P.Slots.Slot_Ptr_6 /= null);
                     pragma Assert (Local_Invariant);
                     goto Finalize_Local;
                  end if;
                  pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                  TLV.Messages.Update (RFLX_Copy_Local_Messages_Ctx, RFLX_Head_Ctx);
                  pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
               end;
            else
               Ctx.P.Next_State := S_Final;
               pragma Warnings (Off, """RFLX_Copy_Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               TLV.Messages.Take_Buffer (RFLX_Copy_Local_Messages_Ctx, RFLX_Copy_Local_Messages_Buffer);
               pragma Warnings (On, """RFLX_Copy_Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               pragma Assert (Ctx.P.Slots.Slot_Ptr_6 = null);
               pragma Assert (RFLX_Copy_Local_Messages_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_6 := RFLX_Copy_Local_Messages_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_6 /= null);
               pragma Assert (Local_Invariant);
               goto Finalize_Local;
            end if;
            pragma Warnings (Off, """RFLX_Copy_Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            TLV.Messages.Take_Buffer (RFLX_Copy_Local_Messages_Ctx, RFLX_Copy_Local_Messages_Buffer);
            pragma Warnings (On, """RFLX_Copy_Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Assert (Ctx.P.Slots.Slot_Ptr_6 = null);
            pragma Assert (RFLX_Copy_Local_Messages_Buffer /= null);
            Ctx.P.Slots.Slot_Ptr_6 := RFLX_Copy_Local_Messages_Buffer;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_6 /= null);
         end;
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      -- tests/feature/session_sequence_append_head/test.rflx:51:10
      if TLV.Message.Valid (Ctx.P.Message_Ctx, TLV.Message.F_Tag) then
         Message_Tag := TLV.Message.Get_Tag (Ctx.P.Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      -- tests/feature/session_sequence_append_head/test.rflx:52:10
      if
         TLV.Tags.Valid (Local_Tags_Ctx)
         and then TLV.Tags.Has_Element (Local_Tags_Ctx)
         and then TLV.Tags.Size (Local_Tags_Ctx) >= TLV.Tag'Size
      then
         Tag := TLV.Tags.Head (Local_Tags_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Local_Invariant);
         goto Finalize_Local;
      end if;
      if
         Message_Tag = TLV.Msg_Data
         and then Tag = TLV.Msg_Data
      then
         Ctx.P.Next_State := S_Reply_2;
      else
         Ctx.P.Next_State := S_Final;
      end if;
      pragma Assert (Local_Invariant);
      <<Finalize_Local>>
      pragma Warnings (Off, """Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Messages.Take_Buffer (Local_Messages_Ctx, Local_Messages_Buffer);
      pragma Warnings (On, """Local_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
      pragma Assert (Local_Messages_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_4 := Local_Messages_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
      pragma Warnings (Off, """Local_Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Tags.Take_Buffer (Local_Tags_Ctx, Local_Tags_Buffer);
      pragma Warnings (On, """Local_Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_5 = null);
      pragma Assert (Local_Tags_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_5 := Local_Tags_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_5 /= null);
      pragma Assert (Global_Initialized (Ctx));
   end Local;

   procedure Reply_2 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_2_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_4 /= null
         and Ctx.P.Slots.Slot_Ptr_5 /= null
         and Ctx.P.Slots.Slot_Ptr_6 /= null
         and Ctx.P.Slots.Slot_Ptr_7 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_2_Invariant);
      -- tests/feature/session_sequence_append_head/test.rflx:64:10
      Ctx.P.Next_State := S_Final;
      pragma Assert (Reply_2_Invariant);
   end Reply_2;

   procedure Initialize (Ctx : in out Context'Class) is
      Messages_Buffer : RFLX_Types.Bytes_Ptr;
      Tags_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Messages_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Messages.Initialize (Ctx.P.Messages_Ctx, Messages_Buffer);
      Tags_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Tags.Initialize (Ctx.P.Tags_Ctx, Tags_Buffer);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_3;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_3 := null;
      pragma Warnings (On, "unused assignment");
      TLV.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Ctx.P.Next_State := S_Global;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Messages_Buffer : RFLX_Types.Bytes_Ptr;
      Tags_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Messages.Take_Buffer (Ctx.P.Messages_Ctx, Messages_Buffer);
      pragma Warnings (On, """Ctx.P.Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Messages_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Messages_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      pragma Warnings (Off, """Ctx.P.Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Tags.Take_Buffer (Ctx.P.Tags_Ctx, Tags_Buffer);
      pragma Warnings (On, """Ctx.P.Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (Tags_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := Tags_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_3 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Final;
   end Finalize;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Global =>
            Global (Ctx);
         when S_Reply_1 =>
            Reply_1 (Ctx);
         when S_Local =>
            Local (Ctx);
         when S_Reply_2 =>
            Reply_2 (Ctx);
         when S_Final =>
            null;
      end case;
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Reply_1 | S_Reply_2);

   procedure Run (Ctx : in out Context'Class) is
   begin
      Tick (Ctx);
      while
         Active (Ctx)
         and not In_IO_State (Ctx)
      loop
         pragma Loop_Invariant (Initialized (Ctx));
         Tick (Ctx);
      end loop;
   end Run;

   procedure Read (Ctx : Context'Class; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
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
            case Ctx.P.Next_State is
               when S_Reply_1 | S_Reply_2 =>
                  TLV_Message_Read (Ctx.P.Message_Ctx);
               when others =>
                  pragma Warnings (Off, "unreachable code");
                  null;
                  pragma Warnings (On, "unreachable code");
            end case;
      end case;
   end Read;

end RFLX.Test.Session;