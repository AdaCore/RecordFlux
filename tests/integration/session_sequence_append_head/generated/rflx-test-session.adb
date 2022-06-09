pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Bit_Length;

   use type RFLX.TLV.Tag;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Message_Tag : TLV.Tag;
      Tag : TLV.Tag;
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_4 /= null
         and Ctx.P.Slots.Slot_Ptr_5 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
      RFLX_Exception : Boolean := False;
   begin
      pragma Assert (Start_Invariant);
      --  tests/integration/session_sequence_append_head/test.rflx:20:10
      if
         not TLV.Messages.Has_Element (Ctx.P.Messages_Ctx)
         or TLV.Messages.Available_Space (Ctx.P.Messages_Ctx) < 32
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Messages_Ctx : TLV.Message.Context;
      begin
         TLV.Messages.Switch (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
         if TLV.Message.Valid_Next (RFLX_Element_Messages_Ctx, TLV.Message.F_Tag) then
            if TLV.Message.Available_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Tag) >= TLV.Message.Field_Size (RFLX_Element_Messages_Ctx, TLV.Message.F_Tag) then
               TLV.Message.Set_Tag (RFLX_Element_Messages_Ctx, TLV.Msg_Data);
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         if TLV.Message.Valid_Next (RFLX_Element_Messages_Ctx, TLV.Message.F_Length) then
            if TLV.Message.Available_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Length) >= TLV.Message.Field_Size (RFLX_Element_Messages_Ctx, TLV.Message.F_Length) then
               TLV.Message.Set_Length (RFLX_Element_Messages_Ctx, 1);
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         if TLV.Message.Valid_Next (RFLX_Element_Messages_Ctx, TLV.Message.F_Value) then
            if TLV.Message.Available_Space (RFLX_Element_Messages_Ctx, TLV.Message.F_Value) >= TLV.Message.Field_Size (RFLX_Element_Messages_Ctx, TLV.Message.F_Value) then
               if TLV.Message.Valid_Length (RFLX_Element_Messages_Ctx, TLV.Message.F_Value, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
                  TLV.Message.Set_Value (RFLX_Element_Messages_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
               else
                  RFLX_Exception := True;
               end if;
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
         TLV.Messages.Update (Ctx.P.Messages_Ctx, RFLX_Element_Messages_Ctx);
         pragma Warnings (On, """RFLX_Element_Messages_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      --  tests/integration/session_sequence_append_head/test.rflx:21:10
      if
         not TLV.Tags.Has_Element (Ctx.P.Tags_Ctx)
         or TLV.Tags.Available_Space (Ctx.P.Tags_Ctx) < TLV.Tag'Size
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      TLV.Tags.Append_Element (Ctx.P.Tags_Ctx, TLV.Msg_Error);
      --  tests/integration/session_sequence_append_head/test.rflx:22:10
      if TLV.Messages.Valid (Ctx.P.Messages_Ctx) then
         declare
            RFLX_Copy_Messages_Ctx : TLV.Messages.Context;
            RFLX_Copy_Messages_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Messages_Buffer := Ctx.P.Slots.Slot_Ptr_4;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_4 := null;
            pragma Warnings (On, "unused assignment");
            if TLV.Messages.Byte_Size (Ctx.P.Messages_Ctx) <= RFLX_Copy_Messages_Buffer'Length then
               TLV.Messages.Copy (Ctx.P.Messages_Ctx, RFLX_Copy_Messages_Buffer.all (RFLX_Copy_Messages_Buffer'First .. RFLX_Copy_Messages_Buffer'First + RFLX_Types.Index (TLV.Messages.Byte_Size (Ctx.P.Messages_Ctx) + 1) - 2));
            else
               RFLX_Exception := True;
            end if;
            TLV.Messages.Initialize (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Messages_Buffer'First), TLV.Messages.Sequence_Last (Ctx.P.Messages_Ctx));
            if TLV.Messages.Has_Element (RFLX_Copy_Messages_Ctx) then
               declare
                  RFLX_Head_Ctx : TLV.Message.Context;
                  RFLX_Target_Message_Buffer : RFLX.RFLX_Types.Bytes_Ptr;
               begin
                  TLV.Messages.Switch (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                  TLV.Message.Verify_Message (RFLX_Head_Ctx);
                  if TLV.Message.Structural_Valid_Message (RFLX_Head_Ctx) then
                     pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     TLV.Message.Take_Buffer (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer);
                     pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                     if TLV.Message.Byte_Size (RFLX_Head_Ctx) <= RFLX_Target_Message_Buffer'Length then
                        TLV.Message.Copy (RFLX_Head_Ctx, RFLX_Target_Message_Buffer.all (RFLX_Target_Message_Buffer'First .. RFLX_Target_Message_Buffer'First + RFLX_Types.Index (TLV.Message.Byte_Size (RFLX_Head_Ctx) + 1) - 2));
                     else
                        RFLX_Exception := True;
                     end if;
                     TLV.Message.Initialize (Ctx.P.Message_Ctx, RFLX_Target_Message_Buffer, TLV.Message.Size (RFLX_Head_Ctx));
                     TLV.Message.Verify_Message (Ctx.P.Message_Ctx);
                  else
                     RFLX_Exception := True;
                  end if;
                  pragma Warnings (Off, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
                  TLV.Messages.Update (RFLX_Copy_Messages_Ctx, RFLX_Head_Ctx);
                  pragma Warnings (On, """RFLX_Head_Ctx"" is set by ""Update"" but not used after the call");
               end;
            else
               RFLX_Exception := True;
            end if;
            pragma Warnings (Off, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            TLV.Messages.Take_Buffer (RFLX_Copy_Messages_Ctx, RFLX_Copy_Messages_Buffer);
            pragma Warnings (On, """RFLX_Copy_Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Messages_Buffer;
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      --  tests/integration/session_sequence_append_head/test.rflx:23:10
      if TLV.Message.Valid (Ctx.P.Message_Ctx, TLV.Message.F_Tag) then
         Message_Tag := TLV.Message.Get_Tag (Ctx.P.Message_Ctx);
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      --  tests/integration/session_sequence_append_head/test.rflx:24:10
      if
         TLV.Tags.Valid (Ctx.P.Tags_Ctx)
         and then TLV.Tags.Has_Element (Ctx.P.Tags_Ctx)
         and then TLV.Tags.Size (Ctx.P.Tags_Ctx) >= TLV.Tag'Size
      then
         Tag := TLV.Tags.Head (Ctx.P.Tags_Ctx);
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      if
         Message_Tag = TLV.Msg_Data
         and then Tag = TLV.Msg_Error
      then
         Ctx.P.Next_State := S_Reply;
      else
         Ctx.P.Next_State := S_Terminated;
      end if;
      pragma Assert (Start_Invariant);
      <<Finalize_Start>>
   end Start;

   procedure Reply (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_4 /= null
         and Ctx.P.Slots.Slot_Ptr_5 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_Invariant);
      --  tests/integration/session_sequence_append_head/test.rflx:36:10
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Reply_Invariant);
   end Reply;

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
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Messages_Buffer : RFLX_Types.Bytes_Ptr;
      Tags_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Messages.Take_Buffer (Ctx.P.Messages_Ctx, Messages_Buffer);
      pragma Warnings (On, """Ctx.P.Messages_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_1 := Messages_Buffer;
      pragma Warnings (Off, """Ctx.P.Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Tags.Take_Buffer (Ctx.P.Tags_Ctx, Tags_Buffer);
      pragma Warnings (On, """Ctx.P.Tags_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_2 := Tags_Buffer;
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      TLV.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_3 := Message_Buffer;
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Terminated;
   end Finalize;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Reply =>
            Reply (Ctx);
         when S_Terminated =>
            null;
      end case;
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Reply);

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
               when S_Reply =>
                  TLV_Message_Read (Ctx.P.Message_Ctx);
               when others =>
                  null;
            end case;
      end case;
   end Read;

end RFLX.Test.Session;
