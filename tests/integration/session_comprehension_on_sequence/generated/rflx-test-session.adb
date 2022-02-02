pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal;
with RFLX.Universal.Option;
with RFLX.Universal.Option_Types;
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;
use type RFLX.Universal.Option_Type;
use type RFLX.Universal.Option_Type_Enum;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      RFLX_Exception : Boolean := False;
   begin
      if
         not Universal.Options.Has_Element (Ctx.P.Options_Ctx)
         or Universal.Options.Available_Space (Ctx.P.Options_Ctx) < 32
      then
         Ctx.P.Next_State := S_Terminated;
         return;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 1);
         if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         return;
      end if;
      if
         not Universal.Options.Has_Element (Ctx.P.Options_Ctx)
         or Universal.Options.Available_Space (Ctx.P.Options_Ctx) < 8
      then
         Ctx.P.Next_State := S_Terminated;
         return;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Null);
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if
         not Universal.Options.Has_Element (Ctx.P.Options_Ctx)
         or Universal.Options.Available_Space (Ctx.P.Options_Ctx) < 40
      then
         Ctx.P.Next_State := S_Terminated;
         return;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 2);
         if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Byte'Val (2), RFLX_Types.Byte'Val (3)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         return;
      end if;
      Ctx.P.Next_State := S_Process;
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Option_Types_Ctx : Universal.Option_Types.Context;
      Message_Options_Ctx : Universal.Options.Context;
      RFLX_Exception : Boolean := False;
      Option_Types_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Options_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Option_Types_Buffer := Ctx.P.Slots.Slot_Ptr_4;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_4 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option_Types.Initialize (Option_Types_Ctx, Option_Types_Buffer);
      Message_Options_Buffer := Ctx.P.Slots.Slot_Ptr_6;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_6 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Message_Options_Ctx, Message_Options_Buffer);
      if Universal.Options.Valid (Ctx.P.Options_Ctx) then
         declare
            RFLX_Copy_Options_Ctx : Universal.Options.Context;
            RFLX_Copy_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Options_Buffer := Ctx.P.Slots.Slot_Ptr_5;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_5 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Options.Byte_Size (Ctx.P.Options_Ctx) <= RFLX_Copy_Options_Buffer'Length then
               Universal.Options.Copy (Ctx.P.Options_Ctx, RFLX_Copy_Options_Buffer.all (RFLX_Copy_Options_Buffer'First .. RFLX_Copy_Options_Buffer'First + RFLX_Types.Index (Universal.Options.Byte_Size (Ctx.P.Options_Ctx) + 1) - 2));
            else
               RFLX_Exception := True;
            end if;
            Universal.Options.Initialize (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Options_Buffer'First), Universal.Options.Sequence_Last (Ctx.P.Options_Ctx));
            while Universal.Options.Has_Element (RFLX_Copy_Options_Ctx) loop
               pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Copy_Options_Ctx));
               pragma Loop_Invariant (Universal.Option_Types.Has_Buffer (Option_Types_Ctx));
               pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_First = RFLX_Copy_Options_Ctx.Buffer_First'Loop_Entry);
               pragma Loop_Invariant (Option_Types_Ctx.Buffer_First = Option_Types_Ctx.Buffer_First'Loop_Entry);
               pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_Last = RFLX_Copy_Options_Ctx.Buffer_Last'Loop_Entry);
               pragma Loop_Invariant (Option_Types_Ctx.Buffer_Last = Option_Types_Ctx.Buffer_Last'Loop_Entry);
               pragma Loop_Invariant (Universal.Option_Types.Valid (Option_Types_Ctx));
               declare
                  E_Ctx : Universal.Option.Context;
               begin
                  Universal.Options.Switch (RFLX_Copy_Options_Ctx, E_Ctx);
                  Universal.Option.Verify_Message (E_Ctx);
                  if Universal.Option.Valid (E_Ctx, Universal.Option.F_Option_Type) then
                     if
                        Universal.Option.Get_Option_Type (E_Ctx).Known
                        and then Universal.Option.Get_Option_Type (E_Ctx).Enum = Universal.OT_Data
                     then
                        if
                           Universal.Option_Types.Has_Element (Option_Types_Ctx)
                           and then Universal.Option_Types.Available_Space (Option_Types_Ctx) >= Universal.Option_Type_Enum'Size
                        then
                           Universal.Option_Types.Append_Element (Option_Types_Ctx, Universal.Option.Get_Option_Type (E_Ctx));
                        else
                           RFLX_Exception := True;
                        end if;
                     end if;
                  else
                     RFLX_Exception := True;
                  end if;
                  pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                  Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                  pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
               end;
               exit when RFLX_Exception;
            end loop;
            pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
            pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_5 := RFLX_Copy_Options_Buffer;
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if
         Universal.Option_Types.Size (Option_Types_Ctx) <= 64768
         and then Universal.Option_Types.Size (Option_Types_Ctx) mod RFLX_Types.Byte'Size = 0
      then
         if RFLX_Types.To_First_Bit_Index (Ctx.P.Message_1_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Ctx.P.Message_1_Ctx.Buffer_First) + 1 >= Universal.Option_Types.Size (Option_Types_Ctx) + 24 then
            Universal.Message.Reset (Ctx.P.Message_1_Ctx, RFLX_Types.To_First_Bit_Index (Ctx.P.Message_1_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Ctx.P.Message_1_Ctx.Buffer_First) + (Universal.Option_Types.Size (Option_Types_Ctx) + 24) - 1);
            Universal.Message.Set_Message_Type (Ctx.P.Message_1_Ctx, Universal.MT_Option_Types);
            Universal.Message.Set_Length (Ctx.P.Message_1_Ctx, Universal.Length (Universal.Option_Types.Size (Option_Types_Ctx) / 8));
            if Universal.Message.Valid_Length (Ctx.P.Message_1_Ctx, Universal.Message.F_Option_Types, RFLX_Types.To_Length (Universal.Option_Types.Size (Option_Types_Ctx))) then
               Universal.Message.Set_Option_Types (Ctx.P.Message_1_Ctx, Option_Types_Ctx);
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
               pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
               pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
               pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
               return;
            end if;
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
            pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
            pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
            pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
            return;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if Universal.Options.Valid (Ctx.P.Options_Ctx) then
         declare
            RFLX_Copy_Options_Ctx : Universal.Options.Context;
            RFLX_Copy_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Options_Buffer := Ctx.P.Slots.Slot_Ptr_7;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_7 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Options.Byte_Size (Ctx.P.Options_Ctx) <= RFLX_Copy_Options_Buffer'Length then
               Universal.Options.Copy (Ctx.P.Options_Ctx, RFLX_Copy_Options_Buffer.all (RFLX_Copy_Options_Buffer'First .. RFLX_Copy_Options_Buffer'First + RFLX_Types.Index (Universal.Options.Byte_Size (Ctx.P.Options_Ctx) + 1) - 2));
            else
               RFLX_Exception := True;
            end if;
            Universal.Options.Initialize (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Options_Buffer'First), Universal.Options.Sequence_Last (Ctx.P.Options_Ctx));
            while Universal.Options.Has_Element (RFLX_Copy_Options_Ctx) loop
               pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Copy_Options_Ctx));
               pragma Loop_Invariant (Universal.Options.Has_Buffer (Message_Options_Ctx));
               pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_First = RFLX_Copy_Options_Ctx.Buffer_First'Loop_Entry);
               pragma Loop_Invariant (Message_Options_Ctx.Buffer_First = Message_Options_Ctx.Buffer_First'Loop_Entry);
               pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_Last = RFLX_Copy_Options_Ctx.Buffer_Last'Loop_Entry);
               pragma Loop_Invariant (Message_Options_Ctx.Buffer_Last = Message_Options_Ctx.Buffer_Last'Loop_Entry);
               pragma Loop_Invariant (Universal.Options.Valid (Message_Options_Ctx));
               declare
                  E_Ctx : Universal.Option.Context;
               begin
                  Universal.Options.Switch (RFLX_Copy_Options_Ctx, E_Ctx);
                  Universal.Option.Verify_Message (E_Ctx);
                  if Universal.Option.Valid (E_Ctx, Universal.Option.F_Option_Type) then
                     if
                        Universal.Option.Get_Option_Type (E_Ctx).Known
                        and then Universal.Option.Get_Option_Type (E_Ctx).Enum = Universal.OT_Data
                     then
                        if
                           Universal.Options.Has_Element (Message_Options_Ctx)
                           and then Universal.Options.Available_Space (Message_Options_Ctx) >= Universal.Option.Size (E_Ctx)
                        then
                           if Universal.Option.Structural_Valid_Message (E_Ctx) then
                              if Universal.Option.Size (E_Ctx) > 0 then
                                 Universal.Options.Append_Element (Message_Options_Ctx, E_Ctx);
                              else
                                 RFLX_Exception := True;
                              end if;
                           else
                              RFLX_Exception := True;
                           end if;
                        else
                           RFLX_Exception := True;
                        end if;
                     end if;
                  else
                     RFLX_Exception := True;
                  end if;
                  pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                  Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                  pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
               end;
               exit when RFLX_Exception;
            end loop;
            pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
            pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_7 := RFLX_Copy_Options_Buffer;
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      if
         Universal.Options.Size (Message_Options_Ctx) <= 32768
         and then Universal.Options.Size (Message_Options_Ctx) mod RFLX_Types.Byte'Size = 0
      then
         if RFLX_Types.To_First_Bit_Index (Ctx.P.Message_2_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Ctx.P.Message_2_Ctx.Buffer_First) + 1 >= Universal.Options.Size (Message_Options_Ctx) + 24 then
            Universal.Message.Reset (Ctx.P.Message_2_Ctx, RFLX_Types.To_First_Bit_Index (Ctx.P.Message_2_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Ctx.P.Message_2_Ctx.Buffer_First) + (Universal.Options.Size (Message_Options_Ctx) + 24) - 1);
            Universal.Message.Set_Message_Type (Ctx.P.Message_2_Ctx, Universal.MT_Options);
            Universal.Message.Set_Length (Ctx.P.Message_2_Ctx, Universal.Length (Universal.Options.Size (Message_Options_Ctx) / 8));
            if Universal.Message.Valid_Length (Ctx.P.Message_2_Ctx, Universal.Message.F_Options, RFLX_Types.To_Length (Universal.Options.Size (Message_Options_Ctx))) then
               Universal.Message.Set_Options (Ctx.P.Message_2_Ctx, Message_Options_Ctx);
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
               pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
               pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
               pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
               return;
            end if;
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
            pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
            pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
            pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
            return;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
         pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
         pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
         return;
      end if;
      Ctx.P.Next_State := S_Send_1;
      pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
      pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_4 := Option_Types_Buffer;
      pragma Warnings (Off, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Message_Options_Ctx, Message_Options_Buffer);
      pragma Warnings (On, """Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_6 := Message_Options_Buffer;
   end Process;

   procedure Send_1 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Ctx.P.Next_State := S_Send_2;
   end Send_1;

   procedure Send_2 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Ctx.P.Next_State := S_Terminated;
   end Send_2;

   procedure Initialize (Ctx : in out Context'Class) is
      Options_Buffer : RFLX_Types.Bytes_Ptr;
      Message_1_Buffer : RFLX_Types.Bytes_Ptr;
      Message_2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Options_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Ctx.P.Options_Ctx, Options_Buffer);
      Message_1_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_1_Ctx, Message_1_Buffer);
      Message_2_Buffer := Ctx.P.Slots.Slot_Ptr_3;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_3 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_2_Ctx, Message_2_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Options_Buffer : RFLX_Types.Bytes_Ptr;
      Message_1_Buffer : RFLX_Types.Bytes_Ptr;
      Message_2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Ctx.P.Options_Ctx, Options_Buffer);
      pragma Warnings (On, """Ctx.P.Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_1 := Options_Buffer;
      pragma Warnings (Off, """Ctx.P.Message_1_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_1_Ctx, Message_1_Buffer);
      pragma Warnings (On, """Ctx.P.Message_1_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_2 := Message_1_Buffer;
      pragma Warnings (Off, """Ctx.P.Message_2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_2_Ctx, Message_2_Buffer);
      pragma Warnings (On, """Ctx.P.Message_2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_3 := Message_2_Buffer;
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Terminated;
   end Finalize;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Process =>
            Process (Ctx);
         when S_Send_1 =>
            Send_1 (Ctx);
         when S_Send_2 =>
            Send_2 (Ctx);
         when S_Terminated =>
            null;
      end case;
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Send_1 | S_Send_2);

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
      procedure Universal_Message_Read is new Universal.Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Send_1 =>
                  Universal_Message_Read (Ctx.P.Message_1_Ctx);
               when S_Send_2 =>
                  Universal_Message_Read (Ctx.P.Message_2_Ctx);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Read;

end RFLX.Test.Session;
