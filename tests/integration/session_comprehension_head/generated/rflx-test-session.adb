pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Bit_Length;

   use type RFLX.Universal.Option_Type_Enum;

   use type RFLX.Universal.Length;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 /= null
         and Ctx.P.Slots.Slot_Ptr_4 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      --  tests/integration/session_comprehension_head/test.rflx:17:10
      if
         not Universal.Options.Has_Element (Ctx.P.Options_Ctx)
         or Universal.Options.Available_Space (Ctx.P.Options_Ctx) < 32
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         if Universal.Option.Available_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type) < 32 then
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
            pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Start_Invariant);
            goto Finalize_Start;
         end if;
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type));
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Length));
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 1);
         if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
            pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Data));
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
            pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Start_Invariant);
            goto Finalize_Start;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      --  tests/integration/session_comprehension_head/test.rflx:19:10
      if
         not Universal.Options.Has_Element (Ctx.P.Options_Ctx)
         or Universal.Options.Available_Space (Ctx.P.Options_Ctx) < 8
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         if Universal.Option.Available_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type) < 8 then
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
            pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Start_Invariant);
            goto Finalize_Start;
         end if;
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type));
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Null);
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      --  tests/integration/session_comprehension_head/test.rflx:21:10
      if
         not Universal.Options.Has_Element (Ctx.P.Options_Ctx)
         or Universal.Options.Available_Space (Ctx.P.Options_Ctx) < 40
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         if Universal.Option.Available_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type) < 40 then
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
            pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Start_Invariant);
            goto Finalize_Start;
         end if;
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type));
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Length));
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 2);
         if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
            pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Data));
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Byte'Val (2), RFLX_Types.Byte'Val (3)));
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
            pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Start_Invariant);
            goto Finalize_Start;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Ctx.P.Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      Ctx.P.Next_State := S_Process;
      pragma Assert (Start_Invariant);
      <<Finalize_Start>>
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      First_Option_Length : Universal.Length;
      function Process_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 /= null
         and Ctx.P.Slots.Slot_Ptr_4 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Process_Invariant);
      --  tests/integration/session_comprehension_head/test.rflx:32:10
      if Universal.Options.Valid (Ctx.P.Options_Ctx) then
         declare
            RFLX_Copy_Options_Ctx : Universal.Options.Context;
            RFLX_Copy_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Options_Buffer := Ctx.P.Slots.Slot_Ptr_3;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_3 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Options.Byte_Size (Ctx.P.Options_Ctx) <= RFLX_Copy_Options_Buffer'Length then
               Universal.Options.Copy (Ctx.P.Options_Ctx, RFLX_Copy_Options_Buffer.all (RFLX_Copy_Options_Buffer'First .. RFLX_Copy_Options_Buffer'First + RFLX_Types.Index (Universal.Options.Byte_Size (Ctx.P.Options_Ctx) + 1) - 2));
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
               pragma Assert (RFLX_Copy_Options_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_3 := RFLX_Copy_Options_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
            Universal.Options.Initialize (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Options_Buffer'First), Universal.Options.Sequence_Last (Ctx.P.Options_Ctx));
            declare
               RFLX_First_Option_Length_Found : Boolean := False;
            begin
               First_Option_Length := Universal.Length'First;
               while Universal.Options.Has_Element (RFLX_Copy_Options_Ctx) loop
                  pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Copy_Options_Ctx));
                  pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_First = RFLX_Copy_Options_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_Last = RFLX_Copy_Options_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (RFLX_Copy_Options_Buffer = null);
                  pragma Loop_Invariant (Ctx.P.Slots.Slot_Ptr_3 = null);
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
                           if Universal.Option.Valid (E_Ctx, Universal.Option.F_Length) then
                              First_Option_Length := Universal.Option.Get_Length (E_Ctx);
                           else
                              Ctx.P.Next_State := S_Terminated;
                              pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                              Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                              pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                              pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                              pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
                              pragma Assert (RFLX_Copy_Options_Buffer /= null);
                              Ctx.P.Slots.Slot_Ptr_3 := RFLX_Copy_Options_Buffer;
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
                              pragma Assert (Process_Invariant);
                              goto Finalize_Process;
                           end if;
                           RFLX_First_Option_Length_Found := True;
                           pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                           Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                           pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                           exit;
                        end if;
                     else
                        Ctx.P.Next_State := S_Terminated;
                        pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                        Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                        pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                        pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                        pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
                        pragma Assert (RFLX_Copy_Options_Buffer /= null);
                        Ctx.P.Slots.Slot_Ptr_3 := RFLX_Copy_Options_Buffer;
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
                        pragma Assert (Process_Invariant);
                        goto Finalize_Process;
                     end if;
                     pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                     Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                     pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                  end;
               end loop;
               if not RFLX_First_Option_Length_Found then
                  Ctx.P.Next_State := S_Terminated;
                  pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                  Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                  pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                  pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
                  pragma Assert (RFLX_Copy_Options_Buffer /= null);
                  Ctx.P.Slots.Slot_Ptr_3 := RFLX_Copy_Options_Buffer;
                  pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
                  pragma Assert (Process_Invariant);
                  goto Finalize_Process;
               end if;
            end;
            pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
            pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
            pragma Assert (RFLX_Copy_Options_Buffer /= null);
            Ctx.P.Slots.Slot_Ptr_3 := RFLX_Copy_Options_Buffer;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      --  tests/integration/session_comprehension_head/test.rflx:34:10
      if Universal.Options.Valid (Ctx.P.Options_Ctx) then
         declare
            RFLX_Copy_Options_Ctx : Universal.Options.Context;
            RFLX_Copy_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Copy_Options_Buffer := Ctx.P.Slots.Slot_Ptr_4;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_4 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Options.Byte_Size (Ctx.P.Options_Ctx) <= RFLX_Copy_Options_Buffer'Length then
               Universal.Options.Copy (Ctx.P.Options_Ctx, RFLX_Copy_Options_Buffer.all (RFLX_Copy_Options_Buffer'First .. RFLX_Copy_Options_Buffer'First + RFLX_Types.Index (Universal.Options.Byte_Size (Ctx.P.Options_Ctx) + 1) - 2));
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
               pragma Assert (RFLX_Copy_Options_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Options_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
            Universal.Options.Initialize (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer, RFLX_Types.To_First_Bit_Index (RFLX_Copy_Options_Buffer'First), Universal.Options.Sequence_Last (Ctx.P.Options_Ctx));
            declare
               RFLX_First_Option_Found : Boolean := False;
            begin
               while Universal.Options.Has_Element (RFLX_Copy_Options_Ctx) loop
                  pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Copy_Options_Ctx));
                  pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_First = RFLX_Copy_Options_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (RFLX_Copy_Options_Ctx.Buffer_Last = RFLX_Copy_Options_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Ctx.P.First_Option_Ctx.Buffer_First = Ctx.P.First_Option_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (Ctx.P.First_Option_Ctx.Buffer_Last = Ctx.P.First_Option_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Universal.Option.Has_Buffer (Ctx.P.First_Option_Ctx));
                  pragma Loop_Invariant (RFLX_Copy_Options_Buffer = null);
                  pragma Loop_Invariant (Ctx.P.Slots.Slot_Ptr_4 = null);
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
                           if Universal.Option.Structural_Valid_Message (E_Ctx) then
                              declare
                                 RFLX_Target_First_Option_Buffer : RFLX_Types.Bytes_Ptr;
                              begin
                                 pragma Warnings (Off, """Ctx.P.First_Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                                 Universal.Option.Take_Buffer (Ctx.P.First_Option_Ctx, RFLX_Target_First_Option_Buffer);
                                 pragma Warnings (On, """Ctx.P.First_Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                                 if Universal.Option.Byte_Size (E_Ctx) <= RFLX_Target_First_Option_Buffer'Length then
                                    Universal.Option.Copy (E_Ctx, RFLX_Target_First_Option_Buffer.all (RFLX_Target_First_Option_Buffer'First .. RFLX_Target_First_Option_Buffer'First + RFLX_Types.Index (Universal.Option.Byte_Size (E_Ctx) + 1) - 2));
                                 else
                                    Ctx.P.Next_State := S_Terminated;
                                    pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                                    Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                                    pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                                    pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                                    Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                                    pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                                    pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                                    pragma Assert (RFLX_Copy_Options_Buffer /= null);
                                    Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Options_Buffer;
                                    pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                                    pragma Assert (Process_Invariant);
                                    goto Finalize_Process;
                                 end if;
                                 Universal.Option.Initialize (Ctx.P.First_Option_Ctx, RFLX_Target_First_Option_Buffer, Universal.Option.Size (E_Ctx));
                                 Universal.Option.Verify_Message (Ctx.P.First_Option_Ctx);
                              end;
                           else
                              Ctx.P.Next_State := S_Terminated;
                              pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                              Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                              pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                              pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                              pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                              pragma Assert (RFLX_Copy_Options_Buffer /= null);
                              Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Options_Buffer;
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                              pragma Assert (Process_Invariant);
                              goto Finalize_Process;
                           end if;
                           RFLX_First_Option_Found := True;
                           pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                           Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                           pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                           exit;
                        end if;
                     else
                        Ctx.P.Next_State := S_Terminated;
                        pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                        Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                        pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                        pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                        pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                        pragma Assert (RFLX_Copy_Options_Buffer /= null);
                        Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Options_Buffer;
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                        pragma Assert (Process_Invariant);
                        goto Finalize_Process;
                     end if;
                     pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                     Universal.Options.Update (RFLX_Copy_Options_Ctx, E_Ctx);
                     pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                  end;
               end loop;
               if not RFLX_First_Option_Found then
                  Ctx.P.Next_State := S_Terminated;
                  pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                  Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
                  pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                  pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                  pragma Assert (RFLX_Copy_Options_Buffer /= null);
                  Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Options_Buffer;
                  pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                  pragma Assert (Process_Invariant);
                  goto Finalize_Process;
               end if;
            end;
            pragma Warnings (Off, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (RFLX_Copy_Options_Ctx, RFLX_Copy_Options_Buffer);
            pragma Warnings (On, """RFLX_Copy_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
            pragma Assert (RFLX_Copy_Options_Buffer /= null);
            Ctx.P.Slots.Slot_Ptr_4 := RFLX_Copy_Options_Buffer;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if First_Option_Length > 0 then
         Ctx.P.Next_State := S_Send;
      else
         Ctx.P.Next_State := S_Terminated;
      end if;
      pragma Assert (Process_Invariant);
      <<Finalize_Process>>
   end Process;

   procedure Send (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Send_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 /= null
         and Ctx.P.Slots.Slot_Ptr_4 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Send_Invariant);
      --  tests/integration/session_comprehension_head/test.rflx:46:10
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Send_Invariant);
   end Send;

   procedure Initialize (Ctx : in out Context'Class) is
      Options_Buffer : RFLX_Types.Bytes_Ptr;
      First_Option_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Options_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Ctx.P.Options_Ctx, Options_Buffer);
      First_Option_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option.Initialize (Ctx.P.First_Option_Ctx, First_Option_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Options_Buffer : RFLX_Types.Bytes_Ptr;
      First_Option_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Ctx.P.Options_Ctx, Options_Buffer);
      pragma Warnings (On, """Ctx.P.Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Options_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Options_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      pragma Warnings (Off, """Ctx.P.First_Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option.Take_Buffer (Ctx.P.First_Option_Ctx, First_Option_Buffer);
      pragma Warnings (On, """Ctx.P.First_Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (First_Option_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := First_Option_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
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
         when S_Send =>
            Send (Ctx);
         when S_Terminated =>
            null;
      end case;
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Send);

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
      procedure Universal_Option_Read is new Universal.Option.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Send =>
                  Universal_Option_Read (Ctx.P.First_Option_Ctx);
               when others =>
                  null;
            end case;
      end case;
   end Read;

end RFLX.Test.Session;