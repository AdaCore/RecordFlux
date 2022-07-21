pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal.Option_Types;
with RFLX.Universal.Options;
with RFLX.Universal.Option;

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.Universal.Message_Type;

   use type RFLX.RFLX_Types.Bit_Length;

   use type RFLX.Universal.Option_Type_Enum;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 /= null
         and Ctx.P.Slots.Slot_Ptr_3 /= null
         and Ctx.P.Slots.Slot_Ptr_4 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      --  tests/integration/session_comprehension_on_message_field/test.rflx:15:10
      Universal.Message.Verify_Message (Ctx.P.Message_Ctx);
      if
         Universal.Message.Structural_Valid_Message (Ctx.P.Message_Ctx)
         and then Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx) = Universal.MT_Options
      then
         Ctx.P.Next_State := S_Process;
      else
         Ctx.P.Next_State := S_Terminated;
      end if;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Option_Types_Ctx : Universal.Option_Types.Context;
      Option_Types_Buffer : RFLX_Types.Bytes_Ptr;
      function Process_Invariant return Boolean is
        (Global_Initialized (Ctx)
         and Universal.Option_Types.Has_Buffer (Option_Types_Ctx)
         and Option_Types_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Option_Types_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_3 /= null
         and Ctx.P.Slots.Slot_Ptr_4 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      Option_Types_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option_Types.Initialize (Option_Types_Ctx, Option_Types_Buffer);
      pragma Assert (Process_Invariant);
      --  tests/integration/session_comprehension_on_message_field/test.rflx:27:10
      Universal.Option_Types.Reset (Option_Types_Ctx);
      if Universal.Message.Structural_Valid_Message (Ctx.P.Message_Ctx) then
         declare
            RFLX_Message_Options_Ctx : Universal.Options.Context;
            RFLX_Message_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Message_Options_Buffer := Ctx.P.Slots.Slot_Ptr_3;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_3 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Message.Byte_Size (Ctx.P.Message_Ctx) <= RFLX_Message_Options_Buffer'Length then
               Universal.Message.Copy (Ctx.P.Message_Ctx, RFLX_Message_Options_Buffer.all (RFLX_Message_Options_Buffer'First .. RFLX_Message_Options_Buffer'First + RFLX_Types.Index (Universal.Message.Byte_Size (Ctx.P.Message_Ctx) + 1) - 2));
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
               pragma Assert (RFLX_Message_Options_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_3 := RFLX_Message_Options_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
            if Universal.Message.Structural_Valid (Ctx.P.Message_Ctx, Universal.Message.F_Options) then
               Universal.Options.Initialize (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer, Universal.Message.Field_First (Ctx.P.Message_Ctx, Universal.Message.F_Options), Universal.Message.Field_Last (Ctx.P.Message_Ctx, Universal.Message.F_Options));
               while Universal.Options.Has_Element (RFLX_Message_Options_Ctx) loop
                  pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Message_Options_Ctx));
                  pragma Loop_Invariant (Universal.Option_Types.Has_Buffer (Option_Types_Ctx));
                  pragma Loop_Invariant (RFLX_Message_Options_Ctx.Buffer_First = RFLX_Message_Options_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (Option_Types_Ctx.Buffer_First = Option_Types_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (RFLX_Message_Options_Ctx.Buffer_Last = RFLX_Message_Options_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Option_Types_Ctx.Buffer_Last = Option_Types_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Universal.Option_Types.Valid (Option_Types_Ctx));
                  pragma Loop_Invariant (RFLX_Message_Options_Buffer = null);
                  pragma Loop_Invariant (Ctx.P.Slots.Slot_Ptr_3 = null);
                  declare
                     E_Ctx : Universal.Option.Context;
                  begin
                     Universal.Options.Switch (RFLX_Message_Options_Ctx, E_Ctx);
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
                              Ctx.P.Next_State := S_Terminated;
                              pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                              Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                              pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                              pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
                              pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
                              pragma Assert (RFLX_Message_Options_Buffer /= null);
                              Ctx.P.Slots.Slot_Ptr_3 := RFLX_Message_Options_Buffer;
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
                              pragma Assert (Process_Invariant);
                              goto Finalize_Process;
                           end if;
                        end if;
                     else
                        Ctx.P.Next_State := S_Terminated;
                        pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                        Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                        pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                        pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
                        pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
                        pragma Assert (RFLX_Message_Options_Buffer /= null);
                        Ctx.P.Slots.Slot_Ptr_3 := RFLX_Message_Options_Buffer;
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
                        pragma Assert (Process_Invariant);
                        goto Finalize_Process;
                     end if;
                     pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                     Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                     pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                  end;
               end loop;
               pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
               pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
               pragma Assert (RFLX_Message_Options_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_3 := RFLX_Message_Options_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_3 = null);
            pragma Assert (RFLX_Message_Options_Buffer /= null);
            Ctx.P.Slots.Slot_Ptr_3 := RFLX_Message_Options_Buffer;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_3 /= null);
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      --  tests/integration/session_comprehension_on_message_field/test.rflx:29:10
      Universal.Option_Types.Reset (Option_Types_Ctx);
      if Universal.Message.Structural_Valid_Message (Ctx.P.Message_Ctx) then
         declare
            RFLX_Message_Options_Ctx : Universal.Options.Context;
            RFLX_Message_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Message_Options_Buffer := Ctx.P.Slots.Slot_Ptr_4;
            pragma Warnings (Off, "unused assignment");
            Ctx.P.Slots.Slot_Ptr_4 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Message.Byte_Size (Ctx.P.Message_Ctx) <= RFLX_Message_Options_Buffer'Length then
               Universal.Message.Copy (Ctx.P.Message_Ctx, RFLX_Message_Options_Buffer.all (RFLX_Message_Options_Buffer'First .. RFLX_Message_Options_Buffer'First + RFLX_Types.Index (Universal.Message.Byte_Size (Ctx.P.Message_Ctx) + 1) - 2));
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
               pragma Assert (RFLX_Message_Options_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_4 := RFLX_Message_Options_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
            if Universal.Message.Structural_Valid (Ctx.P.Message_Ctx, Universal.Message.F_Options) then
               Universal.Options.Initialize (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer, Universal.Message.Field_First (Ctx.P.Message_Ctx, Universal.Message.F_Options), Universal.Message.Field_Last (Ctx.P.Message_Ctx, Universal.Message.F_Options));
               while Universal.Options.Has_Element (RFLX_Message_Options_Ctx) loop
                  pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Message_Options_Ctx));
                  pragma Loop_Invariant (Universal.Option_Types.Has_Buffer (Option_Types_Ctx));
                  pragma Loop_Invariant (RFLX_Message_Options_Ctx.Buffer_First = RFLX_Message_Options_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (Option_Types_Ctx.Buffer_First = Option_Types_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (RFLX_Message_Options_Ctx.Buffer_Last = RFLX_Message_Options_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Option_Types_Ctx.Buffer_Last = Option_Types_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Universal.Option_Types.Valid (Option_Types_Ctx));
                  pragma Loop_Invariant (RFLX_Message_Options_Buffer = null);
                  pragma Loop_Invariant (Ctx.P.Slots.Slot_Ptr_4 = null);
                  declare
                     E_Ctx : Universal.Option.Context;
                  begin
                     Universal.Options.Switch (RFLX_Message_Options_Ctx, E_Ctx);
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
                              Ctx.P.Next_State := S_Terminated;
                              pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                              Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                              pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                              pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
                              pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                              pragma Assert (RFLX_Message_Options_Buffer /= null);
                              Ctx.P.Slots.Slot_Ptr_4 := RFLX_Message_Options_Buffer;
                              pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                              pragma Assert (Process_Invariant);
                              goto Finalize_Process;
                           end if;
                        end if;
                     else
                        Ctx.P.Next_State := S_Terminated;
                        pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                        Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                        pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                        pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
                        pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
                        pragma Assert (RFLX_Message_Options_Buffer /= null);
                        Ctx.P.Slots.Slot_Ptr_4 := RFLX_Message_Options_Buffer;
                        pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
                        pragma Assert (Process_Invariant);
                        goto Finalize_Process;
                     end if;
                     pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                     Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                     pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                  end;
               end loop;
               pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
               pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
               pragma Assert (RFLX_Message_Options_Buffer /= null);
               Ctx.P.Slots.Slot_Ptr_4 := RFLX_Message_Options_Buffer;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_4 = null);
            pragma Assert (RFLX_Message_Options_Buffer /= null);
            Ctx.P.Slots.Slot_Ptr_4 := RFLX_Message_Options_Buffer;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_4 /= null);
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      --  tests/integration/session_comprehension_on_message_field/test.rflx:31:10
      Universal.Message.Reset (Ctx.P.Message_Ctx);
      if
         not (Universal.Option_Types.Size (Option_Types_Ctx) <= 32768
          and then Universal.Option_Types.Size (Option_Types_Ctx) mod RFLX_Types.Byte'Size = 0)
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if Universal.Message.Available_Space (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) < Universal.Option_Types.Size (Option_Types_Ctx) + 24 then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type));
      Universal.Message.Set_Message_Type (Ctx.P.Message_Ctx, Universal.MT_Option_Types);
      pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Length));
      Universal.Message.Set_Length (Ctx.P.Message_Ctx, Universal.Length (Universal.Option_Types.Size (Option_Types_Ctx) / 8));
      if Universal.Message.Valid_Length (Ctx.P.Message_Ctx, Universal.Message.F_Option_Types, Universal.Option_Types.Byte_Size (Option_Types_Ctx)) then
         pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Option_Types));
         Universal.Message.Set_Option_Types (Ctx.P.Message_Ctx, Option_Types_Ctx);
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      Ctx.P.Next_State := S_Reply;
      pragma Assert (Process_Invariant);
      <<Finalize_Process>>
      pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
      pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (Option_Types_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := Option_Types_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
      pragma Assert (Global_Initialized (Ctx));
   end Process;

   procedure Reply (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 /= null
         and Ctx.P.Slots.Slot_Ptr_3 /= null
         and Ctx.P.Slots.Slot_Ptr_4 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_Invariant);
      --  tests/integration/session_comprehension_on_message_field/test.rflx:42:10
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Terminated;
   end Finalize;

   procedure Reset_Messages_Before_Write (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Universal.Message.Reset (Ctx.P.Message_Ctx, Ctx.P.Message_Ctx.First, Ctx.P.Message_Ctx.First - 1);
         when S_Process | S_Reply | S_Terminated =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Process =>
            Process (Ctx);
         when S_Reply =>
            Reply (Ctx);
         when S_Terminated =>
            null;
      end case;
      Reset_Messages_Before_Write (Ctx);
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Start | S_Reply);

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
               when S_Reply =>
                  Universal_Message_Read (Ctx.P.Message_Ctx);
               when others =>
                  null;
            end case;
      end case;
   end Read;

   procedure Write (Ctx : in out Context'Class; Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      Write_Buffer_Length : constant RFLX_Types.Length := Write_Buffer_Size (Ctx, Chan);
      function Write_Pre (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is
        (Buffer'Length > 0
         and then Context_Buffer_Length = Write_Buffer_Length
         and then Offset <= RFLX_Types.Length'Last - Buffer'Length
         and then Buffer'Length + Offset <= Write_Buffer_Length);
      procedure Write (Message_Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length; Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) with
        Pre =>
          Write_Pre (Context_Buffer_Length, Offset)
          and then Offset <= RFLX_Types.Length'Last - Message_Buffer'Length
          and then Message_Buffer'Length + Offset = Write_Buffer_Length,
        Post =>
          Length <= Message_Buffer'Length
      is
      begin
         Length := Buffer'Length;
         Message_Buffer := (others => 0);
         Message_Buffer (Message_Buffer'First .. RFLX_Types.Index (RFLX_Types.Length (Message_Buffer'First) - 1 + Length)) := Buffer;
      end Write;
      procedure Universal_Message_Write is new Universal.Message.Generic_Write (Write, Write_Pre);
   begin
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Start =>
                  Universal_Message_Write (Ctx.P.Message_Ctx, Offset);
               when others =>
                  null;
            end case;
      end case;
   end Write;

end RFLX.Test.Session;
