pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal.Options;
with RFLX.Universal.Option;

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Bit_Length;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Options_Ctx : Universal.Options.Context;
      Options_Buffer : RFLX_Types.Bytes_Ptr;
      RFLX_Exception : Boolean := False;
   begin
      Options_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Options_Ctx, Options_Buffer);
      pragma Assert (Global_Initialized (Ctx)
                     and Universal.Options.Has_Buffer (Options_Ctx)
                     and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                     and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                     and Ctx.P.Slots.Slot_Ptr_2 = null
                     and Ctx.P.Slots.Slot_Ptr_1 = null);
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < 32
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Global_Initialized (Ctx)
                        and Universal.Options.Has_Buffer (Options_Ctx)
                        and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                        and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                        and Ctx.P.Slots.Slot_Ptr_2 = null
                        and Ctx.P.Slots.Slot_Ptr_1 = null);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 1);
         if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (1)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Global_Initialized (Ctx)
                        and Universal.Options.Has_Buffer (Options_Ctx)
                        and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                        and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                        and Ctx.P.Slots.Slot_Ptr_2 = null
                        and Ctx.P.Slots.Slot_Ptr_1 = null);
         goto Finalize_Start;
      end if;
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < 40
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Global_Initialized (Ctx)
                        and Universal.Options.Has_Buffer (Options_Ctx)
                        and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                        and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                        and Ctx.P.Slots.Slot_Ptr_2 = null
                        and Ctx.P.Slots.Slot_Ptr_1 = null);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 2);
         if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Byte'Val (2), RFLX_Types.Byte'Val (3)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Global_Initialized (Ctx)
                        and Universal.Options.Has_Buffer (Options_Ctx)
                        and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                        and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                        and Ctx.P.Slots.Slot_Ptr_2 = null
                        and Ctx.P.Slots.Slot_Ptr_1 = null);
         goto Finalize_Start;
      end if;
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < 8
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Global_Initialized (Ctx)
                        and Universal.Options.Has_Buffer (Options_Ctx)
                        and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                        and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                        and Ctx.P.Slots.Slot_Ptr_2 = null
                        and Ctx.P.Slots.Slot_Ptr_1 = null);
         goto Finalize_Start;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Null);
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if
         Universal.Options.Size (Options_Ctx) <= 32768
         and then Universal.Options.Size (Options_Ctx) mod RFLX_Types.Byte'Size = 0
      then
         if RFLX_Types.To_First_Bit_Index (Ctx.P.Message_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Ctx.P.Message_Ctx.Buffer_First) + 1 >= Universal.Options.Size (Options_Ctx) + 8 then
            Universal.Message.Reset (Ctx.P.Message_Ctx, RFLX_Types.To_First_Bit_Index (Ctx.P.Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Ctx.P.Message_Ctx.Buffer_First) + (Universal.Options.Size (Options_Ctx) + 8) - 1);
            Universal.Message.Set_Message_Type (Ctx.P.Message_Ctx, Universal.MT_Unconstrained_Options);
            if Universal.Message.Valid_Length (Ctx.P.Message_Ctx, Universal.Message.F_Options, RFLX_Types.To_Length (Universal.Options.Size (Options_Ctx))) then
               Universal.Message.Set_Options (Ctx.P.Message_Ctx, Options_Ctx);
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Global_Initialized (Ctx)
                              and Universal.Options.Has_Buffer (Options_Ctx)
                              and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                              and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                              and Ctx.P.Slots.Slot_Ptr_2 = null
                              and Ctx.P.Slots.Slot_Ptr_1 = null);
               goto Finalize_Start;
            end if;
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Assert (Global_Initialized (Ctx)
                           and Universal.Options.Has_Buffer (Options_Ctx)
                           and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                           and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                           and Ctx.P.Slots.Slot_Ptr_2 = null
                           and Ctx.P.Slots.Slot_Ptr_1 = null);
            goto Finalize_Start;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Global_Initialized (Ctx)
                        and Universal.Options.Has_Buffer (Options_Ctx)
                        and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                        and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                        and Ctx.P.Slots.Slot_Ptr_2 = null
                        and Ctx.P.Slots.Slot_Ptr_1 = null);
         goto Finalize_Start;
      end if;
      Ctx.P.Next_State := S_Reply;
      pragma Assert (Global_Initialized (Ctx)
                     and Universal.Options.Has_Buffer (Options_Ctx)
                     and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
                     and Options_Ctx.Buffer_Last = RFLX.RFLX_Types.Index'First + 4095
                     and Ctx.P.Slots.Slot_Ptr_2 = null
                     and Ctx.P.Slots.Slot_Ptr_1 = null);
      <<Finalize_Start>>
      pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
      pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_2 := Options_Buffer;
      pragma Assert (Global_Initialized (Ctx));
   end Start;

   procedure Reply (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 /= null);
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 /= null);
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
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
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

end RFLX.Test.Session;
