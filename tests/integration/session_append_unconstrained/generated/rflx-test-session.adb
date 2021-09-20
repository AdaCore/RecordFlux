pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal.Options;
with RFLX.Universal.Option;
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
      Options_Ctx : Universal.Options.Context;
      RFLX_Exception : Boolean := False;
      Options_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Options_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);
      Universal.Options.Initialize (Options_Ctx, Options_Buffer);
      if
        not Universal.Options.Has_Element (Options_Ctx)
        or Universal.Options.Available_Space (Options_Ctx) < 32
      then
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 1);
         if Universal.Option.Field_Size (RFLX_Element_Options_Ctx, Universal.Option.F_Data) = 1 * RFLX_Types.Byte'Size then
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (1)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, "unused assignment to ""RFLX_Element_Options_Ctx""");
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""RFLX_Element_Options_Ctx""");
      end;
      if RFLX_Exception then
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      if
        not Universal.Options.Has_Element (Options_Ctx)
        or Universal.Options.Available_Space (Options_Ctx) < 40
      then
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
         Universal.Option.Set_Length (RFLX_Element_Options_Ctx, 2);
         if Universal.Option.Field_Size (RFLX_Element_Options_Ctx, Universal.Option.F_Data) = 2 * RFLX_Types.Byte'Size then
            Universal.Option.Set_Data (RFLX_Element_Options_Ctx, (RFLX_Types.Byte'Val (2), RFLX_Types.Byte'Val (3)));
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, "unused assignment to ""RFLX_Element_Options_Ctx""");
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""RFLX_Element_Options_Ctx""");
      end;
      if RFLX_Exception then
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      if
        not Universal.Options.Has_Element (Options_Ctx)
        or Universal.Options.Available_Space (Options_Ctx) < 8
      then
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Null);
         pragma Warnings (Off, "unused assignment to ""RFLX_Element_Options_Ctx""");
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""RFLX_Element_Options_Ctx""");
      end;
      if
        Universal.Options.Size (Options_Ctx) <= 32768
        and then Universal.Options.Size (Options_Ctx) mod RFLX_Types.Byte'Size = 0
      then
         if Message_Ctx.Last - Message_Ctx.First + 1 >= RFLX_Types.Bit_Length (Universal.Options.Size (Options_Ctx) + 8) then
            Universal.Message.Reset (Message_Ctx, RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First) + RFLX_Types.Bit_Length (Universal.Options.Size (Options_Ctx) + 8) - 1);
            Universal.Message.Set_Message_Type (Message_Ctx, Universal.MT_Unconstrained_Options);
            if Universal.Message.Field_Size (Message_Ctx, Universal.Message.F_Options) = Universal.Options.Size (Options_Ctx) then
               Universal.Message.Set_Options (Message_Ctx, Options_Ctx);
            else
               State := S_Terminated;
               pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
               pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
               pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               pragma Warnings (On, "unused assignment to ""Options_Ctx""");
               RFLX_Types.Free (Options_Buffer);
               return;
            end if;
         else
            State := S_Terminated;
            pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
            pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
            pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Warnings (On, "unused assignment to ""Options_Ctx""");
            RFLX_Types.Free (Options_Buffer);
            return;
         end if;
      else
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      if Universal.Message.Structural_Valid_Message (Message_Ctx) then
         declare
            procedure Universal_Message_Read is new Universal.Message.Read (Channel_Write);
         begin
            Universal_Message_Read (Message_Ctx);
         end;
      else
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         RFLX_Types.Free (Options_Buffer);
         return;
      end if;
      State := S_Terminated;
      pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
      pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
      pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Options_Ctx""");
      RFLX_Types.Free (Options_Buffer);
   end Start;

   procedure Initialize is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      if Universal.Message.Has_Buffer (Message_Ctx) then
         pragma Warnings (Off, "unused assignment to ""Message_Ctx""");
         pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Message.Take_Buffer (Message_Ctx, Message_Buffer);
         pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Message_Ctx""");
         RFLX_Types.Free (Message_Buffer);
      end if;
      Message_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);
      Universal.Message.Initialize (Message_Ctx, Message_Buffer);
      State := S_Start;
   end Initialize;

   procedure Finalize is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Message_Ctx""");
      RFLX_Types.Free (Message_Buffer);
      State := S_Terminated;
   end Finalize;

   procedure Tick is
   begin
      case State is
         when S_Start =>
            Start (State);
         when S_Terminated =>
            null;
      end case;
   end Tick;

   procedure Run is
   begin
      Initialize;
      while Active loop
         pragma Loop_Invariant (Initialized);
         Tick;
      end loop;
      Finalize;
   end Run;

end RFLX.Test.Session;
