pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal;
with RFLX.Universal.Option_Types;
with RFLX.Universal.Option;
with RFLX.Universal.Options;
use type RFLX.Universal.Message_Type;
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;
use type RFLX.Universal.Option_Type;
use type RFLX.Universal.Option_Type_Enum;
use type RFLX.RFLX_Types.Length;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      declare
         procedure Universal_Message_Write is new Universal.Message.Write (Channel_Read);
      begin
         Universal_Message_Write (Message_Ctx);
      end;
      Universal.Message.Verify_Message (Message_Ctx);
      if
        Universal.Message.Structural_Valid_Message (Message_Ctx)
        and then Universal.Message.Get_Message_Type (Message_Ctx) = Universal.MT_Options
      then
         Next_State := S_Reply;
      else
         Next_State := S_Terminated;
      end if;
   end Start;

   procedure Reply (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
      Option_Types_Ctx : Universal.Option_Types.Context;
      RFLX_Exception : Boolean := False;
      Option_Types_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Option_Types_Buffer := Test.Session_Allocator.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option_Types.Initialize (Option_Types_Ctx, Option_Types_Buffer);
      if Universal.Message.Structural_Valid_Message (Message_Ctx) then
         declare
            RFLX_Message_Options_Ctx : Universal.Options.Context;
            RFLX_Message_Options_Buffer : RFLX_Types.Bytes_Ptr;
         begin
            RFLX_Message_Options_Buffer := Test.Session_Allocator.Slot_Ptr_3;
            pragma Warnings (Off, "unused assignment");
            Test.Session_Allocator.Slot_Ptr_3 := null;
            pragma Warnings (On, "unused assignment");
            if Universal.Message.Byte_Size (Message_Ctx) <= RFLX_Message_Options_Buffer'Length then
               Universal.Message.Copy (Message_Ctx, RFLX_Message_Options_Buffer.all (RFLX_Message_Options_Buffer'First .. RFLX_Message_Options_Buffer'First + RFLX_Types.Index (Universal.Message.Byte_Size (Message_Ctx) + 1) - 2));
            else
               RFLX_Exception := True;
            end if;
            if Universal.Message.Structural_Valid (Message_Ctx, Universal.Message.F_Options) then
               Universal.Options.Initialize (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer, Universal.Message.Field_First (Message_Ctx, Universal.Message.F_Options), Universal.Message.Field_Last (Message_Ctx, Universal.Message.F_Options));
               while Universal.Options.Has_Element (RFLX_Message_Options_Ctx) loop
                  pragma Loop_Invariant (Universal.Options.Has_Buffer (RFLX_Message_Options_Ctx));
                  pragma Loop_Invariant (Universal.Option_Types.Has_Buffer (Option_Types_Ctx));
                  pragma Loop_Invariant (RFLX_Message_Options_Ctx.Buffer_First = RFLX_Message_Options_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (Option_Types_Ctx.Buffer_First = Option_Types_Ctx.Buffer_First'Loop_Entry);
                  pragma Loop_Invariant (RFLX_Message_Options_Ctx.Buffer_Last = RFLX_Message_Options_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Option_Types_Ctx.Buffer_Last = Option_Types_Ctx.Buffer_Last'Loop_Entry);
                  pragma Loop_Invariant (Universal.Option_Types.Valid (Option_Types_Ctx));
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
                              RFLX_Exception := True;
                           end if;
                        end if;
                     else
                        RFLX_Exception := True;
                     end if;
                     pragma Warnings (Off, "unused assignment to ""E_Ctx""");
                     pragma Warnings (Off, """E_Ctx"" is set by ""Update"" but not used after the call");
                     Universal.Options.Update (RFLX_Message_Options_Ctx, E_Ctx);
                     pragma Warnings (On, """E_Ctx"" is set by ""Update"" but not used after the call");
                     pragma Warnings (On, "unused assignment to ""E_Ctx""");
                  end;
                  exit when RFLX_Exception;
               end loop;
               pragma Warnings (Off, "unused assignment to ""RFLX_Message_Options_Ctx""");
               pragma Warnings (Off, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (RFLX_Message_Options_Ctx, RFLX_Message_Options_Buffer);
               pragma Warnings (On, """RFLX_Message_Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               pragma Warnings (On, "unused assignment to ""RFLX_Message_Options_Ctx""");
            else
               RFLX_Exception := True;
            end if;
            pragma Warnings (Off, "unused assignment");
            Test.Session_Allocator.Slot_Ptr_3 := RFLX_Message_Options_Buffer;
            pragma Warnings (On, "unused assignment");
         end;
      else
         Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      if RFLX_Exception then
         Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      if RFLX_Exception then
         Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      if
        Universal.Option_Types.Size (Option_Types_Ctx) <= 32768
        and then Universal.Option_Types.Size (Option_Types_Ctx) mod RFLX_Types.Byte'Size = 0
      then
         if Message_Ctx.Last - Message_Ctx.First + 1 >= RFLX_Types.Bit_Length (Universal.Option_Types.Size (Option_Types_Ctx) + 24) then
            Universal.Message.Reset (Message_Ctx, RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First) + RFLX_Types.Bit_Length (Universal.Option_Types.Size (Option_Types_Ctx) + 24) - 1);
            Universal.Message.Set_Message_Type (Message_Ctx, Universal.MT_Option_Types);
            Universal.Message.Set_Length (Message_Ctx, Universal.Length (Universal.Option_Types.Size (Option_Types_Ctx) / 8));
            if Universal.Message.Field_Size (Message_Ctx, Universal.Message.F_Option_Types) = Universal.Option_Types.Size (Option_Types_Ctx) then
               Universal.Message.Set_Option_Types (Message_Ctx, Option_Types_Ctx);
            else
               Next_State := S_Terminated;
               pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
               pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
               pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
               pragma Warnings (Off, "unused assignment");
               Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
               pragma Warnings (On, "unused assignment");
               return;
            end if;
         else
            Next_State := S_Terminated;
            pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
            pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
            pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
            pragma Warnings (Off, "unused assignment");
            Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
            pragma Warnings (On, "unused assignment");
            return;
         end if;
      else
         Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      if Universal.Message.Structural_Valid_Message (Message_Ctx) then
         declare
            procedure Universal_Message_Read is new Universal.Message.Read (Channel_Write);
         begin
            Universal_Message_Read (Message_Ctx);
         end;
      else
         Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
         pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      Next_State := S_Terminated;
      pragma Warnings (Off, "unused assignment to ""Option_Types_Ctx""");
      pragma Warnings (Off, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option_Types.Take_Buffer (Option_Types_Ctx, Option_Types_Buffer);
      pragma Warnings (On, """Option_Types_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Option_Types_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := Option_Types_Buffer;
      pragma Warnings (On, "unused assignment");
   end Reply;

   procedure Initialize is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      Message_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Message_Ctx, Message_Buffer);
      Next_State := S_Start;
   end Initialize;

   procedure Finalize is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := Message_Buffer;
      pragma Warnings (On, "unused assignment");
      Next_State := S_Terminated;
   end Finalize;

   procedure Tick is
   begin
      case Next_State is
         when S_Start =>
            Start (Next_State);
         when S_Reply =>
            Reply (Next_State);
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
