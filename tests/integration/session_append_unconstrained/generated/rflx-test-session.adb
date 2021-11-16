pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal.Options;
with RFLX.Universal.Option;
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
      Options_Ctx : Universal.Options.Context;
      RFLX_Exception : Boolean := False;
      Options_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Options_Buffer := Test.Session_Allocator.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Options_Ctx, Options_Buffer);
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < 32
      then
         P_Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
         pragma Warnings (On, "unused assignment");
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
         P_Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < 40
      then
         P_Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
         pragma Warnings (On, "unused assignment");
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
         P_Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < 8
      then
         P_Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
         pragma Warnings (On, "unused assignment");
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
         if RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First) + 1 >= RFLX_Types.Bit_Length (Universal.Options.Size (Options_Ctx) + 8) then
            Universal.Message.Reset (Message_Ctx, RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First) + RFLX_Types.Bit_Length (Universal.Options.Size (Options_Ctx) + 8) - 1);
            Universal.Message.Set_Message_Type (Message_Ctx, Universal.MT_Unconstrained_Options);
            if Universal.Message.Field_Size (Message_Ctx, Universal.Message.F_Options) = Universal.Options.Size (Options_Ctx) then
               Universal.Message.Set_Options (Message_Ctx, Options_Ctx);
            else
               P_Next_State := S_Terminated;
               pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
               pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
               pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
               pragma Warnings (On, "unused assignment to ""Options_Ctx""");
               pragma Warnings (Off, "unused assignment");
               Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
               pragma Warnings (On, "unused assignment");
               return;
            end if;
         else
            P_Next_State := S_Terminated;
            pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
            pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
            pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
            pragma Warnings (On, "unused assignment to ""Options_Ctx""");
            pragma Warnings (Off, "unused assignment");
            Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
            pragma Warnings (On, "unused assignment");
            return;
         end if;
      else
         P_Next_State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
         pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Options_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
         pragma Warnings (On, "unused assignment");
         return;
      end if;
      P_Next_State := S_Reply;
      pragma Warnings (Off, "unused assignment to ""Options_Ctx""");
      pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
      pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Options_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := Options_Buffer;
      pragma Warnings (On, "unused assignment");
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
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      Message_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Message_Ctx, Message_Buffer);
      P_Next_State := S_Start;
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
                    Universal.Message.Structural_Valid_Message (Message_Ctx)
                    and Universal.Message.Byte_Size (Message_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             (case P_Next_State is
                 when S_Reply =>
                    Universal.Message.Byte_Size (Message_Ctx),
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
      procedure Universal_Message_Read is new Universal.Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case P_Next_State is
               when S_Reply =>
                  Universal_Message_Read (Message_Ctx);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Read;

end RFLX.Test.Session;
