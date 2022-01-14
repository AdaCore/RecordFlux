pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
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
   begin
      Universal.Message.Verify_Message (Message_1_Ctx);
      Universal.Message.Verify_Message (Message_2_Ctx);
      if Universal.Message.Byte_Size (Message_1_Ctx) > 0 then
         P_Next_State := S_Reply_1;
      elsif Universal.Message.Byte_Size (Message_2_Ctx) > 0 then
         P_Next_State := S_Reply_2;
      else
         P_Next_State := S_Terminated;
      end if;
   end Start;

   procedure Reply_1 (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      P_Next_State := S_Start;
   end Reply_1;

   procedure Reply_2 (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      P_Next_State := S_Start;
   end Reply_2;

   procedure Initialize is
      Message_1_Buffer : RFLX_Types.Bytes_Ptr;
      Message_2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      Message_1_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Message_1_Ctx, Message_1_Buffer);
      Message_2_Buffer := Test.Session_Allocator.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Message_2_Ctx, Message_2_Buffer);
      P_Next_State := S_Start;
   end Initialize;

   procedure Finalize is
      Message_1_Buffer : RFLX_Types.Bytes_Ptr;
      Message_2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""Message_1_Ctx""");
      pragma Warnings (Off, """Message_1_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Message_1_Ctx, Message_1_Buffer);
      pragma Warnings (On, """Message_1_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Message_1_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := Message_1_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""Message_2_Ctx""");
      pragma Warnings (Off, """Message_2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Message_2_Ctx, Message_2_Buffer);
      pragma Warnings (On, """Message_2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Message_2_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := Message_2_Buffer;
      pragma Warnings (On, "unused assignment");
      P_Next_State := S_Terminated;
   end Finalize;

   procedure Reset_Messages_Before_Write with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      case P_Next_State is
         when S_Start =>
            Universal.Message.Reset (Message_1_Ctx, Message_1_Ctx.First, Message_1_Ctx.First - 1);
            Universal.Message.Reset (Message_2_Ctx, Message_2_Ctx.First, Message_2_Ctx.First - 1);
         when S_Reply_1 | S_Reply_2 | S_Terminated =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick is
   begin
      case P_Next_State is
         when S_Start =>
            Start (P_Next_State);
         when S_Reply_1 =>
            Reply_1 (P_Next_State);
         when S_Reply_2 =>
            Reply_2 (P_Next_State);
         when S_Terminated =>
            null;
      end case;
      Reset_Messages_Before_Write;
   end Tick;

   function In_IO_State return Boolean is
     (P_Next_State in S_Start | S_Reply_1 | S_Reply_2);

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
          when C_I_1 | C_I_2 =>
             False,
          when C_O =>
             (case P_Next_State is
                 when S_Reply_1 =>
                    Universal.Message.Structural_Valid_Message (Message_1_Ctx)
                    and Universal.Message.Byte_Size (Message_1_Ctx) > 0,
                 when S_Reply_2 =>
                    Universal.Message.Structural_Valid_Message (Message_2_Ctx)
                    and Universal.Message.Byte_Size (Message_2_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_I_1 | C_I_2 =>
             raise Program_Error,
          when C_O =>
             (case P_Next_State is
                 when S_Reply_1 =>
                    Universal.Message.Byte_Size (Message_1_Ctx),
                 when S_Reply_2 =>
                    Universal.Message.Byte_Size (Message_2_Ctx),
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
         when C_I_1 | C_I_2 =>
            raise Program_Error;
         when C_O =>
            case P_Next_State is
               when S_Reply_1 =>
                  Universal_Message_Read (Message_1_Ctx);
               when S_Reply_2 =>
                  Universal_Message_Read (Message_2_Ctx);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Read;

   function Needs_Data (Chan : Channel) return Boolean is
     ((case Chan is
          when C_I_1 | C_I_2 =>
             (case P_Next_State is
                 when S_Start =>
                    True,
                 when others =>
                    False),
          when C_O =>
             False));

   function Write_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_I_1 | C_I_2 =>
             4096,
          when C_O =>
             0));

   procedure Write (Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      function Write_Pre (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is
        (Buffer'Length > 0
         and then Context_Buffer_Length = Write_Buffer_Size (Chan)
         and then Offset <= RFLX_Types.Length'Last - Buffer'Length
         and then Buffer'Length + Offset <= Write_Buffer_Size (Chan));
      procedure Write (Message_Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length; Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) with
        Pre =>
          Write_Pre (Context_Buffer_Length, Offset)
          and then Offset <= RFLX_Types.Length'Last - Message_Buffer'Length
          and then Message_Buffer'Length + Offset = Write_Buffer_Size (Chan),
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
         when C_I_1 =>
            case P_Next_State is
               when S_Start =>
                  Universal_Message_Write (Message_1_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
         when C_I_2 =>
            case P_Next_State is
               when S_Start =>
                  Universal_Message_Write (Message_2_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
         when C_O =>
            raise Program_Error;
      end case;
   end Write;

end RFLX.Test.Session;
