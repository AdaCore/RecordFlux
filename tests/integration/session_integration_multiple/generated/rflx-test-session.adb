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
      Universal.Message.Verify_Message (M_Ctx);
      P_Next_State := S_Terminated;
   end Start;

   procedure Initialize is
      M_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      M_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (M_Ctx, M_Buffer);
      P_Next_State := S_Start;
   end Initialize;

   procedure Finalize is
      M_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""M_Ctx""");
      pragma Warnings (Off, """M_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (M_Ctx, M_Buffer);
      pragma Warnings (On, """M_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""M_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := M_Buffer;
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
            Universal.Message.Reset (M_Ctx, M_Ctx.First, M_Ctx.First - 1);
         when S_Terminated =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick is
   begin
      case P_Next_State is
         when S_Start =>
            Start (P_Next_State);
         when S_Terminated =>
            null;
      end case;
      Reset_Messages_Before_Write;
   end Tick;

   function In_IO_State return Boolean is
     (P_Next_State in S_Start);

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

   function Needs_Data (Chan : Channel) return Boolean is
     ((case Chan is
          when C_Channel =>
             (case P_Next_State is
                 when S_Start =>
                    True,
                 when others =>
                    False)));

   function Write_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             4096));

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
         when C_Channel =>
            case P_Next_State is
               when S_Start =>
                  Universal_Message_Write (M_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Write;

end RFLX.Test.Session;
