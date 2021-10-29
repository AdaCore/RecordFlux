pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal;
use type RFLX.Universal.Message_Type;
use type RFLX.Universal.Length;
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
      declare
         procedure Universal_Message_Write is new Universal.Message.Generic_Write (Channel_Read);
      begin
         Universal_Message_Write (Message_Ctx);
      end;
      Universal.Message.Verify_Message (Message_Ctx);
      if
        (Universal.Message.Structural_Valid_Message (Message_Ctx) = True
         and then Universal.Message.Get_Message_Type (Message_Ctx) = Universal.MT_Data)
        and then Universal.Message.Get_Length (Message_Ctx) = 1
      then
         P_Next_State := S_Reply;
      else
         P_Next_State := S_Terminated;
      end if;
   end Start;

   procedure Reply (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      if Message_Ctx.Last - Message_Ctx.First + 1 >= RFLX_Types.Bit_Length (32) then
         Universal.Message.Reset (Message_Ctx, RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First) + RFLX_Types.Bit_Length (32) - 1);
         Universal.Message.Set_Message_Type (Message_Ctx, Universal.MT_Data);
         Universal.Message.Set_Length (Message_Ctx, 1);
         if Universal.Message.Field_Size (Message_Ctx, Universal.Message.F_Data) = 1 * RFLX_Types.Byte'Size then
            Universal.Message.Set_Data (Message_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
         else
            P_Next_State := S_Terminated;
            return;
         end if;
      else
         P_Next_State := S_Terminated;
         return;
      end if;
      if Universal.Message.Structural_Valid_Message (Message_Ctx) then
         declare
            procedure Universal_Message_Read is new Universal.Message.Generic_Read (Channel_Write);
         begin
            Universal_Message_Read (Message_Ctx);
         end;
      else
         P_Next_State := S_Terminated;
         return;
      end if;
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
