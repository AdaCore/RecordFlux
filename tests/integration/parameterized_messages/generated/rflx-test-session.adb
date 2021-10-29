pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Receive (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      Test.Message.Reset (M_Ctx, Length => 2, Extended => False);
      declare
         procedure Test_Message_Write is new Test.Message.Generic_Write (C_Read);
      begin
         Test_Message_Write (M_Ctx);
      end;
      Test.Message.Verify_Message (M_Ctx);
      if Test.Message.Structural_Valid_Message (M_Ctx) then
         Next_State := S_Reply;
      else
         Next_State := S_Terminated;
      end if;
   end Receive;

   procedure Reply (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
      RFLX_Exception : Boolean := False;
   begin
      declare
         RFLX_Message_Ctx : Test.Message.Context;
         RFLX_Message_Buffer : RFLX_Types.Bytes_Ptr;
      begin
         RFLX_Message_Buffer := Test.Session_Allocator.Slot_Ptr_2;
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := null;
         pragma Warnings (On, "unused assignment");
         Test.Message.Initialize (RFLX_Message_Ctx, RFLX_Message_Buffer, Length => Test.Length'First, Extended => Boolean'First);
         if
           Test.Message.Size (M_Ctx) <= 32768
           and then Test.Message.Size (M_Ctx) mod RFLX_Types.Byte'Size = 0
         then
            if RFLX_Message_Ctx.Last - RFLX_Message_Ctx.First + 1 >= RFLX_Types.Bit_Length (RFLX_Types.Bit_Length (M_Ctx.Length) * 8 + 16) then
               Test.Message.Reset (RFLX_Message_Ctx, RFLX_Types.To_First_Bit_Index (RFLX_Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (RFLX_Message_Ctx.Buffer_First) + RFLX_Types.Bit_Length (RFLX_Types.Bit_Length (M_Ctx.Length) * 8 + 16) - 1, Length => M_Ctx.Length, Extended => True);
               if Test.Message.Valid_Next (M_Ctx, Test.Message.F_Data) then
                  if Test.Message.Field_Size (RFLX_Message_Ctx, Test.Message.F_Data) = Test.Message.Field_Size (M_Ctx, Test.Message.F_Data) then
                     if Test.Message.Structural_Valid (M_Ctx, Test.Message.F_Data) then
                        Test.Message.Set_Data (RFLX_Message_Ctx, Test.Message.Get_Data (M_Ctx));
                        if Test.Message.Field_Size (RFLX_Message_Ctx, Test.Message.F_Extension) = 2 * RFLX_Types.Byte'Size then
                           Test.Message.Set_Extension (RFLX_Message_Ctx, (RFLX_Types.Byte'Val (3), RFLX_Types.Byte'Val (4)));
                        else
                           RFLX_Exception := True;
                        end if;
                     else
                        RFLX_Exception := True;
                     end if;
                  else
                     RFLX_Exception := True;
                  end if;
               else
                  RFLX_Exception := True;
               end if;
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         if Test.Message.Structural_Valid_Message (RFLX_Message_Ctx) then
            declare
               procedure Test_Message_Read is new Test.Message.Generic_Read (C_Write);
            begin
               Test_Message_Read (RFLX_Message_Ctx);
            end;
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, "unused assignment to ""RFLX_Message_Ctx""");
         pragma Warnings (Off, """RFLX_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Test.Message.Take_Buffer (RFLX_Message_Ctx, RFLX_Message_Buffer);
         pragma Warnings (On, """RFLX_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""RFLX_Message_Ctx""");
         pragma Warnings (Off, "unused assignment");
         Test.Session_Allocator.Slot_Ptr_2 := RFLX_Message_Buffer;
         pragma Warnings (On, "unused assignment");
      end;
      if RFLX_Exception then
         Next_State := S_Error;
         return;
      end if;
      Next_State := S_Terminated;
   end Reply;

   procedure Error (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      Next_State := S_Terminated;
   end Error;

   procedure Initialize is
      M_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      M_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Test.Message.Initialize (M_Ctx, M_Buffer, Length => Test.Length'First, Extended => Boolean'First);
      Next_State := S_Receive;
   end Initialize;

   procedure Finalize is
      M_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""M_Ctx""");
      pragma Warnings (Off, """M_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Test.Message.Take_Buffer (M_Ctx, M_Buffer);
      pragma Warnings (On, """M_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""M_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := M_Buffer;
      pragma Warnings (On, "unused assignment");
      Next_State := S_Terminated;
   end Finalize;

   procedure Tick is
   begin
      case Next_State is
         when S_Receive =>
            Receive (Next_State);
         when S_Reply =>
            Reply (Next_State);
         when S_Error =>
            Error (Next_State);
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
