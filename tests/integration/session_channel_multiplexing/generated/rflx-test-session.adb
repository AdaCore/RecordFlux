pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

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
      if I_1_Has_Data then
         Next_State := S_Reply_1;
      elsif I_2_Has_Data then
         Next_State := S_Reply_2;
      else
         Next_State := S_Terminated;
      end if;
   end Start;

   procedure Reply_1 (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      declare
         procedure Universal_Message_Write is new Universal.Message.Write (I_1_Read);
      begin
         Universal_Message_Write (Message_1_Ctx);
      end;
      Universal.Message.Verify_Message (Message_1_Ctx);
      if Universal.Message.Structural_Valid_Message (Message_1_Ctx) then
         declare
            procedure Universal_Message_Read is new Universal.Message.Read (O_Write);
         begin
            Universal_Message_Read (Message_1_Ctx);
         end;
      else
         Next_State := S_Error;
         return;
      end if;
      Next_State := S_Start;
   end Reply_1;

   procedure Reply_2 (Next_State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      declare
         procedure Universal_Message_Write is new Universal.Message.Write (I_2_Read);
      begin
         Universal_Message_Write (Message_2_Ctx);
      end;
      Universal.Message.Verify_Message (Message_2_Ctx);
      if Universal.Message.Structural_Valid_Message (Message_2_Ctx) then
         declare
            procedure Universal_Message_Read is new Universal.Message.Read (O_Write);
         begin
            Universal_Message_Read (Message_2_Ctx);
         end;
      else
         Next_State := S_Error;
         return;
      end if;
      Next_State := S_Start;
   end Reply_2;

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
      Next_State := S_Start;
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
      Next_State := S_Terminated;
   end Finalize;

   procedure Tick is
   begin
      case Next_State is
         when S_Start =>
            Start (Next_State);
         when S_Reply_1 =>
            Reply_1 (Next_State);
         when S_Reply_2 =>
            Reply_2 (Next_State);
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
