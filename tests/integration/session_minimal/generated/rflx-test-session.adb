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
      declare
         procedure Universal_Message_Write is new Universal.Message.Write (Channel_Read);
      begin
         Universal_Message_Write (Message_Ctx);
      end;
      Universal.Message.Verify_Message (Message_Ctx);
      if Universal.Message.Structural_Valid_Message (Message_Ctx) then
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
   begin
      if Universal.Message.Structural_Valid_Message (Message_Ctx) then
         declare
            procedure Universal_Message_Read is new Universal.Message.Read (Channel_Write);
         begin
            Universal_Message_Read (Message_Ctx);
         end;
      else
         Next_State := S_Terminated;
         return;
      end if;
      Next_State := S_Terminated;
   end Reply;

   procedure Initialize is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Message_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);
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
      RFLX_Types.Free (Message_Buffer);
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
