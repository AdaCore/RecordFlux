pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal;
use type RFLX.Universal.Message_Type;
use type RFLX.Universal.Length;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (State : out Session_State) with
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
        (Universal.Message.Structural_Valid_Message (Message_Ctx)
         and then Universal.Message.Get_Message_Type (Message_Ctx) = Universal.MT_Data)
        and then Universal.Message.Get_Length (Message_Ctx) = 3
      then
         State := S_Reply;
      else
         State := S_Terminated;
      end if;
   end Start;

   procedure Reply (State : out Session_State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
      Message_Type : Universal.Option_Type;
      Fixed_Size_Message_Ctx : Fixed_Size.Simple_Message.Context;
      Fixed_Size_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Fixed_Size_Message_Buffer := new RFLX_Types.Bytes'(RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095 => RFLX_Types.Byte'First);
      Fixed_Size.Simple_Message.Initialize (Fixed_Size_Message_Ctx, Fixed_Size_Message_Buffer);
      Get_Message_Type (Message_Type);
      if Universal.Message.Structural_Valid (Message_Ctx, Universal.Message.F_Data) then
         declare
            Fixed_Size_Message : Fixed_Size.Simple_Message.Structure;
         begin
            Create_Message (Fixed_Size_Message, Message_Type, Universal.Message.Get_Data (Message_Ctx));
            Fixed_Size.Simple_Message.To_Context (Fixed_Size_Message, Fixed_Size_Message_Ctx);
         end;
      else
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Fixed_Size_Message_Ctx""");
         pragma Warnings (Off, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Fixed_Size.Simple_Message.Take_Buffer (Fixed_Size_Message_Ctx, Fixed_Size_Message_Buffer);
         pragma Warnings (On, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Fixed_Size_Message_Ctx""");
         RFLX_Types.Free (Fixed_Size_Message_Buffer);
         return;
      end if;
      if Fixed_Size.Simple_Message.Structural_Valid_Message (Fixed_Size_Message_Ctx) then
         declare
            procedure Fixed_Size_Simple_Message_Read is new Fixed_Size.Simple_Message.Read (Channel_Write);
         begin
            Fixed_Size_Simple_Message_Read (Fixed_Size_Message_Ctx);
         end;
      else
         State := S_Terminated;
         pragma Warnings (Off, "unused assignment to ""Fixed_Size_Message_Ctx""");
         pragma Warnings (Off, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         Fixed_Size.Simple_Message.Take_Buffer (Fixed_Size_Message_Ctx, Fixed_Size_Message_Buffer);
         pragma Warnings (On, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
         pragma Warnings (On, "unused assignment to ""Fixed_Size_Message_Ctx""");
         RFLX_Types.Free (Fixed_Size_Message_Buffer);
         return;
      end if;
      State := S_Terminated;
      pragma Warnings (Off, "unused assignment to ""Fixed_Size_Message_Ctx""");
      pragma Warnings (Off, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Fixed_Size.Simple_Message.Take_Buffer (Fixed_Size_Message_Ctx, Fixed_Size_Message_Buffer);
      pragma Warnings (On, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Fixed_Size_Message_Ctx""");
      RFLX_Types.Free (Fixed_Size_Message_Buffer);
   end Reply;

   procedure Initialize is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
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
         when S_Reply =>
            Reply (State);
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
