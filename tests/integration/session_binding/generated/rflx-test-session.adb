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
        and then Universal.Message.Get_Length (Message_Ctx) = 1
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
      RFLX_Exception : Boolean := False;
   begin
      declare
         MT : Universal.Message_Type;
      begin
         MT := Universal.MT_Data;
         declare
            Length : Universal.Length;
         begin
            Length := 1;
            declare
               Opaque : constant RFLX_Types.Bytes := (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)) with
                 Size =>
                   1 * RFLX_Types.Byte'Size;
            begin
               if Message_Ctx.Last - Message_Ctx.First + 1 >= RFLX_Types.Bit_Length (Opaque'Size + 24) then
                  Universal.Message.Reset (Message_Ctx, RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Message_Ctx.Buffer_First) + RFLX_Types.Bit_Length (Opaque'Size + 24) - 1);
                  Universal.Message.Set_Message_Type (Message_Ctx, MT);
                  Universal.Message.Set_Length (Message_Ctx, Length);
                  if Universal.Message.Field_Size (Message_Ctx, Universal.Message.F_Data) = Opaque'Size then
                     Universal.Message.Set_Data (Message_Ctx, Opaque);
                  else
                     RFLX_Exception := True;
                  end if;
               else
                  RFLX_Exception := True;
               end if;
            end;
         end;
      end;
      if RFLX_Exception then
         State := S_Terminated;
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
         return;
      end if;
      State := S_Terminated;
   end Reply;

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
