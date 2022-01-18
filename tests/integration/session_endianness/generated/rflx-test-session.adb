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
      Messages.Msg.Verify_Message (In_Msg_Ctx);
      if Messages.Msg.Byte_Size (In_Msg_Ctx) > 0 then
         P_Next_State := S_Copy;
      else
         P_Next_State := S_Terminated;
      end if;
   end Start;

   procedure Copy (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      if RFLX_Types.To_First_Bit_Index (Out_Msg_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Out_Msg_Ctx.Buffer_First) + 1 >= 64 then
         Messages.Msg_LE.Reset (Out_Msg_Ctx, RFLX_Types.To_First_Bit_Index (Out_Msg_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Out_Msg_Ctx.Buffer_First) + 64 - 1);
         if Messages.Msg.Valid (In_Msg_Ctx, Messages.Msg.F_A) then
            Messages.Msg_LE.Set_C (Out_Msg_Ctx, Messages.Msg.Get_A (In_Msg_Ctx));
            if Messages.Msg.Valid (In_Msg_Ctx, Messages.Msg.F_B) then
               Messages.Msg_LE.Set_D (Out_Msg_Ctx, Messages.Msg.Get_B (In_Msg_Ctx));
            else
               P_Next_State := S_Terminated;
               return;
            end if;
         else
            P_Next_State := S_Terminated;
            return;
         end if;
      else
         P_Next_State := S_Terminated;
         return;
      end if;
      P_Next_State := S_Reply;
   end Copy;

   procedure Reply (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      P_Next_State := S_Read2;
   end Reply;

   procedure Read2 (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      Messages.Msg_LE.Verify_Message (In_Msg2_Ctx);
      if Messages.Msg_LE.Byte_Size (In_Msg2_Ctx) > 0 then
         P_Next_State := S_Copy2;
      else
         P_Next_State := S_Terminated;
      end if;
   end Read2;

   procedure Copy2 (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      if RFLX_Types.To_First_Bit_Index (Out_Msg2_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Out_Msg2_Ctx.Buffer_First) + 1 >= 64 then
         Messages.Msg.Reset (Out_Msg2_Ctx, RFLX_Types.To_First_Bit_Index (Out_Msg2_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Out_Msg2_Ctx.Buffer_First) + 64 - 1);
         if Messages.Msg_LE.Valid (In_Msg2_Ctx, Messages.Msg_LE.F_C) then
            Messages.Msg.Set_A (Out_Msg2_Ctx, Messages.Msg_LE.Get_C (In_Msg2_Ctx));
            if Messages.Msg_LE.Valid (In_Msg2_Ctx, Messages.Msg_LE.F_D) then
               Messages.Msg.Set_B (Out_Msg2_Ctx, Messages.Msg_LE.Get_D (In_Msg2_Ctx));
            else
               P_Next_State := S_Terminated;
               return;
            end if;
         else
            P_Next_State := S_Terminated;
            return;
         end if;
      else
         P_Next_State := S_Terminated;
         return;
      end if;
      P_Next_State := S_Reply2;
   end Copy2;

   procedure Reply2 (P_Next_State : out State) with
     Pre =>
       Initialized,
     Post =>
       Initialized
   is
   begin
      P_Next_State := S_Start;
   end Reply2;

   procedure Initialize is
      In_Msg_Buffer : RFLX_Types.Bytes_Ptr;
      In_Msg2_Buffer : RFLX_Types.Bytes_Ptr;
      Out_Msg_Buffer : RFLX_Types.Bytes_Ptr;
      Out_Msg2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize;
      In_Msg_Buffer := Test.Session_Allocator.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Messages.Msg.Initialize (In_Msg_Ctx, In_Msg_Buffer);
      In_Msg2_Buffer := Test.Session_Allocator.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Messages.Msg_LE.Initialize (In_Msg2_Ctx, In_Msg2_Buffer);
      Out_Msg_Buffer := Test.Session_Allocator.Slot_Ptr_3;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_3 := null;
      pragma Warnings (On, "unused assignment");
      Messages.Msg_LE.Initialize (Out_Msg_Ctx, Out_Msg_Buffer);
      Out_Msg2_Buffer := Test.Session_Allocator.Slot_Ptr_4;
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_4 := null;
      pragma Warnings (On, "unused assignment");
      Messages.Msg.Initialize (Out_Msg2_Ctx, Out_Msg2_Buffer);
      P_Next_State := S_Start;
   end Initialize;

   procedure Finalize is
      In_Msg_Buffer : RFLX_Types.Bytes_Ptr;
      In_Msg2_Buffer : RFLX_Types.Bytes_Ptr;
      Out_Msg_Buffer : RFLX_Types.Bytes_Ptr;
      Out_Msg2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""In_Msg_Ctx""");
      pragma Warnings (Off, """In_Msg_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Messages.Msg.Take_Buffer (In_Msg_Ctx, In_Msg_Buffer);
      pragma Warnings (On, """In_Msg_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""In_Msg_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_1 := In_Msg_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""In_Msg2_Ctx""");
      pragma Warnings (Off, """In_Msg2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Messages.Msg_LE.Take_Buffer (In_Msg2_Ctx, In_Msg2_Buffer);
      pragma Warnings (On, """In_Msg2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""In_Msg2_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_2 := In_Msg2_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""Out_Msg_Ctx""");
      pragma Warnings (Off, """Out_Msg_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Messages.Msg_LE.Take_Buffer (Out_Msg_Ctx, Out_Msg_Buffer);
      pragma Warnings (On, """Out_Msg_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Out_Msg_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_3 := Out_Msg_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""Out_Msg2_Ctx""");
      pragma Warnings (Off, """Out_Msg2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Messages.Msg.Take_Buffer (Out_Msg2_Ctx, Out_Msg2_Buffer);
      pragma Warnings (On, """Out_Msg2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Out_Msg2_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Test.Session_Allocator.Slot_Ptr_4 := Out_Msg2_Buffer;
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
            Messages.Msg.Reset (In_Msg_Ctx, In_Msg_Ctx.First, In_Msg_Ctx.First - 1);
         when S_Copy | S_Reply =>
            null;
         when S_Read2 =>
            Messages.Msg_LE.Reset (In_Msg2_Ctx, In_Msg2_Ctx.First, In_Msg2_Ctx.First - 1);
         when S_Copy2 | S_Reply2 | S_Terminated =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick is
   begin
      case P_Next_State is
         when S_Start =>
            Start (P_Next_State);
         when S_Copy =>
            Copy (P_Next_State);
         when S_Reply =>
            Reply (P_Next_State);
         when S_Read2 =>
            Read2 (P_Next_State);
         when S_Copy2 =>
            Copy2 (P_Next_State);
         when S_Reply2 =>
            Reply2 (P_Next_State);
         when S_Terminated =>
            null;
      end case;
      Reset_Messages_Before_Write;
   end Tick;

   function In_IO_State return Boolean is
     (P_Next_State in S_Start | S_Reply | S_Read2 | S_Reply2);

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
          when C_I =>
             False,
          when C_O =>
             (case P_Next_State is
                 when S_Reply =>
                    Messages.Msg_LE.Structural_Valid_Message (Out_Msg_Ctx)
                    and Messages.Msg_LE.Byte_Size (Out_Msg_Ctx) > 0,
                 when S_Reply2 =>
                    Messages.Msg.Structural_Valid_Message (Out_Msg2_Ctx)
                    and Messages.Msg.Byte_Size (Out_Msg2_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_I =>
             raise Program_Error,
          when C_O =>
             (case P_Next_State is
                 when S_Reply =>
                    Messages.Msg_LE.Byte_Size (Out_Msg_Ctx),
                 when S_Reply2 =>
                    Messages.Msg.Byte_Size (Out_Msg2_Ctx),
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
      procedure Messages_Msg_Read is new Messages.Msg.Generic_Read (Read, Read_Pre);
      procedure Messages_Msg_LE_Read is new Messages.Msg_LE.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_I =>
            raise Program_Error;
         when C_O =>
            case P_Next_State is
               when S_Reply =>
                  Messages_Msg_LE_Read (Out_Msg_Ctx);
               when S_Reply2 =>
                  Messages_Msg_Read (Out_Msg2_Ctx);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Read;

   function Needs_Data (Chan : Channel) return Boolean is
     ((case Chan is
          when C_I =>
             (case P_Next_State is
                 when S_Start | S_Read2 =>
                    True,
                 when others =>
                    False),
          when C_O =>
             False));

   function Write_Buffer_Size (Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_I =>
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
      procedure Messages_Msg_Write is new Messages.Msg.Generic_Write (Write, Write_Pre);
      procedure Messages_Msg_LE_Write is new Messages.Msg_LE.Generic_Write (Write, Write_Pre);
   begin
      case Chan is
         when C_I =>
            case P_Next_State is
               when S_Start =>
                  Messages_Msg_Write (In_Msg_Ctx, Offset);
               when S_Read2 =>
                  Messages_Msg_LE_Write (In_Msg2_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
         when C_O =>
            raise Program_Error;
      end case;
   end Write;

end RFLX.Test.Session;