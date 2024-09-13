------------------------------------------------------------------------------
--                                                                          --
--                         Generated by RecordFlux                          --
--                                                                          --
--                          Copyright (C) AdaCore                           --
--                                                                          --
--         SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception          --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Streams);
pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types.Operators;

package body RFLX.Test.S.FSM with
  SPARK_Mode
is

   use RFLX.RFLX_Types.Operators;

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Bit_Length;

   procedure Start (Ctx : in out Context) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      -- tests/feature/messages_with_implict_size/test.rflx:13:10
      Universal.Message.Verify_Message (Ctx.P.M_R_Ctx);
      Ctx.P.Next_State := S_Process;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Process (Ctx : in out Context) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Process_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Process_Invariant);
      -- tests/feature/messages_with_implict_size/test.rflx:20:95
      pragma Warnings (Off, "condition can only be False if invalid values present");
      pragma Warnings (Off, "condition is always False");
      pragma Warnings (Off, "this code can never be executed and has been deleted");
      pragma Warnings (Off, "statement has no effect");
      pragma Warnings (Off, "this statement is never reached");
      if not Universal.Message.Well_Formed (Ctx.P.M_R_Ctx, Universal.Message.F_Data) then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      pragma Warnings (On, "this statement is never reached");
      pragma Warnings (On, "statement has no effect");
      pragma Warnings (On, "this code can never be executed and has been deleted");
      pragma Warnings (On, "condition is always False");
      pragma Warnings (On, "condition can only be False if invalid values present");
      -- tests/feature/messages_with_implict_size/test.rflx:20:10
      Universal.Message.Reset (Ctx.P.M_S_Ctx);
      if not Universal.Message.Sufficient_Space (Ctx.P.M_S_Ctx, Universal.Message.F_Message_Type) then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if not RFLX.Universal.Message.Field_Condition (Ctx.P.M_S_Ctx, RFLX.Universal.Message.F_Message_Type, Universal.To_Base_Integer (Universal.MT_Unconstrained_Data)) then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      Universal.Message.Set_Message_Type (Ctx.P.M_S_Ctx, Universal.MT_Unconstrained_Data);
      declare
         pragma Warnings (Off, "is not modified, could be declared constant");
         RFLX_Ctx_P_M_R_Ctx_Tmp : Universal.Message.Context := Ctx.P.M_R_Ctx;
         pragma Warnings (On, "is not modified, could be declared constant");
         function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
           (Universal.Message.Has_Buffer (RFLX_Ctx_P_M_R_Ctx_Tmp)
            and then Universal.Message.Well_Formed (RFLX_Ctx_P_M_R_Ctx_Tmp, Universal.Message.F_Data)
            and then Length = RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Ctx_P_M_R_Ctx_Tmp, Universal.Message.F_Data)));
         procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
           Pre =>
             RFLX_Process_Data_Pre (Data'Length)
         is
         begin
            Universal.Message.Get_Data (RFLX_Ctx_P_M_R_Ctx_Tmp, Data);
         end RFLX_Process_Data;
         procedure RFLX_Universal_Message_Set_Data is new Universal.Message.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
      begin
         if
            not (Universal.Message.Valid_Next (Ctx.P.M_S_Ctx, Universal.Message.F_Data)
             and Universal.Message.Available_Space (Ctx.P.M_S_Ctx, Universal.Message.F_Data) >= RFLX_Types.To_Bit_Length (RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Ctx_P_M_R_Ctx_Tmp, Universal.Message.F_Data))))
         then
            Ctx.P.Next_State := S_Final;
            Ctx.P.M_R_Ctx := RFLX_Ctx_P_M_R_Ctx_Tmp;
            pragma Assert (Process_Invariant);
            goto Finalize_Process;
         end if;
         if not Universal.Message.Valid_Length (Ctx.P.M_S_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Ctx_P_M_R_Ctx_Tmp, Universal.Message.F_Data))) then
            Ctx.P.Next_State := S_Final;
            Ctx.P.M_R_Ctx := RFLX_Ctx_P_M_R_Ctx_Tmp;
            pragma Assert (Process_Invariant);
            goto Finalize_Process;
         end if;
         RFLX_Universal_Message_Set_Data (Ctx.P.M_S_Ctx, RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Ctx_P_M_R_Ctx_Tmp, Universal.Message.F_Data)));
         Ctx.P.M_R_Ctx := RFLX_Ctx_P_M_R_Ctx_Tmp;
      end;
      Ctx.P.Next_State := S_Reply;
      pragma Assert (Process_Invariant);
      <<Finalize_Process>>
   end Process;

   procedure Reply (Ctx : in out Context) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_Invariant);
      -- tests/feature/messages_with_implict_size/test.rflx:29:10
      Ctx.P.Next_State := S_Final;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Initialize (Ctx : in out Context) is
      M_R_Buffer : RFLX_Types.Bytes_Ptr;
      M_S_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.S.FSM_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      M_R_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.M_R_Ctx, M_R_Buffer);
      M_S_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.M_S_Ctx, M_S_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context) is
      M_R_Buffer : RFLX_Types.Bytes_Ptr;
      M_S_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.M_R_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.M_R_Ctx, M_R_Buffer);
      pragma Warnings (On, """Ctx.P.M_R_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (M_R_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := M_R_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      pragma Warnings (Off, """Ctx.P.M_S_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.M_S_Ctx, M_S_Buffer);
      pragma Warnings (On, """Ctx.P.M_S_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (M_S_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := M_S_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
      Test.S.FSM_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Final;
   end Finalize;

   procedure Reset_Messages_Before_Write (Ctx : in out Context) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Universal.Message.Reset (Ctx.P.M_R_Ctx, Ctx.P.M_R_Ctx.First, Ctx.P.M_R_Ctx.First - 1);
         when S_Process | S_Reply | S_Final =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick (Ctx : in out Context) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Process =>
            Process (Ctx);
         when S_Reply =>
            Reply (Ctx);
         when S_Final =>
            null;
      end case;
      Reset_Messages_Before_Write (Ctx);
   end Tick;

   function In_IO_State (Ctx : Context) return Boolean is
     (Ctx.P.Next_State in S_Start | S_Reply);

   procedure Run (Ctx : in out Context) is
   begin
      Tick (Ctx);
      while
         Active (Ctx)
         and not In_IO_State (Ctx)
      loop
         pragma Loop_Invariant (Initialized (Ctx));
         Tick (Ctx);
      end loop;
   end Run;

   procedure Read (Ctx : Context; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      function Read_Pre (Message_Buffer : RFLX_Types.Bytes) return Boolean is
        (Buffer'Length > 0
         and then Offset < Message_Buffer'Length);
      procedure Read (Message_Buffer : RFLX_Types.Bytes) with
        Pre =>
          Read_Pre (Message_Buffer)
      is
         Length : constant RFLX_Types.Length := RFLX_Types.Length'Min (Buffer'Length, Message_Buffer'Length - Offset);
         Buffer_Last : constant RFLX_Types.Index := Buffer'First + (Length - RFLX_Types.Length'(1));
      begin
         Buffer (Buffer'First .. RFLX_Types.Index (Buffer_Last)) := Message_Buffer (RFLX_Types.Index (RFLX_Types.Length (Message_Buffer'First) + Offset) .. Message_Buffer'First + Offset + (Length - RFLX_Types.Length'(1)));
      end Read;
      procedure Universal_Message_Read is new Universal.Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply =>
                  Universal_Message_Read (Ctx.P.M_S_Ctx);
               when others =>
                  pragma Warnings (Off, "unreachable code");
                  null;
                  pragma Warnings (On, "unreachable code");
            end case;
      end case;
   end Read;

   procedure Write (Ctx : in out Context; Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      Write_Buffer_Length : constant RFLX_Types.Length := Write_Buffer_Size (Ctx, Chan);
      function Write_Pre (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is
        (Buffer'Length > 0
         and then Context_Buffer_Length = Write_Buffer_Length
         and then Offset <= RFLX_Types.Length'Last - Buffer'Length
         and then Buffer'Length + Offset <= Write_Buffer_Length);
      procedure Write (Message_Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length; Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) with
        Pre =>
          Write_Pre (Context_Buffer_Length, Offset)
          and then Offset <= RFLX_Types.Length'Last - Message_Buffer'Length
          and then Message_Buffer'Length + Offset = Write_Buffer_Length,
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
            case Ctx.P.Next_State is
               when S_Start =>
                  Universal_Message_Write (Ctx.P.M_R_Ctx, Offset);
               when others =>
                  pragma Warnings (Off, "unreachable code");
                  null;
                  pragma Warnings (On, "unreachable code");
            end case;
      end case;
   end Write;

end RFLX.Test.S.FSM;