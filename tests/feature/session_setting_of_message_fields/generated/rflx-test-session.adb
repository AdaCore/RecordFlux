pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.Universal.Message_Type;

   use type RFLX.Universal.Length;

   use type RFLX.RFLX_Types.Bit_Length;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      T_0 : Boolean;
      T_1 : Boolean;
      T_2 : Universal.Message_Type;
      T_3 : Boolean;
      T_4 : Boolean;
      T_5 : Universal.Length;
      T_6 : Boolean;
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      -- tests/feature/session_setting_of_message_fields/test.rflx:12:10
      Universal.Message.Verify_Message (Ctx.P.Message_Ctx);
      -- tests/feature/session_setting_of_message_fields/test.rflx:15:16
      T_0 := Universal.Message.Well_Formed_Message (Ctx.P.Message_Ctx);
      -- tests/feature/session_setting_of_message_fields/test.rflx:15:16
      T_1 := T_0;
      -- tests/feature/session_setting_of_message_fields/test.rflx:16:20
      if Universal.Message.Valid (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) then
         T_2 := Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      -- tests/feature/session_setting_of_message_fields/test.rflx:16:20
      T_3 := T_2 = Universal.MT_Data;
      -- tests/feature/session_setting_of_message_fields/test.rflx:15:16
      T_4 := T_1
      and then T_3;
      -- tests/feature/session_setting_of_message_fields/test.rflx:17:20
      if Universal.Message.Valid (Ctx.P.Message_Ctx, Universal.Message.F_Length) then
         T_5 := Universal.Message.Get_Length (Ctx.P.Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Start_Invariant);
         goto Finalize_Start;
      end if;
      -- tests/feature/session_setting_of_message_fields/test.rflx:17:20
      T_6 := T_5 = 1;
      if
         T_4
         and then T_6
      then
         Ctx.P.Next_State := S_Process;
      else
         Ctx.P.Next_State := S_Final;
      end if;
      pragma Assert (Start_Invariant);
      <<Finalize_Start>>
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Local_Message_Ctx : Universal.Message.Context;
      T_7 : RFLX.RFLX_Types.Base_Integer;
      T_8 : Universal.Message_Type;
      T_9 : Universal.Message_Type;
      Local_Message_Buffer : RFLX_Types.Bytes_Ptr;
      function Process_Invariant return Boolean is
        (Global_Initialized (Ctx)
         and Universal.Message.Has_Buffer (Local_Message_Ctx)
         and Local_Message_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Local_Message_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_1 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      Local_Message_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Local_Message_Ctx, Local_Message_Buffer);
      pragma Assert (Process_Invariant);
      -- tests/feature/session_setting_of_message_fields/test.rflx:27:10
      if not Universal.Message.Valid_Next (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type));
      Universal.Message.Set_Message_Type (Ctx.P.Message_Ctx, Universal.MT_Data);
      pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Length));
      Universal.Message.Set_Length (Ctx.P.Message_Ctx, 1);
      if Universal.Message.Valid_Length (Ctx.P.Message_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
         pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Data));
         Universal.Message.Set_Data (Ctx.P.Message_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (65)));
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      -- tests/feature/session_setting_of_message_fields/test.rflx:35:34
      T_7 := RFLX.RFLX_Types.Base_Integer (Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Data));
      -- tests/feature/session_setting_of_message_fields/test.rflx:33:10
      if not Universal.Message.Valid_Next (Local_Message_Ctx, Universal.Message.F_Message_Type) then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      pragma Assert (Universal.Message.Sufficient_Space (Local_Message_Ctx, Universal.Message.F_Message_Type));
      Universal.Message.Set_Message_Type (Local_Message_Ctx, Universal.MT_Data);
      pragma Assert (Universal.Message.Sufficient_Space (Local_Message_Ctx, Universal.Message.F_Length));
      Universal.Message.Set_Length (Local_Message_Ctx, Universal.Length (T_7) / Universal.Length (8));
      declare
         pragma Warnings (Off, "is not modified, could be declared constant");
         RFLX_Ctx_P_Message_Ctx_Tmp : Universal.Message.Context := Ctx.P.Message_Ctx;
         pragma Warnings (On, "is not modified, could be declared constant");
         function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
           (Universal.Message.Has_Buffer (RFLX_Ctx_P_Message_Ctx_Tmp)
            and then Universal.Message.Well_Formed (RFLX_Ctx_P_Message_Ctx_Tmp, Universal.Message.F_Data)
            and then Length = RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Ctx_P_Message_Ctx_Tmp, Universal.Message.F_Data)));
         procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
           Pre =>
             RFLX_Process_Data_Pre (Data'Length)
         is
         begin
            Universal.Message.Get_Data (RFLX_Ctx_P_Message_Ctx_Tmp, Data);
         end RFLX_Process_Data;
         procedure RFLX_Universal_Message_Set_Data is new Universal.Message.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
      begin
         RFLX_Universal_Message_Set_Data (Local_Message_Ctx, RFLX_Types.To_Length (Universal.Message.Field_Size (RFLX_Ctx_P_Message_Ctx_Tmp, Universal.Message.F_Data)));
         Ctx.P.Message_Ctx := RFLX_Ctx_P_Message_Ctx_Tmp;
      end;
      -- tests/feature/session_setting_of_message_fields/test.rflx:40:16
      if Universal.Message.Valid (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) then
         T_8 := Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      -- tests/feature/session_setting_of_message_fields/test.rflx:40:39
      if Universal.Message.Valid (Local_Message_Ctx, Universal.Message.F_Message_Type) then
         T_9 := Universal.Message.Get_Message_Type (Local_Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if T_8 = T_9 then
         Ctx.P.Next_State := S_Reply;
      else
         Ctx.P.Next_State := S_Final;
      end if;
      pragma Assert (Process_Invariant);
      <<Finalize_Process>>
      pragma Warnings (Off, """Local_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Local_Message_Ctx, Local_Message_Buffer);
      pragma Warnings (On, """Local_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (Local_Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := Local_Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
      pragma Assert (Global_Initialized (Ctx));
   end Process;

   procedure Reply (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_Invariant);
      -- tests/feature/session_setting_of_message_fields/test.rflx:48:10
      Ctx.P.Next_State := S_Final;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Final;
   end Finalize;

   procedure Reset_Messages_Before_Write (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Universal.Message.Reset (Ctx.P.Message_Ctx, Ctx.P.Message_Ctx.First, Ctx.P.Message_Ctx.First - 1);
         when S_Process | S_Reply | S_Final =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick (Ctx : in out Context'Class) is
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

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Start | S_Reply);

   procedure Run (Ctx : in out Context'Class) is
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

   procedure Read (Ctx : Context'Class; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
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
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply =>
                  Universal_Message_Read (Ctx.P.Message_Ctx);
               when others =>
                  pragma Warnings (Off, "unreachable code");
                  null;
                  pragma Warnings (On, "unreachable code");
            end case;
      end case;
   end Read;

   procedure Write (Ctx : in out Context'Class; Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
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
                  Universal_Message_Write (Ctx.P.Message_Ctx, Offset);
               when others =>
                  pragma Warnings (Off, "unreachable code");
                  null;
                  pragma Warnings (On, "unreachable code");
            end case;
      end case;
   end Write;

end RFLX.Test.Session;
