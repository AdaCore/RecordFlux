pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
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
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      --  tests/integration/session_message_optimization/test.rflx:27:10
      Universal.Message.Verify_Message (Ctx.P.Message_Ctx);
      if
         (Universal.Message.Structural_Valid_Message (Ctx.P.Message_Ctx)
          and then Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx) = Universal.MT_Data)
         and then Universal.Message.Get_Length (Ctx.P.Message_Ctx) = 3
      then
         Ctx.P.Next_State := S_Process;
      else
         Ctx.P.Next_State := S_Terminated;
      end if;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Local_Length : Universal.Length;
      Option_Data : Test.Option_Data.Structure;
      function Process_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Process_Invariant);
      --  tests/integration/session_message_optimization/test.rflx:41:10
      if Universal.Message.Structural_Valid (Ctx.P.Message_Ctx, Universal.Message.F_Data) then
         declare
            RFLX_Get_Option_Data_Arg_0_Message : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
            RFLX_Get_Option_Data_Arg_0_Message_Length : constant RFLX_Types.Length := RFLX_Types.To_Length (Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Data)) + 1;
         begin
            Universal.Message.Get_Data (Ctx.P.Message_Ctx, RFLX_Get_Option_Data_Arg_0_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Get_Option_Data_Arg_0_Message_Length) - 2));
            Get_Option_Data (Ctx, RFLX_Get_Option_Data_Arg_0_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Get_Option_Data_Arg_0_Message_Length) - 2), Option_Data);
            if not Test.Option_Data.Valid_Structure (Option_Data) then
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      --  tests/integration/session_message_optimization/test.rflx:43:10
      Local_Length := Option_Data.Length;
      --  tests/integration/session_message_optimization/test.rflx:45:10
      Universal.Option.Reset (Ctx.P.Option_Ctx);
      --  tests/integration/session_message_optimization/test.rflx:47:10
      if not Universal.Option.Valid_Next (Ctx.P.Option_Ctx, Universal.Option.F_Option_Type) then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if Universal.Option.Available_Space (Ctx.P.Option_Ctx, Universal.Option.F_Option_Type) < Test.Option_Data.Field_Size_Data (Option_Data) + 24 then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      pragma Assert (Universal.Option.Sufficient_Space (Ctx.P.Option_Ctx, Universal.Option.F_Option_Type));
      Universal.Option.Set_Option_Type (Ctx.P.Option_Ctx, Universal.OT_Data);
      Universal.Option.Set_Length (Ctx.P.Option_Ctx, Option_Data.Length);
      if Universal.Option.Valid_Length (Ctx.P.Option_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (Test.Option_Data.Field_Size_Data (Option_Data))) then
         declare
            function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
              (Test.Option_Data.Valid_Structure (Option_Data)
               and then Length = RFLX_Types.To_Length (Test.Option_Data.Field_Size_Data (Option_Data)));
            procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
              Pre =>
                RFLX_Process_Data_Pre (Data'Length)
            is
            begin
               Data := Option_Data.Data (Option_Data.Data'First .. Option_Data.Data'First + Data'Length - 1);
            end RFLX_Process_Data;
            procedure RFLX_Universal_Option_Set_Data is new Universal.Option.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
         begin
            RFLX_Universal_Option_Set_Data (Ctx.P.Option_Ctx, RFLX_Types.To_Length (Test.Option_Data.Field_Size_Data (Option_Data)));
         end;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if Local_Length > 0 then
         Ctx.P.Next_State := S_Reply;
      else
         Ctx.P.Next_State := S_Terminated;
      end if;
      pragma Assert (Process_Invariant);
      <<Finalize_Process>>
   end Process;

   procedure Reply (Ctx : in out Context'Class) with
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
      --  tests/integration/session_message_optimization/test.rflx:62:10
      Ctx.P.Next_State := S_Trigger_Error;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Trigger_Error (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Null_Option_Data : Test.Option_Data.Structure;
      function Trigger_Error_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Trigger_Error_Invariant);
      --  tests/integration/session_message_optimization/test.rflx:71:10
      Get_Option_Data (Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (0)), Null_Option_Data);
      if not Test.Option_Data.Valid_Structure (Null_Option_Data) then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Trigger_Error_Invariant);
         goto Finalize_Trigger_Error;
      end if;
      --  tests/integration/session_message_optimization/test.rflx:73:10
      if not Universal.Option.Valid_Next (Ctx.P.Option_Ctx, Universal.Option.F_Length) then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Trigger_Error_Invariant);
         goto Finalize_Trigger_Error;
      end if;
      if not Universal.Option.Sufficient_Space (Ctx.P.Option_Ctx, Universal.Option.F_Length) then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Trigger_Error_Invariant);
         goto Finalize_Trigger_Error;
      end if;
      Universal.Option.Set_Length (Ctx.P.Option_Ctx, Null_Option_Data.Length);
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Trigger_Error_Invariant);
      <<Finalize_Trigger_Error>>
   end Trigger_Error;

   procedure Error (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Error_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Error_Invariant);
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Error_Invariant);
   end Error;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Option_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Option_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option.Initialize (Ctx.P.Option_Ctx, Option_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Option_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      pragma Warnings (Off, """Ctx.P.Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option.Take_Buffer (Ctx.P.Option_Ctx, Option_Buffer);
      pragma Warnings (On, """Ctx.P.Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (Option_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := Option_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Terminated;
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
         when S_Process | S_Reply | S_Trigger_Error | S_Error | S_Terminated =>
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
         when S_Trigger_Error =>
            Trigger_Error (Ctx);
         when S_Error =>
            Error (Ctx);
         when S_Terminated =>
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
      procedure Universal_Option_Read is new Universal.Option.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply =>
                  Universal_Option_Read (Ctx.P.Option_Ctx);
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