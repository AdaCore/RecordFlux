pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal.Contains;

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.Universal.Message_Type;

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
      -- tests/integration/session_conversion/test.rflx:13:10
      Universal.Message.Verify_Message (Ctx.P.Message_Ctx);
      if
         Universal.Message.Well_Formed_Message (Ctx.P.Message_Ctx)
         and then Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx) = Universal.MT_Data
      then
         Ctx.P.Next_State := S_Process;
      else
         Ctx.P.Next_State := S_Final;
      end if;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Process (Ctx : in out Context'Class) with
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
      -- tests/integration/session_conversion/test.rflx:23:10
      if Universal.Contains.Option_In_Message_Data (Ctx.P.Message_Ctx) then
         Universal.Contains.Copy_Data (Ctx.P.Message_Ctx, Ctx.P.Inner_Message_Ctx);
         Universal.Option.Verify_Message (Ctx.P.Inner_Message_Ctx);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      Ctx.P.Next_State := S_Reply;
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
      -- tests/integration/session_conversion/test.rflx:32:10
      Ctx.P.Next_State := S_Final;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Inner_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Inner_Message_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option.Initialize (Ctx.P.Inner_Message_Ctx, Inner_Message_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Inner_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      pragma Warnings (Off, """Ctx.P.Inner_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option.Take_Buffer (Ctx.P.Inner_Message_Ctx, Inner_Message_Buffer);
      pragma Warnings (On, """Ctx.P.Inner_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (Inner_Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := Inner_Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
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
      procedure Universal_Option_Read is new Universal.Option.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply =>
                  Universal_Option_Read (Ctx.P.Inner_Message_Ctx);
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
