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
         and Ctx.P.Slots.Slot_Ptr_2 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      --  tests/integration/session_integration/test.rflx:12:10
      Universal.Message.Verify_Message (Ctx.P.Message_Ctx);
      if
         (Universal.Message.Structural_Valid_Message (Ctx.P.Message_Ctx) = True
          and then Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx) = Universal.MT_Data)
         and then Universal.Message.Get_Length (Ctx.P.Message_Ctx) = 1
      then
         Ctx.P.Next_State := S_Prepare_Message;
      else
         Ctx.P.Next_State := S_Final;
      end if;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Prepare_Message (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Prepare_Message_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Prepare_Message_Invariant);
      --  tests/integration/session_integration/test.rflx:23:10
      Universal.Message.Reset (Ctx.P.Message_Ctx);
      if Universal.Message.Available_Space (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) < 32 then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Prepare_Message_Invariant);
         goto Finalize_Prepare_Message;
      end if;
      pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type));
      Universal.Message.Set_Message_Type (Ctx.P.Message_Ctx, Universal.MT_Data);
      pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Length));
      Universal.Message.Set_Length (Ctx.P.Message_Ctx, 1);
      if Universal.Message.Valid_Length (Ctx.P.Message_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
         pragma Assert (Universal.Message.Sufficient_Space (Ctx.P.Message_Ctx, Universal.Message.F_Data));
         Universal.Message.Set_Data (Ctx.P.Message_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Prepare_Message_Invariant);
         goto Finalize_Prepare_Message;
      end if;
      Ctx.P.Next_State := S_Reply;
      pragma Assert (Prepare_Message_Invariant);
      <<Finalize_Prepare_Message>>
   end Prepare_Message;

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
      --  tests/integration/session_integration/test.rflx:33:10
      Ctx.P.Next_State := S_Next;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Next (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      M_Ctx : Universal.Message.Context;
      M_Buffer : RFLX_Types.Bytes_Ptr;
      function Next_Invariant return Boolean is
        (Global_Initialized (Ctx)
         and Universal.Message.Has_Buffer (M_Ctx)
         and M_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and M_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 8191
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_1 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      M_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (M_Ctx, M_Buffer);
      pragma Assert (Next_Invariant);
      --  tests/integration/session_integration/test.rflx:41:10
      Universal.Message.Reset (M_Ctx);
      if Universal.Message.Available_Space (M_Ctx, Universal.Message.F_Message_Type) < 32 then
         Ctx.P.Next_State := S_Final;
         pragma Assert (Next_Invariant);
         goto Finalize_Next;
      end if;
      pragma Assert (Universal.Message.Sufficient_Space (M_Ctx, Universal.Message.F_Message_Type));
      Universal.Message.Set_Message_Type (M_Ctx, Universal.MT_Data);
      pragma Assert (Universal.Message.Sufficient_Space (M_Ctx, Universal.Message.F_Length));
      Universal.Message.Set_Length (M_Ctx, 1);
      if Universal.Message.Valid_Length (M_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (1 * RFLX_Types.Byte'Size)) then
         pragma Assert (Universal.Message.Sufficient_Space (M_Ctx, Universal.Message.F_Data));
         Universal.Message.Set_Data (M_Ctx, (RFLX_Types.Index'First => RFLX_Types.Byte'Val (2)));
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Next_Invariant);
         goto Finalize_Next;
      end if;
      Ctx.P.Next_State := S_Final;
      pragma Assert (Next_Invariant);
      <<Finalize_Next>>
      pragma Warnings (Off, """M_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (M_Ctx, M_Buffer);
      pragma Warnings (On, """M_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (M_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := M_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 /= null);
      pragma Assert (Global_Initialized (Ctx));
   end Next;

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
         when S_Prepare_Message | S_Reply | S_Next | S_Final =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Prepare_Message =>
            Prepare_Message (Ctx);
         when S_Reply =>
            Reply (Ctx);
         when S_Next =>
            Next (Ctx);
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
