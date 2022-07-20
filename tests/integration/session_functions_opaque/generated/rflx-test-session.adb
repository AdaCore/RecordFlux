pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal;
with RFLX.Universal.Message;
with RFLX.Universal.Options;
with RFLX.Universal.Option;
with RFLX.Universal.Values;

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Bit_Length;

   use type RFLX.RFLX_Types.Length;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      Ctx.P.Next_State := S_Check_Message;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Check_Message (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Valid : Boolean;
      Message_Ctx : Universal.Message.Context;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      function Check_Message_Invariant return Boolean is
        (Universal.Message.Has_Buffer (Message_Ctx)
         and Message_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Message_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_1 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Message_Ctx, Message_Buffer);
      pragma Assert (Check_Message_Invariant);
      --  tests/integration/session_functions_opaque/test.rflx:27:10
      Universal.Message.Reset (Message_Ctx);
      if Universal.Message.Available_Space (Message_Ctx, Universal.Message.F_Message_Type) < 40 then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Check_Message_Invariant);
         goto Finalize_Check_Message;
      end if;
      pragma Assert (Universal.Message.Sufficient_Space (Message_Ctx, Universal.Message.F_Message_Type));
      Universal.Message.Set_Message_Type (Message_Ctx, Universal.MT_Data);
      pragma Assert (Universal.Message.Sufficient_Space (Message_Ctx, Universal.Message.F_Length));
      Universal.Message.Set_Length (Message_Ctx, 2);
      if Universal.Message.Valid_Length (Message_Ctx, Universal.Message.F_Data, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
         pragma Assert (Universal.Message.Sufficient_Space (Message_Ctx, Universal.Message.F_Data));
         Universal.Message.Set_Data (Message_Ctx, (RFLX_Types.Byte'Val (3), RFLX_Types.Byte'Val (4)));
      else
         Ctx.P.Next_State := S_Error;
         pragma Assert (Check_Message_Invariant);
         goto Finalize_Check_Message;
      end if;
      --  tests/integration/session_functions_opaque/test.rflx:29:10
      declare
         RFLX_Check_Size_Arg_1_Message : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         RFLX_Check_Size_Arg_1_Message_Length : constant RFLX_Types.Length := Universal.Message.Byte_Size (Message_Ctx);
      begin
         if not Universal.Message.Structural_Valid_Message (Message_Ctx) then
            Ctx.P.Next_State := S_Error;
            pragma Assert (Check_Message_Invariant);
            goto Finalize_Check_Message;
         end if;
         Universal.Message.Data (Message_Ctx, RFLX_Check_Size_Arg_1_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Check_Size_Arg_1_Message_Length + 1) - 2));
         Check_Size (Ctx, Test.Size (Universal.Message.Size (Message_Ctx)), RFLX_Check_Size_Arg_1_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Check_Size_Arg_1_Message_Length + 1) - 2), Valid);
      end;
      if Valid then
         Ctx.P.Next_State := S_Check_Message_Sequence;
      else
         Ctx.P.Next_State := S_Error;
      end if;
      pragma Assert (Check_Message_Invariant);
      <<Finalize_Check_Message>>
      pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
   end Check_Message;

   procedure Check_Message_Sequence (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Valid : Boolean;
      Message_Sequence_Ctx : Universal.Options.Context;
      Message_Sequence_Buffer : RFLX_Types.Bytes_Ptr;
      function Check_Message_Sequence_Invariant return Boolean is
        (Universal.Options.Has_Buffer (Message_Sequence_Ctx)
         and Message_Sequence_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Message_Sequence_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_1 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      Message_Sequence_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Message_Sequence_Ctx, Message_Sequence_Buffer);
      pragma Assert (Check_Message_Sequence_Invariant);
      --  tests/integration/session_functions_opaque/test.rflx:43:10
      if
         not Universal.Options.Has_Element (Message_Sequence_Ctx)
         or Universal.Options.Available_Space (Message_Sequence_Ctx) < 40
      then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Check_Message_Sequence_Invariant);
         goto Finalize_Check_Message_Sequence;
      end if;
      declare
         RFLX_Element_Message_Sequence_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Message_Sequence_Ctx, RFLX_Element_Message_Sequence_Ctx);
         if Universal.Option.Available_Space (RFLX_Element_Message_Sequence_Ctx, Universal.Option.F_Option_Type) < 40 then
            Ctx.P.Next_State := S_Error;
            pragma Warnings (Off, """RFLX_Element_Message_Sequence_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Message_Sequence_Ctx, RFLX_Element_Message_Sequence_Ctx);
            pragma Warnings (On, """RFLX_Element_Message_Sequence_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Check_Message_Sequence_Invariant);
            goto Finalize_Check_Message_Sequence;
         end if;
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Message_Sequence_Ctx, Universal.Option.F_Option_Type));
         Universal.Option.Set_Option_Type (RFLX_Element_Message_Sequence_Ctx, Universal.OT_Data);
         pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Message_Sequence_Ctx, Universal.Option.F_Length));
         Universal.Option.Set_Length (RFLX_Element_Message_Sequence_Ctx, 2);
         if Universal.Option.Valid_Length (RFLX_Element_Message_Sequence_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (2 * RFLX_Types.Byte'Size)) then
            pragma Assert (Universal.Option.Sufficient_Space (RFLX_Element_Message_Sequence_Ctx, Universal.Option.F_Data));
            Universal.Option.Set_Data (RFLX_Element_Message_Sequence_Ctx, (RFLX_Types.Byte'Val (3), RFLX_Types.Byte'Val (4)));
         else
            Ctx.P.Next_State := S_Error;
            pragma Warnings (Off, """RFLX_Element_Message_Sequence_Ctx"" is set by ""Update"" but not used after the call");
            Universal.Options.Update (Message_Sequence_Ctx, RFLX_Element_Message_Sequence_Ctx);
            pragma Warnings (On, """RFLX_Element_Message_Sequence_Ctx"" is set by ""Update"" but not used after the call");
            pragma Assert (Check_Message_Sequence_Invariant);
            goto Finalize_Check_Message_Sequence;
         end if;
         pragma Warnings (Off, """RFLX_Element_Message_Sequence_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Message_Sequence_Ctx, RFLX_Element_Message_Sequence_Ctx);
         pragma Warnings (On, """RFLX_Element_Message_Sequence_Ctx"" is set by ""Update"" but not used after the call");
      end;
      --  tests/integration/session_functions_opaque/test.rflx:45:10
      declare
         RFLX_Check_Size_Arg_1_Message_Sequence : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         RFLX_Check_Size_Arg_1_Message_Sequence_Length : constant RFLX_Types.Length := Universal.Options.Byte_Size (Message_Sequence_Ctx);
      begin
         if not Universal.Options.Valid (Message_Sequence_Ctx) then
            Ctx.P.Next_State := S_Error;
            pragma Assert (Check_Message_Sequence_Invariant);
            goto Finalize_Check_Message_Sequence;
         end if;
         Universal.Options.Data (Message_Sequence_Ctx, RFLX_Check_Size_Arg_1_Message_Sequence (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Check_Size_Arg_1_Message_Sequence_Length + 1) - 2));
         Check_Size (Ctx, Test.Size (Universal.Options.Size (Message_Sequence_Ctx)), RFLX_Check_Size_Arg_1_Message_Sequence (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Check_Size_Arg_1_Message_Sequence_Length + 1) - 2), Valid);
      end;
      if Valid then
         Ctx.P.Next_State := S_Check_Scalar_Sequence;
      else
         Ctx.P.Next_State := S_Error;
      end if;
      pragma Assert (Check_Message_Sequence_Invariant);
      <<Finalize_Check_Message_Sequence>>
      pragma Warnings (Off, """Message_Sequence_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Message_Sequence_Ctx, Message_Sequence_Buffer);
      pragma Warnings (On, """Message_Sequence_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Sequence_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Sequence_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
   end Check_Message_Sequence;

   procedure Check_Scalar_Sequence (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Valid : Boolean;
      Scalar_Sequence_Ctx : Universal.Values.Context;
      Scalar_Sequence_Buffer : RFLX_Types.Bytes_Ptr;
      function Check_Scalar_Sequence_Invariant return Boolean is
        (Universal.Values.Has_Buffer (Scalar_Sequence_Ctx)
         and Scalar_Sequence_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Scalar_Sequence_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_1 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      Scalar_Sequence_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Values.Initialize (Scalar_Sequence_Ctx, Scalar_Sequence_Buffer);
      pragma Assert (Check_Scalar_Sequence_Invariant);
      --  tests/integration/session_functions_opaque/test.rflx:59:10
      if
         not Universal.Values.Has_Element (Scalar_Sequence_Ctx)
         or Universal.Values.Available_Space (Scalar_Sequence_Ctx) < Universal.Value'Size
      then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Check_Scalar_Sequence_Invariant);
         goto Finalize_Check_Scalar_Sequence;
      end if;
      Universal.Values.Append_Element (Scalar_Sequence_Ctx, 1);
      --  tests/integration/session_functions_opaque/test.rflx:61:10
      if
         not Universal.Values.Has_Element (Scalar_Sequence_Ctx)
         or Universal.Values.Available_Space (Scalar_Sequence_Ctx) < Universal.Value'Size
      then
         Ctx.P.Next_State := S_Error;
         pragma Assert (Check_Scalar_Sequence_Invariant);
         goto Finalize_Check_Scalar_Sequence;
      end if;
      Universal.Values.Append_Element (Scalar_Sequence_Ctx, 2);
      --  tests/integration/session_functions_opaque/test.rflx:63:10
      declare
         RFLX_Check_Size_Arg_1_Scalar_Sequence : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         RFLX_Check_Size_Arg_1_Scalar_Sequence_Length : constant RFLX_Types.Length := Universal.Values.Byte_Size (Scalar_Sequence_Ctx);
      begin
         if not Universal.Values.Valid (Scalar_Sequence_Ctx) then
            Ctx.P.Next_State := S_Error;
            pragma Assert (Check_Scalar_Sequence_Invariant);
            goto Finalize_Check_Scalar_Sequence;
         end if;
         Universal.Values.Data (Scalar_Sequence_Ctx, RFLX_Check_Size_Arg_1_Scalar_Sequence (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Check_Size_Arg_1_Scalar_Sequence_Length + 1) - 2));
         Check_Size (Ctx, Test.Size (Universal.Values.Size (Scalar_Sequence_Ctx)), RFLX_Check_Size_Arg_1_Scalar_Sequence (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Check_Size_Arg_1_Scalar_Sequence_Length + 1) - 2), Valid);
      end;
      if Valid then
         Ctx.P.Next_State := S_Terminated;
      else
         Ctx.P.Next_State := S_Error;
      end if;
      pragma Assert (Check_Scalar_Sequence_Invariant);
      <<Finalize_Check_Scalar_Sequence>>
      pragma Warnings (Off, """Scalar_Sequence_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Values.Take_Buffer (Scalar_Sequence_Ctx, Scalar_Sequence_Buffer);
      pragma Warnings (On, """Scalar_Sequence_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Scalar_Sequence_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Scalar_Sequence_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
   end Check_Scalar_Sequence;

   procedure Error (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Error_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 /= null)
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
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
   begin
      Test.Session_Allocator.Finalize (Ctx.P.Slots);
      Ctx.P.Next_State := S_Terminated;
   end Finalize;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Check_Message =>
            Check_Message (Ctx);
         when S_Check_Message_Sequence =>
            Check_Message_Sequence (Ctx);
         when S_Check_Scalar_Sequence =>
            Check_Scalar_Sequence (Ctx);
         when S_Error =>
            Error (Ctx);
         when S_Terminated =>
            null;
      end case;
   end Tick;

   function In_IO_State (Unused_Ctx : Context'Class) return Boolean is
     (False);

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

end RFLX.Test.Session;
