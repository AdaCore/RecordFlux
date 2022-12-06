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
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      -- tests/features/session_functions/test.rflx:38:10
      Universal.Message.Verify_Message (Ctx.P.Message_Ctx);
      if
         (Universal.Message.Well_Formed_Message (Ctx.P.Message_Ctx)
          and then Universal.Message.Get_Message_Type (Ctx.P.Message_Ctx) = Universal.MT_Data)
         and then Universal.Message.Get_Length (Ctx.P.Message_Ctx) = 3
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
      Valid : Test.Result;
      Message_Type : Universal.Option_Type;
      Length : Test.Length;
      function Process_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Process_Invariant);
      -- tests/features/session_functions/test.rflx:52:10
      Get_Message_Type (Ctx, Message_Type);
      -- tests/features/session_functions/test.rflx:53:10
      Valid_Message (Ctx, Message_Type, True, Valid);
      -- tests/features/session_functions/test.rflx:54:10
      if Universal.Message.Well_Formed (Ctx.P.Message_Ctx, Universal.Message.F_Data) then
         Length := Test.Length (Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Data) / 8);
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      -- tests/features/session_functions/test.rflx:56:10
      if Universal.Message.Well_Formed (Ctx.P.Message_Ctx, Universal.Message.F_Data) then
         declare
            Definite_Message : Test.Definite_Message.Structure;
            RFLX_Create_Message_Arg_2_Message : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
            RFLX_Create_Message_Arg_2_Message_Length : constant RFLX_Types.Length := RFLX_Types.To_Length (Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Data)) + 1;
         begin
            Universal.Message.Get_Data (Ctx.P.Message_Ctx, RFLX_Create_Message_Arg_2_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Create_Message_Arg_2_Message_Length) - 2));
            Create_Message (Ctx, Message_Type, Length, RFLX_Create_Message_Arg_2_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Create_Message_Arg_2_Message_Length) - 2), Definite_Message);
            if Test.Definite_Message.Valid_Structure (Definite_Message) then
               if Test.Definite_Message.Sufficient_Buffer_Length (Ctx.P.Definite_Message_Ctx, Definite_Message) then
                  Test.Definite_Message.To_Context (Definite_Message, Ctx.P.Definite_Message_Ctx);
               else
                  Ctx.P.Next_State := S_Final;
                  pragma Assert (Process_Invariant);
                  goto Finalize_Process;
               end if;
            else
               Ctx.P.Next_State := S_Final;
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
         end;
      else
         Ctx.P.Next_State := S_Final;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if Valid = M_Valid then
         Ctx.P.Next_State := S_Reply;
      else
         Ctx.P.Next_State := S_Final;
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
      -- tests/features/session_functions/test.rflx:67:10
      Ctx.P.Next_State := S_Process_2;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Process_2 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Length : Test.Length;
      function Process_2_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Process_2_Invariant);
      -- tests/features/session_functions/test.rflx:75:10
      Length := Test.Length (Universal.Message.Size (Ctx.P.Message_Ctx) / 8);
      -- tests/features/session_functions/test.rflx:77:10
      declare
         Definite_Message : Test.Definite_Message.Structure;
         RFLX_Create_Message_Arg_2_Message : RFLX_Types.Bytes (RFLX_Types.Index'First .. RFLX_Types.Index'First + 4095) := (others => 0);
         RFLX_Create_Message_Arg_2_Message_Length : constant RFLX_Types.Length := Universal.Message.Byte_Size (Ctx.P.Message_Ctx);
      begin
         if not Universal.Message.Well_Formed_Message (Ctx.P.Message_Ctx) then
            Ctx.P.Next_State := S_Final;
            pragma Assert (Process_2_Invariant);
            goto Finalize_Process_2;
         end if;
         Universal.Message.Data (Ctx.P.Message_Ctx, RFLX_Create_Message_Arg_2_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Create_Message_Arg_2_Message_Length + 1) - 2));
         Create_Message (Ctx, (Known => True, Enum => Universal.OT_Data), Length, RFLX_Create_Message_Arg_2_Message (RFLX_Types.Index'First .. RFLX_Types.Index'First + RFLX_Types.Index (RFLX_Create_Message_Arg_2_Message_Length + 1) - 2), Definite_Message);
         if Test.Definite_Message.Valid_Structure (Definite_Message) then
            if Test.Definite_Message.Sufficient_Buffer_Length (Ctx.P.Definite_Message_Ctx, Definite_Message) then
               Test.Definite_Message.To_Context (Definite_Message, Ctx.P.Definite_Message_Ctx);
            else
               Ctx.P.Next_State := S_Final;
               pragma Assert (Process_2_Invariant);
               goto Finalize_Process_2;
            end if;
         else
            Ctx.P.Next_State := S_Final;
            pragma Assert (Process_2_Invariant);
            goto Finalize_Process_2;
         end if;
      end;
      Ctx.P.Next_State := S_Reply_2;
      pragma Assert (Process_2_Invariant);
      <<Finalize_Process_2>>
   end Process_2;

   procedure Reply_2 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Reply_2_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_2_Invariant);
      -- tests/features/session_functions/test.rflx:86:10
      Ctx.P.Next_State := S_Final;
      pragma Assert (Reply_2_Invariant);
   end Reply_2;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Definite_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Definite_Message_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Test.Definite_Message.Initialize (Ctx.P.Definite_Message_Ctx, Definite_Message_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Definite_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null);
      pragma Assert (Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 /= null);
      pragma Warnings (Off, """Ctx.P.Definite_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Test.Definite_Message.Take_Buffer (Ctx.P.Definite_Message_Ctx, Definite_Message_Buffer);
      pragma Warnings (On, """Ctx.P.Definite_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Assert (Ctx.P.Slots.Slot_Ptr_2 = null);
      pragma Assert (Definite_Message_Buffer /= null);
      Ctx.P.Slots.Slot_Ptr_2 := Definite_Message_Buffer;
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
         when S_Process | S_Reply | S_Process_2 | S_Reply_2 | S_Final =>
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
         when S_Process_2 =>
            Process_2 (Ctx);
         when S_Reply_2 =>
            Reply_2 (Ctx);
         when S_Final =>
            null;
      end case;
      Reset_Messages_Before_Write (Ctx);
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Start | S_Reply | S_Reply_2);

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
      procedure Test_Definite_Message_Read is new Test.Definite_Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply | S_Reply_2 =>
                  Test_Definite_Message_Read (Ctx.P.Definite_Message_Ctx);
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
