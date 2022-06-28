pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal.Options;

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   use type RFLX.RFLX_Types.Bit_Length;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      function Start_Invariant return Boolean is
        (Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Start_Invariant);
      --  tests/integration/session_sequence_append/test.rflx:16:10
      Universal.Option.Verify_Message (Ctx.P.Option_Ctx);
      Ctx.P.Next_State := S_Process;
      pragma Assert (Start_Invariant);
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Options_Ctx : Universal.Options.Context;
      Options_Buffer : RFLX_Types.Bytes_Ptr;
      function Process_Invariant return Boolean is
        (Global_Initialized (Ctx)
         and Universal.Options.Has_Buffer (Options_Ctx)
         and Options_Ctx.Buffer_First = RFLX.RFLX_Types.Index'First
         and Options_Ctx.Buffer_Last >= RFLX.RFLX_Types.Index'First + 4095
         and Ctx.P.Slots.Slot_Ptr_3 = null
         and Ctx.P.Slots.Slot_Ptr_1 = null
         and Ctx.P.Slots.Slot_Ptr_2 = null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
      RFLX_Exception : Boolean := False;
   begin
      Options_Buffer := Ctx.P.Slots.Slot_Ptr_3;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_3 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Options.Initialize (Options_Ctx, Options_Buffer);
      pragma Assert (Process_Invariant);
      --  tests/integration/session_sequence_append/test.rflx:25:10
      if
         not (Universal.Option.Size (Ctx.P.Option_Ctx) <= 32768
          and then Universal.Option.Size (Ctx.P.Option_Ctx) mod RFLX_Types.Byte'Size = 0)
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if
         not Universal.Options.Has_Element (Options_Ctx)
         or Universal.Options.Available_Space (Options_Ctx) < Universal.Option.Field_Size (Ctx.P.Option_Ctx, Universal.Option.F_Data) + 24
      then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      declare
         RFLX_Element_Options_Ctx : Universal.Option.Context;
      begin
         Universal.Options.Switch (Options_Ctx, RFLX_Element_Options_Ctx);
         if Universal.Option.Valid_Next (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type) then
            if Universal.Option.Available_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type) >= Universal.Option.Field_Size (RFLX_Element_Options_Ctx, Universal.Option.F_Option_Type) then
               Universal.Option.Set_Option_Type (RFLX_Element_Options_Ctx, Universal.OT_Data);
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         if Universal.Option.Valid_Next (RFLX_Element_Options_Ctx, Universal.Option.F_Length) then
            if Universal.Option.Available_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Length) >= Universal.Option.Field_Size (RFLX_Element_Options_Ctx, Universal.Option.F_Length) then
               if Universal.Option.Valid (Ctx.P.Option_Ctx, Universal.Option.F_Length) then
                  Universal.Option.Set_Length (RFLX_Element_Options_Ctx, Universal.Option.Get_Length (Ctx.P.Option_Ctx));
               else
                  RFLX_Exception := True;
               end if;
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         if Universal.Option.Valid_Next (RFLX_Element_Options_Ctx, Universal.Option.F_Data) then
            if Universal.Option.Available_Space (RFLX_Element_Options_Ctx, Universal.Option.F_Data) >= Universal.Option.Field_Size (RFLX_Element_Options_Ctx, Universal.Option.F_Data) then
               if Universal.Option.Valid_Next (Ctx.P.Option_Ctx, Universal.Option.F_Data) then
                  if Universal.Option.Valid_Length (RFLX_Element_Options_Ctx, Universal.Option.F_Data, RFLX_Types.To_Length (Universal.Option.Field_Size (Ctx.P.Option_Ctx, Universal.Option.F_Data))) then
                     if Universal.Option.Structural_Valid (Ctx.P.Option_Ctx, Universal.Option.F_Data) then
                        declare
                           pragma Warnings (Off, "is not modified, could be declared constant");
                           RFLX_Ctx_P_Option_Ctx_Tmp : Universal.Option.Context := Ctx.P.Option_Ctx;
                           pragma Warnings (On, "is not modified, could be declared constant");
                           function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
                             (Universal.Option.Has_Buffer (RFLX_Ctx_P_Option_Ctx_Tmp)
                              and then Universal.Option.Structural_Valid (RFLX_Ctx_P_Option_Ctx_Tmp, Universal.Option.F_Data)
                              and then Length = RFLX_Types.To_Length (Universal.Option.Field_Size (RFLX_Ctx_P_Option_Ctx_Tmp, Universal.Option.F_Data)));
                           procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
                             Pre =>
                               RFLX_Process_Data_Pre (Data'Length)
                           is
                           begin
                              Universal.Option.Get_Data (RFLX_Ctx_P_Option_Ctx_Tmp, Data);
                           end RFLX_Process_Data;
                           procedure RFLX_Universal_Option_Set_Data is new Universal.Option.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
                        begin
                           RFLX_Universal_Option_Set_Data (RFLX_Element_Options_Ctx, RFLX_Types.To_Length (Universal.Option.Field_Size (RFLX_Ctx_P_Option_Ctx_Tmp, Universal.Option.F_Data)));
                           Ctx.P.Option_Ctx := RFLX_Ctx_P_Option_Ctx_Tmp;
                        end;
                     else
                        RFLX_Exception := True;
                     end if;
                  else
                     RFLX_Exception := True;
                  end if;
               else
                  RFLX_Exception := True;
               end if;
            else
               RFLX_Exception := True;
            end if;
         else
            RFLX_Exception := True;
         end if;
         pragma Warnings (Off, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
         Universal.Options.Update (Options_Ctx, RFLX_Element_Options_Ctx);
         pragma Warnings (On, """RFLX_Element_Options_Ctx"" is set by ""Update"" but not used after the call");
      end;
      if RFLX_Exception then
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      --  tests/integration/session_sequence_append/test.rflx:29:10
      Universal.Message.Reset (Ctx.P.Message_Ctx);
      if Universal.Message.Valid_Next (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) then
         if Universal.Message.Available_Space (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) >= Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Message_Type) then
            Universal.Message.Set_Message_Type (Ctx.P.Message_Ctx, Universal.MT_Options);
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Assert (Process_Invariant);
            goto Finalize_Process;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if Universal.Message.Valid_Next (Ctx.P.Message_Ctx, Universal.Message.F_Length) then
         if Universal.Message.Available_Space (Ctx.P.Message_Ctx, Universal.Message.F_Length) >= Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Length) then
            Universal.Message.Set_Length (Ctx.P.Message_Ctx, Universal.Length (Universal.Options.Size (Options_Ctx) / 8));
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Assert (Process_Invariant);
            goto Finalize_Process;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      if Universal.Message.Valid_Next (Ctx.P.Message_Ctx, Universal.Message.F_Options) then
         if Universal.Message.Available_Space (Ctx.P.Message_Ctx, Universal.Message.F_Options) >= Universal.Message.Field_Size (Ctx.P.Message_Ctx, Universal.Message.F_Options) then
            if Universal.Message.Valid_Length (Ctx.P.Message_Ctx, Universal.Message.F_Options, Universal.Options.Byte_Size (Options_Ctx)) then
               Universal.Message.Set_Options (Ctx.P.Message_Ctx, Options_Ctx);
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Process_Invariant);
               goto Finalize_Process;
            end if;
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Assert (Process_Invariant);
            goto Finalize_Process;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Process_Invariant);
         goto Finalize_Process;
      end if;
      Ctx.P.Next_State := S_Reply;
      pragma Assert (Process_Invariant);
      <<Finalize_Process>>
      pragma Warnings (Off, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Options.Take_Buffer (Options_Ctx, Options_Buffer);
      pragma Warnings (On, """Options_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_3 := Options_Buffer;
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
         and Ctx.P.Slots.Slot_Ptr_2 = null
         and Ctx.P.Slots.Slot_Ptr_3 /= null)
       with
        Annotate =>
          (GNATprove, Inline_For_Proof),
        Ghost;
   begin
      pragma Assert (Reply_Invariant);
      --  tests/integration/session_sequence_append/test.rflx:40:10
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Reply_Invariant);
   end Reply;

   procedure Initialize (Ctx : in out Context'Class) is
      Option_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Option_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Option.Initialize (Ctx.P.Option_Ctx, Option_Buffer);
      Message_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_Ctx, Message_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Option_Buffer : RFLX_Types.Bytes_Ptr;
      Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Option.Take_Buffer (Ctx.P.Option_Ctx, Option_Buffer);
      pragma Warnings (On, """Ctx.P.Option_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_1 := Option_Buffer;
      pragma Warnings (Off, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Ctx.P.Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_2 := Message_Buffer;
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
            Universal.Option.Reset (Ctx.P.Option_Ctx, Ctx.P.Option_Ctx.First, Ctx.P.Option_Ctx.First - 1);
         when S_Process | S_Reply | S_Terminated =>
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
      procedure Universal_Message_Read is new Universal.Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply =>
                  Universal_Message_Read (Ctx.P.Message_Ctx);
               when others =>
                  null;
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
      procedure Universal_Option_Write is new Universal.Option.Generic_Write (Write, Write_Pre);
   begin
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Start =>
                  Universal_Option_Write (Ctx.P.Option_Ctx, Offset);
               when others =>
                  null;
            end case;
      end case;
   end Write;

end RFLX.Test.Session;
