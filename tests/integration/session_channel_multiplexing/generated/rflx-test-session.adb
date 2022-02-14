pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Bit_Length;

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Universal.Message.Verify_Message (Ctx.P.Message_1_Ctx);
      Universal.Message.Verify_Message (Ctx.P.Message_2_Ctx);
      if Universal.Message.Byte_Size (Ctx.P.Message_1_Ctx) > 0 then
         Ctx.P.Next_State := S_Reply_1;
      elsif Universal.Message.Byte_Size (Ctx.P.Message_2_Ctx) > 0 then
         Ctx.P.Next_State := S_Reply_2;
      else
         Ctx.P.Next_State := S_Terminated;
      end if;
   end Start;

   procedure Reply_1 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Ctx.P.Next_State := S_Start;
   end Reply_1;

   procedure Reply_2 (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Ctx.P.Next_State := S_Start;
   end Reply_2;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_1_Buffer : RFLX_Types.Bytes_Ptr;
      Message_2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      Message_1_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_1_Ctx, Message_1_Buffer);
      Message_2_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.P.Message_2_Ctx, Message_2_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_1_Buffer : RFLX_Types.Bytes_Ptr;
      Message_2_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.Message_1_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_1_Ctx, Message_1_Buffer);
      pragma Warnings (On, """Ctx.P.Message_1_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_1 := Message_1_Buffer;
      pragma Warnings (Off, """Ctx.P.Message_2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.P.Message_2_Ctx, Message_2_Buffer);
      pragma Warnings (On, """Ctx.P.Message_2_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_2 := Message_2_Buffer;
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
            Universal.Message.Reset (Ctx.P.Message_1_Ctx, Ctx.P.Message_1_Ctx.First, Ctx.P.Message_1_Ctx.First - 1);
            Universal.Message.Reset (Ctx.P.Message_2_Ctx, Ctx.P.Message_2_Ctx.First, Ctx.P.Message_2_Ctx.First - 1);
         when S_Reply_1 | S_Reply_2 | S_Terminated =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.P.Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Reply_1 =>
            Reply_1 (Ctx);
         when S_Reply_2 =>
            Reply_2 (Ctx);
         when S_Terminated =>
            null;
      end case;
      Reset_Messages_Before_Write (Ctx);
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State in S_Start | S_Reply_1 | S_Reply_2);

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
         when C_I_1 | C_I_2 =>
            raise Program_Error;
         when C_O =>
            case Ctx.P.Next_State is
               when S_Reply_1 =>
                  Universal_Message_Read (Ctx.P.Message_1_Ctx);
               when S_Reply_2 =>
                  Universal_Message_Read (Ctx.P.Message_2_Ctx);
               when others =>
                  raise Program_Error;
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
         when C_I_1 =>
            case Ctx.P.Next_State is
               when S_Start =>
                  Universal_Message_Write (Ctx.P.Message_1_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
         when C_I_2 =>
            case Ctx.P.Next_State is
               when S_Start =>
                  Universal_Message_Write (Ctx.P.Message_2_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
         when C_O =>
            raise Program_Error;
      end case;
   end Write;

end RFLX.Test.Session;
