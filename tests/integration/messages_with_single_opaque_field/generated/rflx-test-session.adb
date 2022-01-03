pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

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
   begin
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 = null);
      Test.Message.Verify_Message (Ctx.P.M_R_Ctx);
      Ctx.P.Next_State := S_Process;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 = null);
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 = null);
      if
         Test.Message.Size (Ctx.P.M_R_Ctx) <= 32768
         and then Test.Message.Size (Ctx.P.M_R_Ctx) mod RFLX_Types.Byte'Size = 0
      then
         if RFLX_Types.To_First_Bit_Index (Ctx.P.M_S_Ctx.Buffer_Last) - RFLX_Types.To_First_Bit_Index (Ctx.P.M_S_Ctx.Buffer_First) + 1 >= Test.Message.Field_Size (Ctx.P.M_R_Ctx, Test.Message.F_Data) then
            Test.Message.Reset (Ctx.P.M_S_Ctx, RFLX_Types.To_First_Bit_Index (Ctx.P.M_S_Ctx.Buffer_First), RFLX_Types.To_First_Bit_Index (Ctx.P.M_S_Ctx.Buffer_First) + Test.Message.Field_Size (Ctx.P.M_R_Ctx, Test.Message.F_Data) - 1);
            if Test.Message.Valid_Next (Ctx.P.M_R_Ctx, Test.Message.F_Data) then
               if Test.Message.Valid_Length (Ctx.P.M_S_Ctx, Test.Message.F_Data, RFLX_Types.To_Length (Test.Message.Field_Size (Ctx.P.M_R_Ctx, Test.Message.F_Data))) then
                  if Test.Message.Structural_Valid (Ctx.P.M_R_Ctx, Test.Message.F_Data) then
                     declare
                        pragma Warnings (Off, "is not modified, could be declared constant");
                        RFLX_Ctx_P_M_R_Ctx_Tmp : Test.Message.Context := Ctx.P.M_R_Ctx;
                        pragma Warnings (On, "is not modified, could be declared constant");
                        function RFLX_Process_Data_Pre (Length : RFLX_Types.Length) return Boolean is
                          (Test.Message.Has_Buffer (RFLX_Ctx_P_M_R_Ctx_Tmp)
                           and then Test.Message.Structural_Valid (RFLX_Ctx_P_M_R_Ctx_Tmp, Test.Message.F_Data)
                           and then Length >= RFLX_Types.To_Length (Test.Message.Field_Size (RFLX_Ctx_P_M_R_Ctx_Tmp, Test.Message.F_Data)));
                        procedure RFLX_Process_Data (Data : out RFLX_Types.Bytes) with
                          Pre =>
                            RFLX_Process_Data_Pre (Data'Length)
                        is
                        begin
                           Test.Message.Get_Data (RFLX_Ctx_P_M_R_Ctx_Tmp, Data);
                        end RFLX_Process_Data;
                        procedure RFLX_Test_Message_Set_Data is new Test.Message.Generic_Set_Data (RFLX_Process_Data, RFLX_Process_Data_Pre);
                     begin
                        RFLX_Test_Message_Set_Data (Ctx.P.M_S_Ctx, RFLX_Types.To_Length (Test.Message.Field_Size (RFLX_Ctx_P_M_R_Ctx_Tmp, Test.Message.F_Data)));
                        Ctx.P.M_R_Ctx := RFLX_Ctx_P_M_R_Ctx_Tmp;
                     end;
                  else
                     Ctx.P.Next_State := S_Terminated;
                     pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                                    and Ctx.P.Slots.Slot_Ptr_2 = null);
                     goto Finalize_Process;
                  end if;
               else
                  Ctx.P.Next_State := S_Terminated;
                  pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                                 and Ctx.P.Slots.Slot_Ptr_2 = null);
                  goto Finalize_Process;
               end if;
            else
               Ctx.P.Next_State := S_Terminated;
               pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                              and Ctx.P.Slots.Slot_Ptr_2 = null);
               goto Finalize_Process;
            end if;
         else
            Ctx.P.Next_State := S_Terminated;
            pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                           and Ctx.P.Slots.Slot_Ptr_2 = null);
            goto Finalize_Process;
         end if;
      else
         Ctx.P.Next_State := S_Terminated;
         pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                        and Ctx.P.Slots.Slot_Ptr_2 = null);
         goto Finalize_Process;
      end if;
      Ctx.P.Next_State := S_Reply;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 = null);
      <<Finalize_Process>>
   end Process;

   procedure Reply (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 = null);
      Ctx.P.Next_State := S_Terminated;
      pragma Assert (Ctx.P.Slots.Slot_Ptr_1 = null
                     and Ctx.P.Slots.Slot_Ptr_2 = null);
   end Reply;

   procedure Initialize (Ctx : in out Context'Class) is
      M_R_Buffer : RFLX_Types.Bytes_Ptr;
      M_S_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Test.Session_Allocator.Initialize (Ctx.P.Slots, Ctx.P.Memory);
      M_R_Buffer := Ctx.P.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Test.Message.Initialize (Ctx.P.M_R_Ctx, M_R_Buffer);
      M_S_Buffer := Ctx.P.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.P.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Test.Message.Initialize (Ctx.P.M_S_Ctx, M_S_Buffer);
      Ctx.P.Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      M_R_Buffer : RFLX_Types.Bytes_Ptr;
      M_S_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, """Ctx.P.M_R_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Test.Message.Take_Buffer (Ctx.P.M_R_Ctx, M_R_Buffer);
      pragma Warnings (On, """Ctx.P.M_R_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_1 := M_R_Buffer;
      pragma Warnings (Off, """Ctx.P.M_S_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Test.Message.Take_Buffer (Ctx.P.M_S_Ctx, M_S_Buffer);
      pragma Warnings (On, """Ctx.P.M_S_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Ctx.P.Slots.Slot_Ptr_2 := M_S_Buffer;
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
            Test.Message.Reset (Ctx.P.M_R_Ctx, Ctx.P.M_R_Ctx.First, Ctx.P.M_R_Ctx.First - 1);
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
      procedure Test_Message_Read is new Test.Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Reply =>
                  Test_Message_Read (Ctx.P.M_S_Ctx);
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
      procedure Test_Message_Write is new Test.Message.Generic_Write (Write, Write_Pre);
   begin
      case Chan is
         when C_Channel =>
            case Ctx.P.Next_State is
               when S_Start =>
                  Test_Message_Write (Ctx.P.M_R_Ctx, Offset);
               when others =>
                  null;
            end case;
      end case;
   end Write;

end RFLX.Test.Session;