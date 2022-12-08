pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.Session_Allocator;
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Options;
with RFLX.Universal.Message;

package RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Length;

   type Channel is (C_Channel);

   type State is (S_Start, S_Process, S_Send_1, S_Send_2, S_Final);

   type Private_Context is private;

   type Context is abstract tagged limited
      record
         P : Private_Context;
      end record;

   function Uninitialized (Ctx : Context'Class) return Boolean;

   function Initialized (Ctx : Context'Class) return Boolean;

   function Active (Ctx : Context'Class) return Boolean;

   procedure Initialize (Ctx : in out Context'Class) with
     Pre =>
       Uninitialized (Ctx),
     Post =>
       Initialized (Ctx)
       and Active (Ctx);

   procedure Finalize (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Uninitialized (Ctx)
       and not Active (Ctx);

   pragma Warnings (Off, "subprogram ""Tick"" has no effect");

   procedure Tick (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Tick"" has no effect");

   function In_IO_State (Ctx : Context'Class) return Boolean;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State (Ctx : Context'Class) return State;

   function Has_Data (Ctx : Context'Class; Chan : Channel) return Boolean with
     Pre =>
       Initialized (Ctx);

   function Read_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length with
     Pre =>
       Initialized (Ctx)
       and then Has_Data (Ctx, Chan);

   procedure Read (Ctx : Context'Class; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized (Ctx)
       and then Has_Data (Ctx, Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Read_Buffer_Size (Ctx, Chan),
     Post =>
       Initialized (Ctx);

private

   type Private_Context is
      record
         Next_State : State := S_Start;
         Options_Ctx : Universal.Options.Context;
         Message_1_Ctx : Universal.Message.Context;
         Message_2_Ctx : Universal.Message.Context;
         Slots : Test.Session_Allocator.Slots;
         Memory : Test.Session_Allocator.Memory;
      end record;

   function Uninitialized (Ctx : Context'Class) return Boolean is
     (not Universal.Options.Has_Buffer (Ctx.P.Options_Ctx)
      and not Universal.Message.Has_Buffer (Ctx.P.Message_1_Ctx)
      and not Universal.Message.Has_Buffer (Ctx.P.Message_2_Ctx)
      and Test.Session_Allocator.Uninitialized (Ctx.P.Slots));

   function Global_Initialized (Ctx : Context'Class) return Boolean is
     (Universal.Options.Has_Buffer (Ctx.P.Options_Ctx)
      and then Ctx.P.Options_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Ctx.P.Options_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then Universal.Message.Has_Buffer (Ctx.P.Message_1_Ctx)
      and then Ctx.P.Message_1_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Ctx.P.Message_1_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then Universal.Message.Has_Buffer (Ctx.P.Message_2_Ctx)
      and then Ctx.P.Message_2_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Ctx.P.Message_2_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095);

   function Initialized (Ctx : Context'Class) return Boolean is
     (Global_Initialized (Ctx)
      and then Test.Session_Allocator.Global_Allocated (Ctx.P.Slots));

   function Active (Ctx : Context'Class) return Boolean is
     (Ctx.P.Next_State /= S_Final);

   function Next_State (Ctx : Context'Class) return State is
     (Ctx.P.Next_State);

   function Has_Data (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_Channel =>
             (case Ctx.P.Next_State is
                 when S_Send_1 =>
                    Universal.Message.Well_Formed_Message (Ctx.P.Message_1_Ctx)
                    and Universal.Message.Byte_Size (Ctx.P.Message_1_Ctx) > 0,
                 when S_Send_2 =>
                    Universal.Message.Well_Formed_Message (Ctx.P.Message_2_Ctx)
                    and Universal.Message.Byte_Size (Ctx.P.Message_2_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             (case Ctx.P.Next_State is
                 when S_Send_1 =>
                    Universal.Message.Byte_Size (Ctx.P.Message_1_Ctx),
                 when S_Send_2 =>
                    Universal.Message.Byte_Size (Ctx.P.Message_2_Ctx),
                 when others =>
                    RFLX_Types.Unreachable)));

end RFLX.Test.Session;