pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.Session_Allocator;
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Message;
with RFLX.Fixed_Size;
with RFLX.Fixed_Size.Simple_Message;

generic
   with procedure Get_Message_Type (Get_Message_Type : out RFLX.Universal.Option_Type);
   with procedure Create_Message (Create_Message : out RFLX.Fixed_Size.Simple_Message.Structure; Message_Type : RFLX.Universal.Option_Type; Data : RFLX_Types.Bytes);
   with procedure Valid_Message (Valid_Message : out Boolean; Message_Type : RFLX.Universal.Option_Type; Strict : Boolean);
package RFLX.Test.Session with
  SPARK_Mode,
  Initial_Condition =>
    Uninitialized
is

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Length;

   type Channel is (C_Channel);

   type State is (S_Start, S_Process, S_Reply, S_Terminated);

   function Uninitialized return Boolean;

   function Initialized return Boolean;

   function Active return Boolean;

   procedure Initialize with
     Pre =>
       Uninitialized,
     Post =>
       Initialized
       and Active;

   procedure Finalize with
     Pre =>
       Initialized,
     Post =>
       Uninitialized
       and not Active;

   pragma Warnings (Off, "subprogram ""Tick"" has no effect");

   procedure Tick with
     Pre =>
       Initialized,
     Post =>
       Initialized;

   pragma Warnings (On, "subprogram ""Tick"" has no effect");

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run with
     Pre =>
       Initialized,
     Post =>
       Initialized;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State return State;

   function Has_Data (Chan : Channel) return Boolean with
     Pre =>
       Initialized;

   function Read_Buffer_Size (Chan : Channel) return RFLX_Types.Length with
     Pre =>
       Initialized
       and then Has_Data (Chan);

   procedure Read (Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized
       and then Has_Data (Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Read_Buffer_Size (Chan),
     Post =>
       Initialized;

   function Needs_Data (Chan : Channel) return Boolean with
     Pre =>
       Initialized;

   function Write_Buffer_Size (Chan : Channel) return RFLX_Types.Length;

   procedure Write (Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized
       and then Needs_Data (Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Write_Buffer_Size (Chan),
     Post =>
       Initialized;

private

   P_Next_State : State := S_Start;

   Message_Ctx : Universal.Message.Context;

   Fixed_Size_Message_Ctx : Fixed_Size.Simple_Message.Context;

   function Uninitialized return Boolean is
     (not Universal.Message.Has_Buffer (Message_Ctx)
      and not Fixed_Size.Simple_Message.Has_Buffer (Fixed_Size_Message_Ctx));

   function Initialized return Boolean is
     (Universal.Message.Has_Buffer (Message_Ctx)
      and then Message_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Message_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then Fixed_Size.Simple_Message.Has_Buffer (Fixed_Size_Message_Ctx)
      and then Fixed_Size_Message_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Fixed_Size_Message_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then Test.Session_Allocator.Global_Allocated);

   function Active return Boolean is
     (P_Next_State /= S_Terminated);

   function Next_State return State is
     (P_Next_State);

end RFLX.Test.Session;
