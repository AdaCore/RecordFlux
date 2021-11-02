pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.B.Session_Allocator;
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Message;

generic
package RFLX.B.Session with
  SPARK_Mode,
  Initial_Condition =>
    Uninitialized
is

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Length;

   type Channel is (C_Channel);

   type State is (S_Start, S_Terminated);

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

   M_Ctx : Universal.Message.Context;

   function Uninitialized return Boolean is
     (not Universal.Message.Has_Buffer (M_Ctx));

   function Initialized return Boolean is
     (Universal.Message.Has_Buffer (M_Ctx)
      and then M_Ctx.Buffer_First = RFLX_Types.Index'First
      and then M_Ctx.Buffer_Last = RFLX_Types.Index'First + 2047
      and then B.Session_Allocator.Global_Allocated);

   function Active return Boolean is
     (P_Next_State /= S_Terminated);

   function Next_State return State is
     (P_Next_State);

end RFLX.B.Session;
