pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.Session_Allocator;
with RFLX.RFLX_Types;
with RFLX.Universal;

generic
   with function Channel_Has_Data return Boolean;
   with procedure Channel_Read (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length);
   with procedure Channel_Write (Buffer : RFLX_Types.Bytes);
package RFLX.Test.Session with
  SPARK_Mode,
  Initial_Condition =>
    Uninitialized
is

   pragma Unreferenced (Channel_Has_Data);

   type State is (S_Start, S_Reply, S_Terminated);

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
       Uninitialized,
     Post =>
       Uninitialized;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State return State;

private

   P_Next_State : State := S_Start;

   Global : Universal.Value := 11;

   function Uninitialized return Boolean is
     (True);

   function Initialized return Boolean is
     (Test.Session_Allocator.Global_Allocated);

   function Active return Boolean is
     (P_Next_State /= S_Terminated);

   function Next_State return State is
     (P_Next_State);

end RFLX.Test.Session;
