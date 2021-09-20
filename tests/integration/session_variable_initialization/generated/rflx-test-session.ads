pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
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

   function Uninitialized return Boolean;

   function Initialized return Boolean;

   function Active return Boolean;

   procedure Initialize with
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
     Post =>
       Uninitialized;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

private

   type Session_State is (S_Start, S_Reply, S_Terminated);

   State : Session_State := S_Start;

   Global : Universal.Value := 11;

   function Uninitialized return Boolean is
     (True);

   function Initialized return Boolean is
     (True);

   function Active return Boolean is
     (State /= S_Terminated);

end RFLX.Test.Session;
