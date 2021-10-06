pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Message;
with RFLX.Fixed_Size;
with RFLX.Fixed_Size.Simple_Message;

generic
   with function Channel_Has_Data return Boolean;
   with procedure Channel_Read (Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length);
   with procedure Channel_Write (Buffer : RFLX_Types.Bytes);
   with procedure Get_Message_Type (Get_Message_Type : out RFLX.Universal.Option_Type);
   with procedure Create_Message (Create_Message : out RFLX.Fixed_Size.Simple_Message.Structure; Message_Type : RFLX.Universal.Option_Type; Data : RFLX_Types.Bytes);
package RFLX.Test.Session with
  SPARK_Mode,
  Initial_Condition =>
    Uninitialized
is

   pragma Unreferenced (Channel_Has_Data);

   type Session_State is (S_Start, S_Reply, S_Terminated);

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

   function State return Session_State;

private

   use type RFLX.RFLX_Types.Index;

   Next_State : Session_State := S_Start;

   Message_Ctx : Universal.Message.Context;

   function Uninitialized return Boolean is
     (not Universal.Message.Has_Buffer (Message_Ctx));

   function Initialized return Boolean is
     (Universal.Message.Has_Buffer (Message_Ctx)
      and then Message_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Message_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095);

   function Active return Boolean is
     (Next_State /= S_Terminated);

   function State return Session_State is
     (Next_State);

end RFLX.Test.Session;
