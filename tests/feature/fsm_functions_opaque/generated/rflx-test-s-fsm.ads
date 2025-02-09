------------------------------------------------------------------------------
--                                                                          --
--                         Generated by RecordFlux                          --
--                                                                          --
--                          Copyright (C) AdaCore                           --
--                                                                          --
--         SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception          --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Streams);
pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.S.FSM_Allocator;

package RFLX.Test.S.FSM
with
  SPARK_Mode
is

   type State is (S_Start, S_Check_Message, S_Check_Message_Sequence, S_Check_Scalar_Sequence, S_Error, S_Final);

   type Private_Context is private;

   type Context is limited
      record
         P : Private_Context;
         E : RFLX.Test.S_Environment.State;
      end record;

   function Uninitialized (Ctx : Context) return Boolean;

   function Global_Allocated (Ctx : Context) return Boolean;

   function Initialized (Ctx : Context) return Boolean;

   function Active (Ctx : Context) return Boolean;

   procedure Initialize (Ctx : in out Context)
   with
     Pre =>
       Uninitialized (Ctx),
     Post =>
       Initialized (Ctx)
       and Active (Ctx);

   procedure Finalize (Ctx : in out Context)
   with
     Pre =>
       Initialized (Ctx),
     Post =>
       Uninitialized (Ctx)
       and not Active (Ctx);

   pragma Warnings (Off, "subprogram ""Tick"" has no effect");

   procedure Tick (Ctx : in out Context)
   with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Tick"" has no effect");

   function In_IO_State (Unused_Ctx : Context) return Boolean;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run (Ctx : in out Context)
   with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State (Ctx : Context) return State;

private

   type Private_Context is
      record
         Next_State : State := S_Start;
         Slots : Test.S.FSM_Allocator.Slots;
         Memory : Test.S.FSM_Allocator.Memory;
      end record;

   function Uninitialized (Ctx : Context) return Boolean is
     (Test.S.FSM_Allocator.Uninitialized (Ctx.P.Slots));

   function Global_Allocated (Ctx : Context) return Boolean is
     (Test.S.FSM_Allocator.Global_Allocated (Ctx.P.Slots));

   function Initialized (Ctx : Context) return Boolean is
     (Global_Allocated (Ctx));

   function Active (Ctx : Context) return Boolean is
     (Ctx.P.Next_State /= S_Final);

   function Next_State (Ctx : Context) return State is
     (Ctx.P.Next_State);

end RFLX.Test.S.FSM;
