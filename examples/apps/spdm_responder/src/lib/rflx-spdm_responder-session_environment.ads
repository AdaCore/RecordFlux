with System;

package RFLX.SPDM_Responder.Session_Environment with
   SPARK_Mode
is

   --  Implementation defined.
   --
   --  This record can be defined freely by the implementer to
   --  hold any value required by the platform. Memory management
   --  for this struct is also the responsibility of the
   --  implementer.
   type State is
      record
         Instance : System.Address;
      end record;

   --  Initialize State.
   --
   --  This procedure is implemented by the platform code. It must be called
   --  before the start of the state machine to ensure that State.Instance is
   --  initialized. If all components of State are initialized by default, this
   --  procedure can be removed.
   --
   --  @param State Session_Environment.State.
   procedure Plat_Initialize (State : out Session_Environment.State) with
      Always_Terminates;

end RFLX.SPDM_Responder.Session_Environment;
