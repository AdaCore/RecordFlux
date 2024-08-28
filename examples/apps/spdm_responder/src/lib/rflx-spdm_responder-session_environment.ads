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
         Instance : System.Address := System.Null_Address;
      end record;

end RFLX.SPDM_Responder.Session_Environment;
