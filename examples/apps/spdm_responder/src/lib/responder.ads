package Responder with
   SPARK_Mode,
   Abstract_State => Responder_State,
   Initializes => Responder_State,
   Initial_Condition => Uninitialized
is

   function Uninitialized return Boolean;

   procedure Main with
      Global => (In_Out => Responder_State);

end Responder;
