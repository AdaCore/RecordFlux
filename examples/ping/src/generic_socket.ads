private with Interfaces.C;

generic
   type Element_Type is (<>);
   type Index_Type is (<>);
   type Buffer_Type is array (Index_Type range <>) of Element_Type;
package Generic_Socket
  with Abstract_State => Network
is
   procedure Setup
   with
      Global => (Output => Network);

   function Valid return Boolean
   with
      Global => (Input => Network);

   procedure Receive (Buffer  : out Buffer_Type;
                      Last    : out Index_Type;
                      Success : out Boolean) with
      Global => (Input => Network),
      Pre  => Valid and Buffer'Length > 0,
      Post => Valid
              and then (if Success then Last <= Buffer'Last);

   procedure Send (Buffer  : Buffer_Type;
                   Success : out Boolean) with
      Global => (Input => Network),
      Pre => Valid,
      Post => Valid;

end Generic_Socket;
