private with Interfaces.C;
with RFLX.IPv4;

generic
   type Element_Type is (<>);
   type Index_Type is (<>);
   type Buffer_Type is array (Index_Type range <>) of Element_Type;
   type IP4_Address is mod <>;
package Generic_Socket with
   SPARK_Mode,
   Abstract_State => Network,
   Initializes    => Network
is

   procedure Setup with
      Global => (In_Out => Network);

   function Valid return Boolean with
      Global => (Input => Network);

   procedure Receive (Buffer  : out Buffer_Type;
                      Last    : out Index_Type;
                      Success : out Boolean) with
      Global => (Input => Network),
      Pre    => Valid and Buffer'Length > 0,
      Post   => Valid and then (if Success then Last <= Buffer'Last);

   procedure Send (Buffer      :     Buffer_Type;
                   Destination :     IP4_Address;
                   Success     : out Boolean) with
      Global => (Input => Network),
      Pre    => Valid,
      Post   => Valid;

end Generic_Socket;
