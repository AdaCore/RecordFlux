with RFLX.RFLX_Types;

package Socket
   with SPARK_Mode, Abstract_State => Network
is
   Initialized : Boolean := False with Ghost;

   procedure Initialize (Port : Natural)
      with Global => (In_Out => Initialized, Output => Network),
           Pre    => not Initialized,
           Post   => Initialized;

   procedure Send (Data : RFLX.RFLX_Types.Bytes)
      with Global => (Input => Initialized, In_Out => Network),
           Pre    => Initialized;

   procedure Receive (Data    : out RFLX.RFLX_Types.Bytes;
                      Success : out Boolean)
      with Global => (Input => Initialized, In_Out => Network),
           Pre    => Initialized;
end Socket;
