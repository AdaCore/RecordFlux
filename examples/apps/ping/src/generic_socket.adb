with Interfaces.C;

package body Generic_Socket with
   SPARK_Mode,
   Refined_State => (Network => (FD_In, FD_Out))
is

   package IC renames Interfaces.C;
   use type IC.int;
   use type IC.size_t;

   procedure C_Perror (Message : String) with
      Global        => null,
      Import,
      External_Name => "perror";

   FD_In  : IC.int := -1;
   FD_Out : IC.int := -1;

   procedure Setup
   is
      function C_Socket (Domain   : IC.int;
                         C_Type   : IC.int;
                         Protocol : IC.int) return IC.int with
         Global        => null,
         Import,
         Convention    => C,
         External_Name => "socket";
      PF_INET      : constant IC.int := 2;
      SOCK_RAW     : constant IC.int := 3;
      IPPROTO_ICMP : constant IC.int := 1;
      IPPROTO_RAW  : constant IC.int := 255;
   begin
      FD_In := C_Socket (PF_INET, SOCK_RAW, IPPROTO_ICMP);
      if FD_In < 0 then
         C_Perror ("C_Socket (PF_INET, SOCK_RAW, IPPROTO_ICMP)" & ASCII.NUL);
         return;
      end if;
      FD_Out := C_Socket (PF_INET, SOCK_RAW, IPPROTO_RAW);
      if FD_Out < 0 then
         C_Perror ("C_Socket (PF_Inet, SOCK_RAW, IPPROTO_RAW)" & ASCII.NUL);
         return;
      end if;
   end Setup;

   function Valid return Boolean is (FD_In > -1 and then FD_Out > -1) with
      Refined_Global => (Input => (FD_In, FD_Out));

   procedure Receive (Buffer  : out Buffer_Type;
                      Last    : out Index_Type;
                      Success : out Boolean)
   is
      function C_Recv (FD     : IC.int;
                       Buffer : Buffer_Type;
                       Length : IC.size_t;
                       Flags  : IC.int) return IC.int with
         Import,
         Convention    => C,
         External_Name => "recv",
         Global        => null,
         Pre           => Length <= IC.size_t (Buffer'Length),
         Post          => C_Recv'Result <= IC.int (Buffer'Length);
      Result : IC.int;
   begin
      Buffer := (others => Element_Type'First);
      Result := C_Recv (FD_In, Buffer, IC.size_t (Buffer'Length), 0);
      if Result > 0 then
         Success := True;
         Last    := Index_Type'Val (Index_Type'Pos (Buffer'First) + IC.int'Pos (Result) - 1);
      else
         Success := False;
         Last := Buffer'First;
         C_Perror ("Error receiving packet len:" & Buffer'Length'Img & ASCII.NUL);
      end if;
   end Receive;

   procedure Send (Buffer      :     Buffer_Type;
                   Destination :     IP4_Address;
                   Success     : out Boolean)
   is
      function C_Send (FD     : IC.int;
                       Dest   : IP4_Address;
                       Buffer : Buffer_Type;
                       Length : IC.size_t) return IC.int with
         Import,
         Convention    => C,
         External_Name => "c_send",
         Global        => null,
         Pre           => Length >= IC.size_t (Buffer'Length),
         Post          => C_Send'Result <= IC.int (Buffer'Length);
      Result : IC.int;
   begin
      if Buffer'Length < 1 then
         Success := True;
         return;
      end if;
      Result  := C_Send (FD_Out, Destination, Buffer, IC.size_t (Buffer'Length));
      Success := Result >= IC.int (Buffer'Length);
      if not Success then
         C_Perror ("Error sending packet len: " & Buffer'Length'Img & ASCII.NUL);
      end if;
   end Send;

end Generic_Socket;
