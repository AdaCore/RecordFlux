with RFLX.Pub_Sub;

package DB with
  SPARK_Mode, Abstract_State => Subscribers
is
   function Is_Subscribed (ID : RFLX.Pub_Sub.Identifier)
      return Boolean
         with Global => (Input => Subscribers);

   procedure Unsubscribe (ID : RFLX.Pub_Sub.Identifier) with
      Global => (In_Out => Subscribers),
      Pre    => Is_Subscribed (ID);

   procedure Subscribe (ID : RFLX.Pub_Sub.Identifier) with
      Global => (In_Out => Subscribers),
      Post => Is_Subscribed (ID);

   type Identifiers is array (Natural range <>)
      of RFLX.Pub_Sub.Identifier;

   function Current_Subscribers return Identifiers with
      Global => (Input => Subscribers);
end DB;
