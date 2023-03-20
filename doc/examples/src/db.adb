package body DB with
  SPARK_Mode, Refined_State => (Subscribers => (Subs))
is

   type Subscribed is array (RFLX.Pub_Sub.Identifier) of Boolean;
   Subs : Subscribed := (others => False);

   procedure Subscribe (ID : RFLX.Pub_Sub.Identifier) is
   begin
      Subs (ID) := True;
   end Subscribe;

   procedure Unsubscribe (ID : RFLX.Pub_Sub.Identifier) is
   begin
      Subs (ID) := False;
   end Unsubscribe;

   function Is_Subscribed (ID : RFLX.Pub_Sub.Identifier) return Boolean is
     (Subs (ID));

   function Num_Subscribers return Natural is
      Total : Natural := 0;
   begin
      for Subscriber in Subs'Range loop
         if Subs (Subscriber) and Total < Natural'Last then
            Total := Total + 1;
         end if;
      end loop;
      return Total;
   end Num_Subscribers;

   function Current_Subscribers return Identifiers is
      N     : constant Natural := Num_Subscribers;
      Index : Natural          := 1;
   begin
      return
        Result : Identifiers (1 .. N) := (others => RFLX.Pub_Sub.Identifier'First) do
         for Subscriber in Subs'Range loop
            pragma Loop_Invariant (Index >= 1);
            if Subs (Subscriber) and Index <= Result'Last and
              Index < Natural'Last
            then
               Result (Index) := Subscriber;
               Index          := Index + 1;
            end if;
         end loop;
      end return;
   end Current_Subscribers;

end DB;
