with Socket;
with Broker;

procedure Main is
begin
   Socket.Initialize (Port => 8_888);
   loop
      Broker.Run;
   end loop;
end Main;
