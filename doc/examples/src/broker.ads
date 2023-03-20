with Socket;

package Broker
   with SPARK_Mode
is
   procedure Run with
      Pre => Socket.Initialized;
end Broker;
