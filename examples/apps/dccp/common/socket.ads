with RFLX.RFLX_Types;
private with GNAT.Sockets;

package Socket with
  SPARK_Mode
is
   type Channel is private with
     Annotate => (GNATprove, Ownership, "Needs_Reclamation");
   use type RFLX.RFLX_Types.Index;

   function Is_Open (Chan : Channel) return Boolean with
     Ghost, Annotate => (GNATprove, Ownership, "Needs_Reclamation");

   function Initialize
     (Port : Natural; Server : Boolean := False) return Channel with
     Post => Is_Open (Initialize'Result);

   procedure Receive
     (Chan :     Channel; Data : out RFLX.RFLX_Types.Bytes;
      Last : out RFLX.RFLX_Types.Index; Success : out Boolean) with
     Pre => Is_Open (Chan), Post => Data'First <= Last and Last <= Data'Last;

   procedure Send (Chan : Channel; Data : RFLX.RFLX_Types.Bytes) with
     Pre => Is_Open (Chan);

   procedure Close (Chan : in out Channel) with
     Pre     => Is_Open (Chan), Post => not Is_Open (Chan),
     Depends => (Chan => null, null => Chan);

private

   pragma SPARK_Mode (Off);

   type Channel is record
      Socket  : GNAT.Sockets.Socket_Type;
      Port    : GNAT.Sockets.Port_Type;
      Is_Open : Boolean;
   end record;

end Socket;
