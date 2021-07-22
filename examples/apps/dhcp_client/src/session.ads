pragma SPARK_Mode;

with RFLX.DHCP_Client.Session;

with Channel;

package Session is new RFLX.DHCP_Client.Session (Channel.Has_Data, Channel.Receive, Channel.Send);
