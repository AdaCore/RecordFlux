pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.ICMP with
  SPARK_Mode
is

   type Tag is (Echo_Reply, Destination_Unreachable, Source_Quench, Redirect, Echo_Request, Time_Exceeded, Parameter_Problem, Timestamp_Msg, Timestamp_Reply, Information_Request, Information_Reply) with
     Size =>
       8;
   for Tag use (Echo_Reply => 0, Destination_Unreachable => 3, Source_Quench => 4, Redirect => 5, Echo_Request => 8, Time_Exceeded => 11, Parameter_Problem => 12, Timestamp_Msg => 13, Timestamp_Reply => 14, Information_Request => 15, Information_Reply => 16);

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Tag (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 8 | 3 | 11 | 12 | 4 | 5 | 13 | 14 | 15 | 16);

   function To_Base_Integer (Enum : RFLX.ICMP.Tag) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when Echo_Reply =>
             0,
          when Echo_Request =>
             8,
          when Destination_Unreachable =>
             3,
          when Time_Exceeded =>
             11,
          when Parameter_Problem =>
             12,
          when Source_Quench =>
             4,
          when Redirect =>
             5,
          when Timestamp_Msg =>
             13,
          when Timestamp_Reply =>
             14,
          when Information_Request =>
             15,
          when Information_Reply =>
             16));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Tag is
     ((case Val is
          when 0 =>
             Echo_Reply,
          when 8 =>
             Echo_Request,
          when 3 =>
             Destination_Unreachable,
          when 11 =>
             Time_Exceeded,
          when 12 =>
             Parameter_Problem,
          when 4 =>
             Source_Quench,
          when 5 =>
             Redirect,
          when 13 =>
             Timestamp_Msg,
          when 14 =>
             Timestamp_Reply,
          when 15 =>
             Information_Request,
          when 16 =>
             Information_Reply,
          when others =>
             RFLX.ICMP.Tag'Last))
    with
     Pre =>
       Valid_Tag (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Destination_Unreachable is (Net_Unreachable, Host_Unreachable, Protocol_Unreachable, Port_Unreachable, Fragmentation_Needed_DF_Set, Source_Route_Failed) with
     Size =>
       8;
   for Code_Destination_Unreachable use (Net_Unreachable => 0, Host_Unreachable => 1, Protocol_Unreachable => 2, Port_Unreachable => 3, Fragmentation_Needed_DF_Set => 4, Source_Route_Failed => 5);

   function Valid_Code_Destination_Unreachable (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 3 | 4 | 5);

   function To_Base_Integer (Enum : RFLX.ICMP.Code_Destination_Unreachable) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when Net_Unreachable =>
             0,
          when Host_Unreachable =>
             1,
          when Protocol_Unreachable =>
             2,
          when Port_Unreachable =>
             3,
          when Fragmentation_Needed_DF_Set =>
             4,
          when Source_Route_Failed =>
             5));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Code_Destination_Unreachable is
     ((case Val is
          when 0 =>
             Net_Unreachable,
          when 1 =>
             Host_Unreachable,
          when 2 =>
             Protocol_Unreachable,
          when 3 =>
             Port_Unreachable,
          when 4 =>
             Fragmentation_Needed_DF_Set,
          when 5 =>
             Source_Route_Failed,
          when others =>
             RFLX.ICMP.Code_Destination_Unreachable'Last))
    with
     Pre =>
       Valid_Code_Destination_Unreachable (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Time_Exceeded is (TTL_Exceeded, Fragment_Reassembly_Time_Exceeded) with
     Size =>
       8;
   for Code_Time_Exceeded use (TTL_Exceeded => 0, Fragment_Reassembly_Time_Exceeded => 1);

   function Valid_Code_Time_Exceeded (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1);

   function To_Base_Integer (Enum : RFLX.ICMP.Code_Time_Exceeded) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when TTL_Exceeded =>
             0,
          when Fragment_Reassembly_Time_Exceeded =>
             1));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Code_Time_Exceeded is
     ((case Val is
          when 0 =>
             TTL_Exceeded,
          when 1 =>
             Fragment_Reassembly_Time_Exceeded,
          when others =>
             RFLX.ICMP.Code_Time_Exceeded'Last))
    with
     Pre =>
       Valid_Code_Time_Exceeded (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Redirect is (Redirect_for_Network, Redirect_for_Host, Redirect_for_Service_Network, Redirect_for_Service_Host) with
     Size =>
       8;
   for Code_Redirect use (Redirect_for_Network => 0, Redirect_for_Host => 1, Redirect_for_Service_Network => 2, Redirect_for_Service_Host => 3);

   function Valid_Code_Redirect (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 3);

   function To_Base_Integer (Enum : RFLX.ICMP.Code_Redirect) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when Redirect_for_Network =>
             0,
          when Redirect_for_Host =>
             1,
          when Redirect_for_Service_Network =>
             2,
          when Redirect_for_Service_Host =>
             3));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Code_Redirect is
     ((case Val is
          when 0 =>
             Redirect_for_Network,
          when 1 =>
             Redirect_for_Host,
          when 2 =>
             Redirect_for_Service_Network,
          when 3 =>
             Redirect_for_Service_Host,
          when others =>
             RFLX.ICMP.Code_Redirect'Last))
    with
     Pre =>
       Valid_Code_Redirect (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Zero is range 0 .. 0 with
     Size =>
       8;

   function Valid_Code_Zero (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val = 0);

   function To_Base_Integer (Val : RFLX.ICMP.Code_Zero) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Code_Zero is
     (RFLX.ICMP.Code_Zero (Val))
    with
     Pre =>
       Valid_Code_Zero (Val);

   type Checksum is mod 2**16 with
     Size =>
       16;

   function Valid_Checksum (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 65535);

   function To_Base_Integer (Val : RFLX.ICMP.Checksum) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Checksum is
     (RFLX.ICMP.Checksum (Val))
    with
     Pre =>
       Valid_Checksum (Val);

   type Identifier is mod 2**16 with
     Size =>
       16;

   function Valid_Identifier (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 65535);

   function To_Base_Integer (Val : RFLX.ICMP.Identifier) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Identifier is
     (RFLX.ICMP.Identifier (Val))
    with
     Pre =>
       Valid_Identifier (Val);

   type Sequence_Number is mod 2**16 with
     Size =>
       16;

   function Valid_Sequence_Number (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 65535);

   function To_Base_Integer (Val : RFLX.ICMP.Sequence_Number) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Sequence_Number is
     (RFLX.ICMP.Sequence_Number (Val))
    with
     Pre =>
       Valid_Sequence_Number (Val);

   type Pointer is mod 2**8 with
     Size =>
       8;

   function Valid_Pointer (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 255);

   function To_Base_Integer (Val : RFLX.ICMP.Pointer) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Pointer is
     (RFLX.ICMP.Pointer (Val))
    with
     Pre =>
       Valid_Pointer (Val);

   type Timestamp is mod 2**32 with
     Size =>
       32;

   function Valid_Timestamp (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.ICMP.Timestamp) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Timestamp is
     (RFLX.ICMP.Timestamp (Val))
    with
     Pre =>
       Valid_Timestamp (Val);

   type Gateway_Internet_Address is mod 2**32 with
     Size =>
       32;

   function Valid_Gateway_Internet_Address (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.ICMP.Gateway_Internet_Address) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Gateway_Internet_Address is
     (RFLX.ICMP.Gateway_Internet_Address (Val))
    with
     Pre =>
       Valid_Gateway_Internet_Address (Val);

   type Unused_32 is range 0 .. 0 with
     Size =>
       32;

   function Valid_Unused_32 (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val = 0);

   function To_Base_Integer (Val : RFLX.ICMP.Unused_32) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Unused_32 is
     (RFLX.ICMP.Unused_32 (Val))
    with
     Pre =>
       Valid_Unused_32 (Val);

   type Unused_24 is range 0 .. 0 with
     Size =>
       24;

   function Valid_Unused_24 (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val = 0);

   function To_Base_Integer (Val : RFLX.ICMP.Unused_24) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.ICMP.Unused_24 is
     (RFLX.ICMP.Unused_24 (Val))
    with
     Pre =>
       Valid_Unused_24 (Val);

end RFLX.ICMP;
