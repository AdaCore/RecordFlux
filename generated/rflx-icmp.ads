pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

package RFLX.ICMP with
  SPARK_Mode
is

   type Tag_Base is mod 2**8;

   type Tag is (Echo_Reply, Destination_Unreachable, Source_Quench, Redirect, Echo_Request, Time_Exceeded, Parameter_Problem, Timestamp_Msg, Timestamp_Reply, Information_Request, Information_Reply) with
     Size =>
       8;
   for Tag use (Echo_Reply => 0, Destination_Unreachable => 3, Source_Quench => 4, Redirect => 5, Echo_Request => 8, Time_Exceeded => 11, Parameter_Problem => 12, Timestamp_Msg => 13, Timestamp_Reply => 14, Information_Request => 15, Information_Reply => 16);

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Tag return RFLX.ICMP.Tag is
     (RFLX.ICMP.Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Tag_Base) return Boolean is
     ((case Val is
          when 0 | 8 | 3 | 11 | 12 | 4 | 5 | 13 | 14 | 15 | 16 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.ICMP.Tag) return RFLX.ICMP.Tag_Base is
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

   function To_Actual (Val : RFLX.ICMP.Tag_Base) return RFLX.ICMP.Tag is
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
             Unreachable_ICMP_Tag))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Destination_Unreachable_Base is mod 2**8;

   type Code_Destination_Unreachable is (Net_Unreachable, Host_Unreachable, Protocol_Unreachable, Port_Unreachable, Fragmentation_Needed_DF_Set, Source_Route_Failed) with
     Size =>
       8;
   for Code_Destination_Unreachable use (Net_Unreachable => 0, Host_Unreachable => 1, Protocol_Unreachable => 2, Port_Unreachable => 3, Fragmentation_Needed_DF_Set => 4, Source_Route_Failed => 5);

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Code_Destination_Unreachable return RFLX.ICMP.Code_Destination_Unreachable is
     (RFLX.ICMP.Code_Destination_Unreachable'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Code_Destination_Unreachable_Base) return Boolean is
     ((case Val is
          when 0 | 1 | 2 | 3 | 4 | 5 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.ICMP.Code_Destination_Unreachable) return RFLX.ICMP.Code_Destination_Unreachable_Base is
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

   function To_Actual (Val : RFLX.ICMP.Code_Destination_Unreachable_Base) return RFLX.ICMP.Code_Destination_Unreachable is
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
             Unreachable_ICMP_Code_Destination_Unreachable))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Time_Exceeded_Base is mod 2**8;

   type Code_Time_Exceeded is (TTL_Exceeded, Fragment_Reassembly_Time_Exceeded) with
     Size =>
       8;
   for Code_Time_Exceeded use (TTL_Exceeded => 0, Fragment_Reassembly_Time_Exceeded => 1);

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Code_Time_Exceeded return RFLX.ICMP.Code_Time_Exceeded is
     (RFLX.ICMP.Code_Time_Exceeded'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Code_Time_Exceeded_Base) return Boolean is
     ((case Val is
          when 0 | 1 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.ICMP.Code_Time_Exceeded) return RFLX.ICMP.Code_Time_Exceeded_Base is
     ((case Enum is
          when TTL_Exceeded =>
             0,
          when Fragment_Reassembly_Time_Exceeded =>
             1));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.ICMP.Code_Time_Exceeded_Base) return RFLX.ICMP.Code_Time_Exceeded is
     ((case Val is
          when 0 =>
             TTL_Exceeded,
          when 1 =>
             Fragment_Reassembly_Time_Exceeded,
          when others =>
             Unreachable_ICMP_Code_Time_Exceeded))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Redirect_Base is mod 2**8;

   type Code_Redirect is (Redirect_for_Network, Redirect_for_Host, Redirect_for_Service_Network, Redirect_for_Service_Host) with
     Size =>
       8;
   for Code_Redirect use (Redirect_for_Network => 0, Redirect_for_Host => 1, Redirect_for_Service_Network => 2, Redirect_for_Service_Host => 3);

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Code_Redirect return RFLX.ICMP.Code_Redirect is
     (RFLX.ICMP.Code_Redirect'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Code_Redirect_Base) return Boolean is
     ((case Val is
          when 0 | 1 | 2 | 3 =>
             True,
          when others =>
             False));

   function To_Base (Enum : RFLX.ICMP.Code_Redirect) return RFLX.ICMP.Code_Redirect_Base is
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

   function To_Actual (Val : RFLX.ICMP.Code_Redirect_Base) return RFLX.ICMP.Code_Redirect is
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
             Unreachable_ICMP_Code_Redirect))
    with
     Pre =>
       Valid (Val);

   pragma Warnings (On, "unreachable branch");

   type Code_Zero_Base is mod 2**8 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Code_Zero is range 0 .. 0 with
     Size =>
       8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Code_Zero return RFLX.ICMP.Code_Zero is
     (RFLX.ICMP.Code_Zero'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Code_Zero_Base) return Boolean is
     (Val = 0);

   function To_Base (Val : RFLX.ICMP.Code_Zero) return RFLX.ICMP.Code_Zero_Base is
     (RFLX.ICMP.Code_Zero_Base (Val));

   function To_Actual (Val : RFLX.ICMP.Code_Zero_Base) return RFLX.ICMP.Code_Zero is
     (RFLX.ICMP.Code_Zero (Val))
    with
     Pre =>
       Valid (Val);

   type Checksum is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Checksum return RFLX.ICMP.Checksum is
     (RFLX.ICMP.Checksum'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.ICMP.Checksum) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.ICMP.Checksum) return RFLX.ICMP.Checksum is
     (Val);

   function To_Actual (Val : RFLX.ICMP.Checksum) return RFLX.ICMP.Checksum is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Identifier is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Identifier return RFLX.ICMP.Identifier is
     (RFLX.ICMP.Identifier'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.ICMP.Identifier) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.ICMP.Identifier) return RFLX.ICMP.Identifier is
     (Val);

   function To_Actual (Val : RFLX.ICMP.Identifier) return RFLX.ICMP.Identifier is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Sequence_Number is mod 2**16 with
     Size =>
       16;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Sequence_Number return RFLX.ICMP.Sequence_Number is
     (RFLX.ICMP.Sequence_Number'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.ICMP.Sequence_Number) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.ICMP.Sequence_Number) return RFLX.ICMP.Sequence_Number is
     (Val);

   function To_Actual (Val : RFLX.ICMP.Sequence_Number) return RFLX.ICMP.Sequence_Number is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Pointer is mod 2**8 with
     Size =>
       8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Pointer return RFLX.ICMP.Pointer is
     (RFLX.ICMP.Pointer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.ICMP.Pointer) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.ICMP.Pointer) return RFLX.ICMP.Pointer is
     (Val);

   function To_Actual (Val : RFLX.ICMP.Pointer) return RFLX.ICMP.Pointer is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Timestamp is mod 2**32 with
     Size =>
       32;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Timestamp return RFLX.ICMP.Timestamp is
     (RFLX.ICMP.Timestamp'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.ICMP.Timestamp) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.ICMP.Timestamp) return RFLX.ICMP.Timestamp is
     (Val);

   function To_Actual (Val : RFLX.ICMP.Timestamp) return RFLX.ICMP.Timestamp is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Gateway_Internet_Address is mod 2**32 with
     Size =>
       32;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Gateway_Internet_Address return RFLX.ICMP.Gateway_Internet_Address is
     (RFLX.ICMP.Gateway_Internet_Address'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   pragma Warnings (Off, "unused variable ""Val""");

   pragma Warnings (Off, "formal parameter ""Val"" is not referenced");

   function Valid (Val : RFLX.ICMP.Gateway_Internet_Address) return Boolean is
     (True);

   pragma Warnings (On, "formal parameter ""Val"" is not referenced");

   pragma Warnings (On, "unused variable ""Val""");

   function To_Base (Val : RFLX.ICMP.Gateway_Internet_Address) return RFLX.ICMP.Gateway_Internet_Address is
     (Val);

   function To_Actual (Val : RFLX.ICMP.Gateway_Internet_Address) return RFLX.ICMP.Gateway_Internet_Address is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Unused_32_Base is mod 2**32 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Unused_32 is range 0 .. 0 with
     Size =>
       32;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Unused_32 return RFLX.ICMP.Unused_32 is
     (RFLX.ICMP.Unused_32'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Unused_32_Base) return Boolean is
     (Val = 0);

   function To_Base (Val : RFLX.ICMP.Unused_32) return RFLX.ICMP.Unused_32_Base is
     (RFLX.ICMP.Unused_32_Base (Val));

   function To_Actual (Val : RFLX.ICMP.Unused_32_Base) return RFLX.ICMP.Unused_32 is
     (RFLX.ICMP.Unused_32 (Val))
    with
     Pre =>
       Valid (Val);

   type Unused_24_Base is mod 2**24 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Unused_24 is range 0 .. 0 with
     Size =>
       24;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_ICMP_Unused_24 return RFLX.ICMP.Unused_24 is
     (RFLX.ICMP.Unused_24'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.ICMP.Unused_24_Base) return Boolean is
     (Val = 0);

   function To_Base (Val : RFLX.ICMP.Unused_24) return RFLX.ICMP.Unused_24_Base is
     (RFLX.ICMP.Unused_24_Base (Val));

   function To_Actual (Val : RFLX.ICMP.Unused_24_Base) return RFLX.ICMP.Unused_24 is
     (RFLX.ICMP.Unused_24 (Val))
    with
     Pre =>
       Valid (Val);

end RFLX.ICMP;
