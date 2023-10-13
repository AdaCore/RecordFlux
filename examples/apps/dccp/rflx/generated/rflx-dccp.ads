pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types;

package RFLX.DCCP with
  SPARK_Mode
is

   type Port_Type is range 0 .. 2**16 - 1 with
     Size =>
       16;

   use type RFLX.RFLX_Types.Base_Integer;

   function Valid_Port_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 65535);

   function To_Base_Integer (Val : RFLX.DCCP.Port_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Port_Type is
     (RFLX.DCCP.Port_Type (Val))
    with
     Pre =>
       Valid_Port_Type (Val);

   type Data_Offset_Type is range 3 .. 2**8 - 1 with
     Size =>
       8;

   function Valid_Data_Offset_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val >= 3
      and Val <= 255);

   function To_Base_Integer (Val : RFLX.DCCP.Data_Offset_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Data_Offset_Type is
     (RFLX.DCCP.Data_Offset_Type (Val))
    with
     Pre =>
       Valid_Data_Offset_Type (Val);

   type Checksum_Type is range 0 .. 2**16 - 1 with
     Size =>
       16;

   function Valid_Checksum_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 65535);

   function To_Base_Integer (Val : RFLX.DCCP.Checksum_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Checksum_Type is
     (RFLX.DCCP.Checksum_Type (Val))
    with
     Pre =>
       Valid_Checksum_Type (Val);

   type CCVal_Type is range 0 .. 2**4 - 1 with
     Size =>
       4;

   function Valid_CCVal_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 15);

   function To_Base_Integer (Val : RFLX.DCCP.CCVal_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.CCVal_Type is
     (RFLX.DCCP.CCVal_Type (Val))
    with
     Pre =>
       Valid_CCVal_Type (Val);

   type Checksum_Coverage_Type is range 0 .. 2**4 - 1 with
     Size =>
       4;

   function Valid_Checksum_Coverage_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 15);

   function To_Base_Integer (Val : RFLX.DCCP.Checksum_Coverage_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Checksum_Coverage_Type is
     (RFLX.DCCP.Checksum_Coverage_Type (Val))
    with
     Pre =>
       Valid_Checksum_Coverage_Type (Val);

   type Reserved_3_Type is range 0 .. 2**3 - 1 with
     Size =>
       3;

   function Valid_Reserved_3_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 7);

   function To_Base_Integer (Val : RFLX.DCCP.Reserved_3_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Reserved_3_Type is
     (RFLX.DCCP.Reserved_3_Type (Val))
    with
     Pre =>
       Valid_Reserved_3_Type (Val);

   type Type_Field is (DCCP_REQUEST, DCCP_RESPONSE, DCCP_DATA, DCCP_ACK, DCCP_DATA_ACK, DCCP_CLOSEREQ, DCCP_CLOSE, DCCP_RESET, DCCP_SYNC, DCCP_SYNCACK) with
     Size =>
       4;
   for Type_Field use (DCCP_REQUEST => 0, DCCP_RESPONSE => 1, DCCP_DATA => 2, DCCP_ACK => 3, DCCP_DATA_ACK => 4, DCCP_CLOSEREQ => 5, DCCP_CLOSE => 6, DCCP_RESET => 7, DCCP_SYNC => 8, DCCP_SYNCACK => 9);

   function Valid_Type_Field (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9);

   function To_Base_Integer (Enum : RFLX.DCCP.Type_Field) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when DCCP_REQUEST =>
             0,
          when DCCP_RESPONSE =>
             1,
          when DCCP_DATA =>
             2,
          when DCCP_ACK =>
             3,
          when DCCP_DATA_ACK =>
             4,
          when DCCP_CLOSEREQ =>
             5,
          when DCCP_CLOSE =>
             6,
          when DCCP_RESET =>
             7,
          when DCCP_SYNC =>
             8,
          when DCCP_SYNCACK =>
             9));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Type_Field is
     ((case Val is
          when 0 =>
             DCCP_REQUEST,
          when 1 =>
             DCCP_RESPONSE,
          when 2 =>
             DCCP_DATA,
          when 3 =>
             DCCP_ACK,
          when 4 =>
             DCCP_DATA_ACK,
          when 5 =>
             DCCP_CLOSEREQ,
          when 6 =>
             DCCP_CLOSE,
          when 7 =>
             DCCP_RESET,
          when 8 =>
             DCCP_SYNC,
          when 9 =>
             DCCP_SYNCACK,
          when others =>
             RFLX.DCCP.Type_Field'Last))
    with
     Pre =>
       Valid_Type_Field (Val);

   pragma Warnings (On, "unreachable branch");

   type CsCov_Type is range 0 .. 2**4 - 1 with
     Size =>
       4;

   function Valid_CsCov_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 15);

   function To_Base_Integer (Val : RFLX.DCCP.CsCov_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.CsCov_Type is
     (RFLX.DCCP.CsCov_Type (Val))
    with
     Pre =>
       Valid_CsCov_Type (Val);

   type Ext_Seq_Type is (NOT_EXTENDED, EXTENDED) with
     Size =>
       1;
   for Ext_Seq_Type use (NOT_EXTENDED => 0, EXTENDED => 1);

   function Valid_Ext_Seq_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 1 | 0);

   function To_Base_Integer (Enum : RFLX.DCCP.Ext_Seq_Type) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when EXTENDED =>
             1,
          when NOT_EXTENDED =>
             0));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Ext_Seq_Type is
     ((case Val is
          when 1 =>
             EXTENDED,
          when 0 =>
             NOT_EXTENDED,
          when others =>
             RFLX.DCCP.Ext_Seq_Type'Last))
    with
     Pre =>
       Valid_Ext_Seq_Type (Val);

   pragma Warnings (On, "unreachable branch");

   type Sequence_Number_Long_Type is range 0 .. 2**48 - 1 with
     Size =>
       48;

   function Valid_Sequence_Number_Long_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 281474976710655);

   function To_Base_Integer (Val : RFLX.DCCP.Sequence_Number_Long_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Sequence_Number_Long_Type is
     (RFLX.DCCP.Sequence_Number_Long_Type (Val))
    with
     Pre =>
       Valid_Sequence_Number_Long_Type (Val);

   type Sequence_Number_Short_Type is range 0 .. 2**24 - 1 with
     Size =>
       24;

   function Valid_Sequence_Number_Short_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 16777215);

   function To_Base_Integer (Val : RFLX.DCCP.Sequence_Number_Short_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Sequence_Number_Short_Type is
     (RFLX.DCCP.Sequence_Number_Short_Type (Val))
    with
     Pre =>
       Valid_Sequence_Number_Short_Type (Val);

   type Reserved_8_Type is range 0 .. 2**8 - 1 with
     Size =>
       8;

   function Valid_Reserved_8_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 255);

   function To_Base_Integer (Val : RFLX.DCCP.Reserved_8_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Reserved_8_Type is
     (RFLX.DCCP.Reserved_8_Type (Val))
    with
     Pre =>
       Valid_Reserved_8_Type (Val);

   type Reserved_16_Type is range 0 .. 2**16 - 1 with
     Size =>
       16;

   function Valid_Reserved_16_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 65535);

   function To_Base_Integer (Val : RFLX.DCCP.Reserved_16_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Reserved_16_Type is
     (RFLX.DCCP.Reserved_16_Type (Val))
    with
     Pre =>
       Valid_Reserved_16_Type (Val);

   type Ack_Number_Long_Type is range 0 .. 2**48 - 1 with
     Size =>
       48;

   function Valid_Ack_Number_Long_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 281474976710655);

   function To_Base_Integer (Val : RFLX.DCCP.Ack_Number_Long_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Ack_Number_Long_Type is
     (RFLX.DCCP.Ack_Number_Long_Type (Val))
    with
     Pre =>
       Valid_Ack_Number_Long_Type (Val);

   type Ack_Number_Short_Type is range 0 .. 2**24 - 1 with
     Size =>
       24;

   function Valid_Ack_Number_Short_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 16777215);

   function To_Base_Integer (Val : RFLX.DCCP.Ack_Number_Short_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Ack_Number_Short_Type is
     (RFLX.DCCP.Ack_Number_Short_Type (Val))
    with
     Pre =>
       Valid_Ack_Number_Short_Type (Val);

   type Service_Code_Type is range 0 .. 2**32 - 1 with
     Size =>
       32;

   function Valid_Service_Code_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.DCCP.Service_Code_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Service_Code_Type is
     (RFLX.DCCP.Service_Code_Type (Val))
    with
     Pre =>
       Valid_Service_Code_Type (Val);

   type Reset_Code_Type is (UNSPECIFIED, CLOSED, ABORTED, NO_CONNECTION, PACKET_ERROR, OPTION_ERROR, MANDATORY_ERROR, CONNECTION_REFUSED, BAD_SERVICE_CODE, TOO_BUSY, BAD_INIT_COOKIE, AGGRESSION_PENALTY) with
     Size =>
       8;
   for Reset_Code_Type use (UNSPECIFIED => 0, CLOSED => 1, ABORTED => 2, NO_CONNECTION => 3, PACKET_ERROR => 4, OPTION_ERROR => 5, MANDATORY_ERROR => 6, CONNECTION_REFUSED => 7, BAD_SERVICE_CODE => 8, TOO_BUSY => 9, BAD_INIT_COOKIE => 10, AGGRESSION_PENALTY => 11);

   function Valid_Reset_Code_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11);

   function To_Base_Integer (Enum : RFLX.DCCP.Reset_Code_Type) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when UNSPECIFIED =>
             0,
          when CLOSED =>
             1,
          when ABORTED =>
             2,
          when NO_CONNECTION =>
             3,
          when PACKET_ERROR =>
             4,
          when OPTION_ERROR =>
             5,
          when MANDATORY_ERROR =>
             6,
          when CONNECTION_REFUSED =>
             7,
          when BAD_SERVICE_CODE =>
             8,
          when TOO_BUSY =>
             9,
          when BAD_INIT_COOKIE =>
             10,
          when AGGRESSION_PENALTY =>
             11));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Reset_Code_Type is
     ((case Val is
          when 0 =>
             UNSPECIFIED,
          when 1 =>
             CLOSED,
          when 2 =>
             ABORTED,
          when 3 =>
             NO_CONNECTION,
          when 4 =>
             PACKET_ERROR,
          when 5 =>
             OPTION_ERROR,
          when 6 =>
             MANDATORY_ERROR,
          when 7 =>
             CONNECTION_REFUSED,
          when 8 =>
             BAD_SERVICE_CODE,
          when 9 =>
             TOO_BUSY,
          when 10 =>
             BAD_INIT_COOKIE,
          when 11 =>
             AGGRESSION_PENALTY,
          when others =>
             RFLX.DCCP.Reset_Code_Type'Last))
    with
     Pre =>
       Valid_Reset_Code_Type (Val);

   pragma Warnings (On, "unreachable branch");

   type Data_Type is range 0 .. 2**8 - 1 with
     Size =>
       8;

   function Valid_Data_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 255);

   function To_Base_Integer (Val : RFLX.DCCP.Data_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Data_Type is
     (RFLX.DCCP.Data_Type (Val))
    with
     Pre =>
       Valid_Data_Type (Val);

   type Opt_Type is (PADDING, MANDATORY, SLOW_RECEIVER, CHANGE_L, CONFIRM_L, CHANGE_R, CONFIRM_R, INIT_COOKIE, NDP_COUNT, ACK_VECTOR_0, ACK_VECTOR_1, DATA_DROPPED, TIMESTAMP, TIMESTAMP_ECHO, ELAPSED_TIME, DATA_CHECKSUM, CCID3_LOSS_EVT_RATE, CCID3_RCV_RATE) with
     Size =>
       8;
   for Opt_Type use (PADDING => 0, MANDATORY => 1, SLOW_RECEIVER => 2, CHANGE_L => 32, CONFIRM_L => 33, CHANGE_R => 34, CONFIRM_R => 35, INIT_COOKIE => 36, NDP_COUNT => 37, ACK_VECTOR_0 => 38, ACK_VECTOR_1 => 39, DATA_DROPPED => 40, TIMESTAMP => 41, TIMESTAMP_ECHO => 42, ELAPSED_TIME => 43, DATA_CHECKSUM => 44, CCID3_LOSS_EVT_RATE => 192, CCID3_RCV_RATE => 194);

   function Valid_Opt_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 192 | 194);

   function To_Base_Integer (Enum : RFLX.DCCP.Opt_Type) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when PADDING =>
             0,
          when MANDATORY =>
             1,
          when SLOW_RECEIVER =>
             2,
          when CHANGE_L =>
             32,
          when CONFIRM_L =>
             33,
          when CHANGE_R =>
             34,
          when CONFIRM_R =>
             35,
          when INIT_COOKIE =>
             36,
          when NDP_COUNT =>
             37,
          when ACK_VECTOR_0 =>
             38,
          when ACK_VECTOR_1 =>
             39,
          when DATA_DROPPED =>
             40,
          when TIMESTAMP =>
             41,
          when TIMESTAMP_ECHO =>
             42,
          when ELAPSED_TIME =>
             43,
          when DATA_CHECKSUM =>
             44,
          when CCID3_LOSS_EVT_RATE =>
             192,
          when CCID3_RCV_RATE =>
             194));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Opt_Type is
     ((case Val is
          when 0 =>
             PADDING,
          when 1 =>
             MANDATORY,
          when 2 =>
             SLOW_RECEIVER,
          when 32 =>
             CHANGE_L,
          when 33 =>
             CONFIRM_L,
          when 34 =>
             CHANGE_R,
          when 35 =>
             CONFIRM_R,
          when 36 =>
             INIT_COOKIE,
          when 37 =>
             NDP_COUNT,
          when 38 =>
             ACK_VECTOR_0,
          when 39 =>
             ACK_VECTOR_1,
          when 40 =>
             DATA_DROPPED,
          when 41 =>
             TIMESTAMP,
          when 42 =>
             TIMESTAMP_ECHO,
          when 43 =>
             ELAPSED_TIME,
          when 44 =>
             DATA_CHECKSUM,
          when 192 =>
             CCID3_LOSS_EVT_RATE,
          when 194 =>
             CCID3_RCV_RATE,
          when others =>
             RFLX.DCCP.Opt_Type'Last))
    with
     Pre =>
       Valid_Opt_Type (Val);

   pragma Warnings (On, "unreachable branch");

   type Option_Length_Type is range 0 .. 2**8 - 1 with
     Size =>
       8;

   function Valid_Option_Length_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 255);

   function To_Base_Integer (Val : RFLX.DCCP.Option_Length_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Option_Length_Type is
     (RFLX.DCCP.Option_Length_Type (Val))
    with
     Pre =>
       Valid_Option_Length_Type (Val);

   type Option_Feature_Type is (FEATURE_RESERVED, CCID, ALLOW_SHORT_SEQNOS, SEQUENCE_WINDOW, ECN_INCAPABLE, ACK_RATIO, SEND_ACK_VECTOR, SEND_NDP_COUNT, MINIMUM_CHECKSUM_COVERAGE, CHECK_DATA_CHECKSUM) with
     Size =>
       8;
   for Option_Feature_Type use (FEATURE_RESERVED => 0, CCID => 1, ALLOW_SHORT_SEQNOS => 2, SEQUENCE_WINDOW => 3, ECN_INCAPABLE => 4, ACK_RATIO => 5, SEND_ACK_VECTOR => 6, SEND_NDP_COUNT => 7, MINIMUM_CHECKSUM_COVERAGE => 8, CHECK_DATA_CHECKSUM => 9);

   function Valid_Option_Feature_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val in 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9);

   function To_Base_Integer (Enum : RFLX.DCCP.Option_Feature_Type) return RFLX.RFLX_Types.Base_Integer is
     ((case Enum is
          when FEATURE_RESERVED =>
             0,
          when CCID =>
             1,
          when ALLOW_SHORT_SEQNOS =>
             2,
          when SEQUENCE_WINDOW =>
             3,
          when ECN_INCAPABLE =>
             4,
          when ACK_RATIO =>
             5,
          when SEND_ACK_VECTOR =>
             6,
          when SEND_NDP_COUNT =>
             7,
          when MINIMUM_CHECKSUM_COVERAGE =>
             8,
          when CHECK_DATA_CHECKSUM =>
             9));

   pragma Warnings (Off, "unreachable branch");

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Option_Feature_Type is
     ((case Val is
          when 0 =>
             FEATURE_RESERVED,
          when 1 =>
             CCID,
          when 2 =>
             ALLOW_SHORT_SEQNOS,
          when 3 =>
             SEQUENCE_WINDOW,
          when 4 =>
             ECN_INCAPABLE,
          when 5 =>
             ACK_RATIO,
          when 6 =>
             SEND_ACK_VECTOR,
          when 7 =>
             SEND_NDP_COUNT,
          when 8 =>
             MINIMUM_CHECKSUM_COVERAGE,
          when 9 =>
             CHECK_DATA_CHECKSUM,
          when others =>
             RFLX.DCCP.Option_Feature_Type'Last))
    with
     Pre =>
       Valid_Option_Feature_Type (Val);

   pragma Warnings (On, "unreachable branch");

   type Receive_Rate_Type is range 0 .. 2**32 - 1 with
     Size =>
       32;

   function Valid_Receive_Rate_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.DCCP.Receive_Rate_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Receive_Rate_Type is
     (RFLX.DCCP.Receive_Rate_Type (Val))
    with
     Pre =>
       Valid_Receive_Rate_Type (Val);

   type Loss_Rate_Type is range 0 .. 2**32 - 1 with
     Size =>
       32;

   function Valid_Loss_Rate_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.DCCP.Loss_Rate_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Loss_Rate_Type is
     (RFLX.DCCP.Loss_Rate_Type (Val))
    with
     Pre =>
       Valid_Loss_Rate_Type (Val);

   type Timestamp_Option_Type is range 0 .. 2**32 - 1 with
     Size =>
       32;

   function Valid_Timestamp_Option_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.DCCP.Timestamp_Option_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Timestamp_Option_Type is
     (RFLX.DCCP.Timestamp_Option_Type (Val))
    with
     Pre =>
       Valid_Timestamp_Option_Type (Val);

   type Timestamp_Echo_Option_Type is range 0 .. 2**32 - 1 with
     Size =>
       32;

   function Valid_Timestamp_Echo_Option_Type (Val : RFLX.RFLX_Types.Base_Integer) return Boolean is
     (Val <= 4294967295);

   function To_Base_Integer (Val : RFLX.DCCP.Timestamp_Echo_Option_Type) return RFLX.RFLX_Types.Base_Integer is
     (RFLX.RFLX_Types.Base_Integer (Val));

   function To_Actual (Val : RFLX.RFLX_Types.Base_Integer) return RFLX.DCCP.Timestamp_Echo_Option_Type is
     (RFLX.DCCP.Timestamp_Echo_Option_Type (Val))
    with
     Pre =>
       Valid_Timestamp_Echo_Option_Type (Val);

end RFLX.DCCP;
