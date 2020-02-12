pragma SPARK_Mode;
with RFLX.In_TLV.Generic_Contains;
with RFLX.Types;
with RFLX.TLV.Message;

package RFLX.In_TLV.Contains is new RFLX.In_TLV.Generic_Contains (RFLX.Types, RFLX.TLV.Message);
